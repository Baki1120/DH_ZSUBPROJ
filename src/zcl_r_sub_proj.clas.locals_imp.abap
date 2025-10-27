CLASS lsc_zr_sub_proj DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.

ENDCLASS.


CLASS lsc_zr_sub_proj IMPLEMENTATION.
  METHOD save_modified.
    DATA lt_create           TYPE TABLE OF ztb_sub_proj WITH EMPTY KEY.
    DATA lt_update           TYPE TABLE OF ztb_sub_proj WITH EMPTY KEY.
    DATA lt_delete           TYPE TABLE OF ztb_sub_proj WITH EMPTY KEY.
    DATA lt_aws              TYPE TABLE OF ztb_sub_proj WITH EMPTY KEY.
    DATA events_to_be_raised TYPE TABLE FOR EVENT zr_sub_proj~ApiCalled.

    lt_create = CORRESPONDING #( create-subproject MAPPING FROM ENTITY ).
    lt_update = CORRESPONDING #( update-subproject MAPPING FROM ENTITY ).
    lt_delete = CORRESPONDING #( delete-subproject MAPPING FROM ENTITY ).

    MOVE-CORRESPONDING lt_create TO lt_aws KEEPING TARGET LINES.
    MOVE-CORRESPONDING lt_update TO lt_aws KEEPING TARGET LINES.
    MOVE-CORRESPONDING lt_delete TO lt_aws KEEPING TARGET LINES.
    DELETE lt_aws WHERE uuid_api IS NOT INITIAL.

    IF lt_aws IS NOT INITIAL.
      TRY.
          DATA(bgpf_process_name) = zbgpfcl_exe_send_subproject=>run_via_bgpf_tx_uncontrolled(
                                        i_rap_bo_entity_key = lt_aws ).
        CATCH cx_bgmc.
      ENDTRY.
    ENDIF.

    " ---------------------------------------------------------------------
    DATA o_ztb_sub_proj TYPE ztb_sub_proj.
    DATA n_ztb_sub_proj TYPE ztb_sub_proj.
    DATA changenumber   TYPE if_chdo_object_tools_rel=>ty_cdchangenr.
    CONSTANTS cdoc_upd_object TYPE if_chdo_object_tools_rel=>ty_cdchngindh VALUE 'U'.
    DATA upd_ztb_sub_proj TYPE if_chdo_object_tools_rel=>ty_cdchngindh.

    LOOP AT update-subproject INTO DATA(ls_subproject).

      IF     ls_subproject-UuidApi          IS NOT INITIAL
         AND ls_subproject-%control-UuidApi  = if_abap_behv=>mk-on.

        CLEAR events_to_be_raised.
        APPEND INITIAL LINE TO events_to_be_raised.
        events_to_be_raised[ 1 ] = CORRESPONDING #( ls_subproject ).
        RAISE ENTITY EVENT zr_sub_proj~ApiCalled FROM events_to_be_raised.

      ENDIF.

      READ ENTITIES OF zr_sub_proj
           IN LOCAL MODE
           ENTITY SubProject
           ALL FIELDS WITH VALUE #( ( %key-Uuid = ls_subproject-Uuid ) )
           RESULT DATA(l_data).

      LOOP AT l_data ASSIGNING FIELD-SYMBOL(<f_data>).

        SELECT SINGLE *
          FROM ztb_sub_proj
          WITH
          PRIVILEGED ACCESS
          WHERE uuid = @<f_data>-Uuid
          INTO @DATA(ls_old_data).

        IF ls_subproject-%control-ProjectNumber = 01.
          o_ztb_sub_proj-project_number = ls_old_data-project_number.
          n_ztb_sub_proj-project_number = <f_data>-ProjectNumber.
        ENDIF.

        IF ls_subproject-%control-SubProjectNumber = 01.
          o_ztb_sub_proj-sub_project_number = ls_old_data-sub_project_number.
          n_ztb_sub_proj-sub_project_number = <f_data>-SubProjectNumber.
        ENDIF.

        IF ls_subproject-%control-SubProjectName = 01.
          o_ztb_sub_proj-sub_project_name = ls_old_data-sub_project_name.
          n_ztb_sub_proj-sub_project_name = <f_data>-SubProjectName.
        ENDIF.

        IF ls_subproject-%control-CampusCode = 01.
          o_ztb_sub_proj-campus_code = ls_old_data-campus_code.
          n_ztb_sub_proj-campus_code = <f_data>-CampusCode.
        ENDIF.

        IF ls_subproject-%control-active = 01.
          o_ztb_sub_proj-active = ls_old_data-active.
          n_ztb_sub_proj-active = <f_data>-active.
        ENDIF.

        upd_ztb_sub_proj = 'U'.
        n_ztb_sub_proj-uuid = <f_data>-Uuid.
        o_ztb_sub_proj-uuid = ls_old_data-uuid.
        n_ztb_sub_proj-uuid_api = <f_data>-UuidApi.
        o_ztb_sub_proj-uuid_api = ls_old_data-uuid_api.

        CONVERT UTCLONG utclong_current( )
                INTO DATE FINAL(datlo)
                TIME FINAL(timlo)
                TIME ZONE xco_cp_time=>time_zone->user->value.

        TRY.
            zcl_zcdoc_sub_proj_chdo=>write( EXPORTING objectid                = CONV #( <f_data>-Uuid )
                                                      utime                   = timlo
                                                      udate                   = datlo
                                                      username                = xco_cp=>sy->user( )->name
                                                      object_change_indicator = cdoc_upd_object
                                                      o_ztb_sub_proj          = o_ztb_sub_proj
                                                      n_ztb_sub_proj          = n_ztb_sub_proj
                                                      upd_ztb_sub_proj        = upd_ztb_sub_proj
                                            IMPORTING changenumber            = changenumber ).
          CATCH cx_chdo_write_error.
        ENDTRY.

        CLEAR: o_ztb_sub_proj,
               n_ztb_sub_proj,
               ls_old_data.
      ENDLOOP.

    ENDLOOP.

    IF update-subproject IS NOT INITIAL.
      UPDATE ztb_sub_proj FROM TABLE @update-subproject
      INDICATORS SET STRUCTURE %control MAPPING FROM ENTITY.
    ENDIF.

    IF delete-subproject IS NOT INITIAL.
      LOOP AT delete-subproject INTO DATA(fa_delete).
        DELETE FROM ztb_sub_proj WHERE uuid = @fa_delete-Uuid.
        DELETE FROM ztb_sub_proj_d WHERE uuid = @fa_delete-Uuid.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_zr_sub_proj DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR SubProject
      RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR SubProject RESULT result.

    METHODS setActive FOR DETERMINE ON MODIFY
      IMPORTING keys FOR SubProject~setActive.

    METHODS validateData FOR VALIDATE ON SAVE
      IMPORTING keys FOR SubProject~validateData.
*    METHODS triggerAPI FOR DETERMINE ON SAVE
*      IMPORTING keys FOR SubProject~triggerAPI.

ENDCLASS.


CLASS lhc_zr_sub_proj IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zr_sub_proj IN LOCAL MODE
         ENTITY SubProject
         ALL FIELDS WITH
         CORRESPONDING #( keys )
         RESULT FINAL(lt_data)
         FAILED failed.

    result = VALUE #( FOR ls_data IN lt_data
                      ( %tky                    = ls_data-%tky
                        %delete                 = if_abap_behv=>fc-o-disabled
                        %action-Edit            = if_abap_behv=>fc-o-enabled

*                        %field-Message          = if_abap_behv=>fc-f-read_only
*                        %field-Status           = if_abap_behv=>fc-f-read_only

                        %field-ProjectNumber    = COND #( WHEN ls_data-Active = abap_true
                                                          THEN if_abap_behv=>fc-f-mandatory
                                                          ELSE if_abap_behv=>fc-f-read_only )
                        %field-SubProjectNumber = COND #( WHEN ls_data-Active = abap_true
                                                          THEN if_abap_behv=>fc-f-mandatory
                                                          ELSE if_abap_behv=>fc-f-read_only )
                        %field-CampusCode       = COND #( WHEN ls_data-Active = abap_true
                                                          THEN if_abap_behv=>fc-f-mandatory
                                                          ELSE if_abap_behv=>fc-f-read_only )
                        %field-SubProjectName   = COND #( WHEN ls_data-Active = abap_true
                                                          THEN if_abap_behv=>fc-f-mandatory
                                                          ELSE if_abap_behv=>fc-f-read_only ) ) ).
  ENDMETHOD.

  METHOD setActive.
    READ ENTITIES OF zr_sub_proj IN LOCAL MODE
         ENTITY SubProject
         FIELDS ( Active )
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_data).

    " If Status is already set, do nothing
    DELETE lt_data WHERE Active IS NOT INITIAL.

    IF lt_data IS INITIAL.
      RETURN.
    ENDIF.

    MODIFY ENTITIES OF zr_sub_proj IN LOCAL MODE
           ENTITY SubProject
           UPDATE FIELDS ( Active )
           WITH VALUE #( FOR ls_data IN lt_data
                         ( %tky   = ls_data-%tky
                           Active = 'X' ) ).
  ENDMETHOD.

  METHOD validateData.
    DATA lt_data TYPE TABLE FOR READ RESULT zr_sub_proj\\SubProject.

    READ ENTITIES OF zr_sub_proj
         IN LOCAL MODE
         ENTITY SubProject
         FIELDS ( ProjectNumber SubProjectNumber )
         WITH CORRESPONDING #( keys )
         RESULT lt_data.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<f_data>).

      APPEND VALUE #( %tky        = <f_data>-%tky
                      %state_area = 'VALIDATE_DATA' ) TO reported-subproject.

      SELECT SINGLE COUNT( * ) FROM ztb_sub_proj
        WHERE project_number     = @<f_data>-ProjectNumber
          AND sub_project_number = @<f_data>-SubProjectNumber.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( %tky = <f_data>-%tky ) TO failed-subproject.

      APPEND VALUE #( %tky                      = <f_data>-%tky
                      %state_area               = 'VALIDATE_DATA'
                      %msg                      = new_message( id       = 'Z_SUB_PROJ_MSG'
                                                               number   = 001
                                                               v1       = <f_data>-ProjectNumber
                                                               v2       = <f_data>-SubProjectNumber
                                                               severity = if_abap_behv_message=>severity-error )
                      %element-ProjectNumber    = if_abap_behv=>mk-on
                      %element-SubProjectNumber = if_abap_behv=>mk-on ) TO reported-subproject.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
