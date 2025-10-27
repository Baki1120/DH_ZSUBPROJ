CLASS zbgpfcl_exe_send_subproject DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES if_bgmc_operation.
    INTERFACES if_bgmc_op_single_tx_uncontr.
    INTERFACES if_bgmc_op_single.

    TYPES tt_sub_proj TYPE STANDARD TABLE OF ztb_sub_proj.

    CLASS-METHODS run_via_bgpf_tx_uncontrolled
      IMPORTING i_rap_bo_entity_key             TYPE tt_sub_proj
                i_batch                         TYPE abap_boolean OPTIONAL
      RETURNING VALUE(r_process_monitor_string) TYPE string
      RAISING   cx_bgmc.

    METHODS constructor
      IMPORTING i_rap_bo_entity_key TYPE tt_sub_proj
                i_batch             TYPE abap_boolean OPTIONAL.

    CONSTANTS :
      BEGIN OF bgpf_state,
        unknown         TYPE int1 VALUE IS INITIAL,
        erroneous       TYPE int1 VALUE 1,
        new             TYPE int1 VALUE 2,
        running         TYPE int1 VALUE 3,
        successful      TYPE int1 VALUE 4,
        started_from_bo TYPE int1 VALUE 99,
      END OF bgpf_state.

  PRIVATE SECTION.
    DATA transaction_data TYPE TABLE OF ztb_sub_proj WITH EMPTY KEY.
    DATA batch            TYPE abap_boolean.

    CONSTANTS wait_time_in_seconds TYPE i VALUE 5.

ENDCLASS.



CLASS ZBGPFCL_EXE_SEND_SUBPROJECT IMPLEMENTATION.


  METHOD constructor.
    transaction_data = i_rap_bo_entity_key.
    batch = i_batch.
  ENDMETHOD.


  METHOD if_bgmc_op_single_tx_uncontr~execute.
    DATA lt_log      TYPE STANDARD TABLE OF ztb_sub_proj_log.
    DATA ls_data     TYPE zcl_check_connection_aws=>ts_data_subproject.
    DATA lv_msg      TYPE string.
    DATA ls_status   TYPE if_web_http_response=>http_status.

    " ---------------------------------------------------------------------
    DATA update      TYPE TABLE FOR UPDATE zr_sub_proj\\SubProject.
    DATA update_line TYPE STRUCTURE FOR UPDATE zr_sub_proj\\SubProject.

    WAIT UP TO wait_time_in_seconds SECONDS.
    " Get Time stamp
    GET TIME STAMP FIELD DATA(ts).

    READ ENTITIES OF zr_sub_proj
         ENTITY SubProject
         ALL FIELDS WITH
         VALUE #( FOR ls_trans IN transaction_data
                  ( %key-Uuid = ls_trans-uuid  ) )
         RESULT DATA(lt_data).

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<f_data>).
      ls_data-sap_id           = to_lower( xco_cp=>uuid( <f_data>-uuid )->as(
                                               io_format = xco_cp_uuid=>format->c36 )->value ).
      ls_data-campus           = <f_data>-CampusCode.
      ls_data-project_number   = <f_data>-ProjectNumber.
      ls_data-number           = <f_data>-SubProjectNumber.
      ls_data-name             = <f_data>-SubProjectName.
      ls_data-active_indicator = <f_data>-Active.

      IF batch = abap_true.
        CONTINUE.
      ENDIF.

      NEW zcl_check_connection_aws( )->aws_table_subproject( EXPORTING is_data   = ls_data
                                                             IMPORTING ev_msg    = lv_msg
                                                                       es_status = ls_status ).
*      DATA(lv_uuid) = xco_cp=>uuid( )->value.
      APPEND VALUE #( uuid                  = ls_data-sap_id
                      type                  = 'A'
                      project_number        = <f_data>-ProjectNumber
                      campus_code           = <f_data>-CampusCode
                      sub_project_number    = <f_data>-SubProjectNumber
                      sub_project_name      = <f_data>-SubProjectName
                      Active                = <f_data>-Active
                      Status                = SWITCH #( ls_status-code WHEN 200 OR 201 THEN 'S' ELSE 'F' )
                      Message               = condense( lv_msg )
                      created_by            = <f_data>-CreatedBy
                      created_at            = ts
                      local_last_changed_at = ts
                      local_last_changed_by = <f_data>-LocalLastChangedBy
                      last_changed_at       = ts ) TO lt_log.

      MOVE-CORRESPONDING <f_data> TO update_line.
      update_line-%is_draft = if_abap_behv=>mk-off.
      update_line-UuidApi   = ls_data-sap_id.
      update_line-%control-UuidApi = if_abap_behv=>mk-on.
      update_line-Message = condense( lv_msg ).
      update_line-Status  = SWITCH #( ls_status-code WHEN 200 OR 201 THEN 'S' ELSE 'F' ).
      APPEND update_line TO update.

      CLEAR: ls_data,
             lv_msg,
             ls_status.
    ENDLOOP.

    IF sy-subrc = 0.
      MODIFY ENTITIES OF zr_sub_proj
             ENTITY SubProject
             UPDATE FIELDS ( UuidApi Status Message )
             WITH update
             REPORTED DATA(reported_ready)
             FAILED DATA(failed_ready).
*    ELSE.
*      update_line-%control-Status = if_abap_behv=>mk-off.
*      update_line-UuidApi = ''.
*      update_line-Status = ''.
*      update_line-Message = ''.
*      APPEND update_line TO update.
*
*      MODIFY ENTITIES OF zr_sub_proj
*             ENTITY SubProject
*             UPDATE FIELDS ( UuidApi Status Message )
*             WITH update
*             REPORTED reported_ready
*             FAILED failed_ready.

    ENDIF.

    IF lt_log IS NOT INITIAL.
      MODIFY ztb_sub_proj_log FROM TABLE @lt_log.
    ENDIF.
  ENDMETHOD.


  METHOD if_bgmc_op_single~execute.
  ENDMETHOD.


  METHOD run_via_bgpf_tx_uncontrolled.
    TRY.
        DATA(process_monitor) = cl_bgmc_process_factory=>get_default( )->create(
                                              )->set_name( |Call API AWS to create Sub Project records|
                                              )->set_operation_tx_uncontrolled(
                                                  NEW zbgpfcl_exe_send_subproject(
                                                          i_rap_bo_entity_key = i_rap_bo_entity_key
                                                          i_batch             = i_batch )
                                              )->save_for_execution( ).

        r_process_monitor_string = process_monitor->to_string( ).

      CATCH cx_bgmc INTO DATA(lx_bgmc).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
