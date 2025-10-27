class ZCL_ZCDOC_SUB_PROJ_CHDO definition
  public
  create public .

public section.

  interfaces IF_CHDO_ENHANCEMENTS .

  class-data OBJECTCLASS type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDOBJECTCL read-only value 'ZCDOC_SUB_PROJ' ##NO_TEXT.

  class-methods WRITE
    importing
      !OBJECTID type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDOBJECTV
      !UTIME type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDUZEIT
      !UDATE type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDDATUM
      !USERNAME type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDUSERNAME
      !PLANNED_CHANGE_NUMBER type IF_CHDO_OBJECT_TOOLS_REL=>TY_PLANCHNGNR default SPACE
      !OBJECT_CHANGE_INDICATOR type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDCHNGINDH default 'U'
      !PLANNED_OR_REAL_CHANGES type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDFLAG default SPACE
      !NO_CHANGE_POINTERS type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDFLAG default SPACE
      !O_ZTB_SUB_PROJ type ZTB_SUB_PROJ optional
      !N_ZTB_SUB_PROJ type ZTB_SUB_PROJ optional
      !UPD_ZTB_SUB_PROJ type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDCHNGINDH default SPACE
    exporting
      value(CHANGENUMBER) type IF_CHDO_OBJECT_TOOLS_REL=>TY_CDCHANGENR
    raising
      CX_CHDO_WRITE_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZCDOC_SUB_PROJ_CHDO IMPLEMENTATION.


  method WRITE.
*"----------------------------------------------------------------------
*"         this WRITE method is generated for object ZCDOC_SUB_PROJ
*"         never change it manually, please!        :07/07/2025
*"         All changes will be overwritten without a warning!
*"
*"         CX_CHDO_WRITE_ERROR is used for error handling
*"----------------------------------------------------------------------

    DATA: l_upd        TYPE if_chdo_object_tools_rel=>ty_cdchngind.

    CALL METHOD cl_chdo_write_tools=>changedocument_open
      EXPORTING
        objectclass             = objectclass
        objectid                = objectid
        planned_change_number   = planned_change_number
        planned_or_real_changes = planned_or_real_changes.

     IF ( N_ZTB_SUB_PROJ IS INITIAL ) AND
        ( O_ZTB_SUB_PROJ IS INITIAL ).
       l_upd  = space.
     ELSE.
       l_upd = UPD_ZTB_SUB_PROJ.
     ENDIF.

     IF  l_upd  NE space.
       CALL METHOD CL_CHDO_WRITE_TOOLS=>changedocument_single_case
         EXPORTING
           tablename              = 'ZTB_SUB_PROJ'
           workarea_old           = O_ZTB_SUB_PROJ
           workarea_new           = N_ZTB_SUB_PROJ
           change_indicator       = UPD_ZTB_SUB_PROJ
           docu_delete            = ''
           docu_insert            = ''
           docu_delete_if         = ''
           docu_insert_if         = ''
                  .
     ENDIF.

    CALL METHOD cl_chdo_write_tools=>changedocument_close
      EXPORTING
        objectclass             = objectclass
        objectid                = objectid
        date_of_change          = udate
        time_of_change          = utime
        username                = username
        object_change_indicator = object_change_indicator
        no_change_pointers      = no_change_pointers
      IMPORTING
        changenumber            = changenumber.

  endmethod.
ENDCLASS.
