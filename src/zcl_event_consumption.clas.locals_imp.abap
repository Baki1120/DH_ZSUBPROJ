CLASS lcl_abap_behv_event_handler DEFINITION INHERITING FROM cl_abap_behavior_event_handler.

  PRIVATE SECTION.
    METHODS confirm_data FOR ENTITY EVENT it_parameters FOR SubProject~send_data.

ENDCLASS.


CLASS lcl_abap_behv_event_handler IMPLEMENTATION.
  METHOD confirm_data.
    DATA(lv_msg) = it_parameters[ 1 ]-Message.
  ENDMETHOD.
ENDCLASS.
