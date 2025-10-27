CLASS zcl_get_change_docs_sub_proj DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES if_oo_adt_classrun.

    TYPES t_change_documents TYPE STANDARD TABLE OF zc_sub_proj_log WITH EMPTY KEY.

    DATA change_documents TYPE t_change_documents READ-ONLY.

    METHODS get_change_documents
      IMPORTING it_objectid            TYPE cl_chdo_read_tools=>tt_r_objectid
      RETURNING VALUE(changedocuments) TYPE t_change_documents.
ENDCLASS.



CLASS ZCL_GET_CHANGE_DOCS_SUB_PROJ IMPLEMENTATION.


  METHOD get_change_documents.
    DATA rt_cdredadd TYPE cl_chdo_read_tools=>tt_cdredadd_tab.
    DATA lr_err      TYPE REF TO cx_chdo_read_error.

    TRY.
        cl_chdo_read_tools=>changedocument_read( EXPORTING i_objectclass   = 'ZCDOC_SUB_PROJ'
                                                           it_objectid     = it_objectid
                                                 IMPORTING et_cdredadd_tab = rt_cdredadd ).

        MOVE-CORRESPONDING rt_cdredadd TO changedocuments.

      CATCH cx_chdo_read_error INTO lr_err.
    ENDTRY.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.


  METHOD if_rap_query_provider~select.
    DATA business_data TYPE STANDARD TABLE OF zc_sub_proj_log WITH EMPTY KEY.
    DATA lt_page       TYPE STANDARD TABLE OF zc_sub_proj_log WITH EMPTY KEY.

    DATA(top)  = io_request->get_paging( )->get_page_size( ).
    DATA(skip) = io_request->get_paging( )->get_offset( ).
    DATA(requested_fields) = io_request->get_requested_elements( ).

    DATA lt_objectid TYPE cl_chdo_read_tools=>tt_r_objectid.
    DATA ls_objectid TYPE cl_chdo_read_tools=>ty_r_objectid_line.

    CLEAR: business_data,
           lt_page.

    TRY.

        DATA(filter_condition) = io_request->get_filter( )->get_as_sql_string( ).
        DATA(filter_condition_ranges) = io_request->get_filter( )->get_as_ranges( ).

        READ TABLE filter_condition_ranges WITH KEY name = 'OBJECTID'
             INTO DATA(filter_condition_objectid).

        LOOP AT filter_condition_objectid-range INTO DATA(low_value).
          ls_objectid-low    = low_value-low.
          ls_objectid-option = 'EQ'.
          ls_objectid-sign   = 'I'.
          APPEND ls_objectid TO lt_objectid.
        ENDLOOP.

        business_data = get_change_documents( lt_objectid ).

        IF top < 0.
          top = 1.
        ENDIF.

        SELECT * FROM @business_data AS implementation_types
          WHERE (filter_condition)
          INTO TABLE @business_data.

        LOOP AT business_data ASSIGNING FIELD-SYMBOL(<business_data>).
          CONVERT DATE <business_data>-udate TIME <business_data>-utime
                INTO TIME STAMP <business_data>-utimestamp
                TIME ZONE cl_abap_context_info=>get_user_time_zone( CONV #( <business_data>-username ) ).

          SELECT SINGLE FullName
              FROM zi_usr_sub_proj_vh
              WITH PRIVILEGED ACCESS
              WHERE BusinessPartner = @<business_data>-username
              INTO @DATA(lv_name).

          IF lv_name IS NOT INITIAL.
            <business_data>-username = lv_name.
            CLEAR lv_name.
          ENDIF.
        ENDLOOP.

        " SORTING
        DATA(sort_order) = VALUE abap_sortorder_tab(
                                     FOR sort_element IN io_request->get_sort_elements( )
                                     ( name = sort_element-element_name descending = sort_element-descending ) ).
        IF sort_order IS INITIAL.
          sort_order = VALUE #( ( name = 'UTIMESTAMP' descending = abap_true ) ).
        ENDIF.

        SORT business_data BY (sort_order).

        IF lines( business_data ) > 1.
          LOOP AT business_data INTO DATA(ls_row) FROM skip + 1 TO skip + top.
            APPEND ls_row TO lt_page.
          ENDLOOP.
        ELSE.
          lt_page = business_data.
        ENDIF.

        io_response->set_data( lt_page ).

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( business_data ) ).
        ENDIF.

      CATCH cx_root INTO DATA(exception).
        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).
        DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.
        ASSERT 1 = 2.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
