class zcl_association_processor definition
                                public
                                create public.

  public section.

    interfaces: zif_association_processor.

    aliases: map_association_to_join for zif_association_processor~map_association_to_join.

    methods navigate_association
              importing
                i_data type if_data_preview=>ty_cds_assoc_nav_data optional
              returning
                value(r_val) type string
              raising
                cx_adt_rest.

  protected section.

endclass.
class zcl_association_processor implementation.

  METHOD navigate_association.

    DATA lo_query_handler      TYPE REF TO cl_adt_dp_open_sql_handler.
    DATA lx_dd_sobject_get     TYPE REF TO cx_dd_sobject_get.
    DATA lx_dd_ddl_check       TYPE REF TO cx_dd_ddl_check.
    DATA lo_dd_sobject_util    TYPE REF TO if_dd_sobject_util.
    DATA lo_cds_stmt_gen       TYPE REF TO cl_adt_dp_cds_assoc_osql_map.
    DATA lr_query_result       TYPE REF TO data.
    DATA ls_table_det          TYPE if_data_preview=>ty_data_preview_table_data.
    DATA ls_assoc_nav_data     TYPE if_data_preview=>ty_cds_assoc_nav_data.
    DATA ls_selected_row_data  TYPE if_data_preview=>ty_table_cell_content.
    DATA lt_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_values.
    DATA ls_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_value.
    DATA lt_field_details      TYPE ddentity_column_tab.
    DATA ls_field_detail       TYPE ddentity_column.
    DATA ls_sobjname           TYPE ddstrucobjname.
    DATA lt_sobjnames          TYPE if_dd_sobject_types=>ty_t_sobjnames.
    DATA lv_execution_time     TYPE string.
    DATA lv_view_name          TYPE viewname.
    DATA lv_tab_name           TYPE tabname.
    DATA lv_subrc              LIKE sy-subrc.
    DATA lv_error              TYPE symsgv.

    FIELD-SYMBOLS:  <fs_table_col>     TYPE if_data_preview=>ty_column.

    ls_assoc_nav_data = i_data.
*    ls_assoc_nav_data = value #( cds_entity_name = `demo_cds_assoc_scarr`
*                                 association_name = `_spfli`
*                                 association_target = `demo_cds_assoc_spfli`
*                                 selected_row_data = value #( ( name = 'CARRID'
*                                                                value = 'AA' ) ) ).

    TRANSLATE ls_assoc_nav_data-association_target TO UPPER CASE.
    TRANSLATE ls_assoc_nav_data-association_name   TO UPPER CASE.
    TRANSLATE ls_assoc_nav_data-cds_entity_name    TO UPPER CASE.

    LOOP AT ls_assoc_nav_data-selected_row_data INTO ls_selected_row_data.
      CLEAR ls_source_data.
      ls_source_data-component_name = ls_selected_row_data-name.
      ls_source_data-value = ls_selected_row_data-value.
      APPEND ls_source_data TO lt_source_data.
    ENDLOOP.

    TRY.

      CASE ls_assoc_nav_data-target_type.

        WHEN 'B'.

        WHEN 'T'.

          lv_tab_name = ls_assoc_nav_data-association_target.
          TRANSLATE lv_tab_name TO UPPER CASE.
          CALL FUNCTION 'DB_EXISTS_TABLE'
            EXPORTING
              tabname = lv_tab_name
            IMPORTING
              subrc   = lv_subrc.
          IF lv_subrc NE 0.
            lv_error =  lv_view_name.
            RAISE EXCEPTION TYPE cx_adt_datapreview_common
              EXPORTING
                textid = cx_adt_datapreview_common=>not_in_database
                msgv1  = lv_error.

          ENDIF.

        WHEN 'V'.

          lv_view_name = ls_assoc_nav_data-association_target.
          TRANSLATE lv_view_name TO UPPER CASE.
          CALL FUNCTION 'DB_EXISTS_VIEW'
            EXPORTING
              viewname = lv_view_name
            IMPORTING
              subrc    = lv_subrc.

          IF lv_subrc NE 0.
            lv_error =  lv_view_name.
            RAISE EXCEPTION TYPE cx_adt_datapreview_common
              EXPORTING
                textid = cx_adt_datapreview_common=>not_in_database
                msgv1  = lv_error.

          ENDIF.

        ENDCASE.

        TRY.

          CREATE OBJECT lo_cds_stmt_gen
            EXPORTING
              i_entity_name        = ls_assoc_nav_data-cds_entity_name
              i_association_name   = ls_assoc_nav_data-association_name
              i_target_data_source = ls_assoc_nav_data-association_target
              i_source_data        = lt_source_data
              i_param_values       = ls_assoc_nav_data-param_values.

          lo_cds_stmt_gen->get_statement(
*          lo_cds_stmt_gen->get_new_statement(
            IMPORTING
              e_statement           = r_val
           ).

        CATCH cx_dd_sobject INTO DATA(lx_dd_sobject).

          RAISE EXCEPTION TYPE cx_adt_datapreview_common EXPORTING previous = lx_dd_sobject.

        CATCH cx_dd_ddl_check INTO lx_dd_ddl_check.

          RAISE EXCEPTION TYPE cx_adt_datapreview_common EXPORTING previous = lx_dd_ddl_check.

        ENDTRY.

      CATCH cx_dd_sobject_get INTO lx_dd_sobject_get.

        RAISE EXCEPTION TYPE cx_adt_datapreview_common EXPORTING previous = lx_dd_sobject_get.

    ENDTRY.

  ENDMETHOD.
  METHOD zif_association_processor~map_association_to_join.

    constants statement_after_join type string value `(?i)\b(?:left|inner)\b.*` ##NO_TEXT.

    if i_association is not initial.

      try.

        cl_dd_sobject_factory=>create_util( )->get_associations( exporting entitynames = i_potential_data_sources
                                                                 importing associations = data(associations) ).

        data(association_name) = segment( val = i_association
                                          sep = `[`
                                          index = 1 ).

        data(assoc) = associations[ associationname = to_upper( association_name ) ].

        data(assoc_select) = me->navigate_association( value #( cds_entity_name = assoc-entityname
                                                                association_name = association_name
                                                                association_target = assoc-entityname_t ) ).

        r_val = match( val = assoc_select
                       pcre = statement_after_join ).

      catch cx_dd_sobject_get
            cx_adt_rest into data(e).

        raise exception type cx_sql_exception exporting sql_message = 'Error mapping the association'(001)
                                                        previous = e.

      endtry.

    endif.

  ENDMETHOD.

ENDCLASS.
