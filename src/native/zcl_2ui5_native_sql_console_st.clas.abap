class zcl_2ui5_native_sql_console_st definition
                                     public
                                     create protected.

  public section.

    interfaces: if_serializable_object.

    types t_self type ref to zcl_2ui5_native_sql_console_st.

    types: begin of t_page,
             app_width_limited type abap_bool,
           end of t_page.

    types: begin of t_sql_editor_pane,
             statement type string,
             fallback_max_rows type i,
             layout_size type string,
           end of t_sql_editor_pane.

    types: begin of t_history_element,
             id type z2ui5_nsql_c_hst-id,
             natural_id type z2ui5_nsql_c_hst-natural_id,
             created_at type string,
             sql_statement type z2ui5_nsql_c_hst-sql_statement,
             rows_no type z2ui5_nsql_c_hst-rows_no,
             selected type abap_bool,
             infostate type string,
             highlight type string,
           end of t_history_element,
           t_history_collection type standard table of zcl_2ui5_native_sql_console_st=>t_history_element with empty key
                                                                                                         with non-unique sorted key by_sel components selected
                                                                                                         with unique hashed key by_key components id,
           begin of t_history_pane,
             layout_size type string,
             last_item_successfully_added type z2ui5_nsql_c_hst-id,
             items type zcl_2ui5_native_sql_console_st=>t_history_collection,
           end of t_history_pane.

    types: begin of t_value_map,
             pc type string,
             ea type string,
           end of t_value_map,
           begin of t_column_config,
             label type string,
             property type string,
             type type string,
             unit type string,
             delimiter type abap_bool,
             unit_property type string,
             width type string,
             scale type i,
             text_align type string,
             display_unit type string,
             true_value type string,
             false_value type string,
             template type string,
             input_format type string,
             wrap type abap_bool,
             auto_scale type abap_bool,
             timezone type string,
             timezone_property type string,
             display_timezone type abap_bool,
             utc type abap_bool,
             value_map type zcl_2ui5_native_sql_console_st=>t_value_map,
           end of t_column_config,
           t_columns_config type standard table of zcl_2ui5_native_sql_console_st=>t_column_config with empty key,
           begin of t_results_pane,
             db_data type ref to data,
             filtered_db_data type ref to data,
             output_data type ref to data,
             layout_size type string,
             title type string,
             column_config type zcl_2ui5_native_sql_console_st=>t_columns_config,
             wide_filter_string type string,
             filters type z2ui5_cl_util=>ty_t_filter_multi,
           end of t_results_pane.

    class-methods instance
                    importing
                      i_serialized type zcl_2ui5_native_sql_console_st=>t_self optional
                    returning
                      value(r_val) type zcl_2ui5_native_sql_console_st=>t_self.

    data is_initialized type abap_bool.

    data event_awaiting_response type string.

    data page type zcl_2ui5_native_sql_console_st=>t_page.

    data sql_editor_pane type zcl_2ui5_native_sql_console_st=>t_sql_editor_pane.

    data history_pane type zcl_2ui5_native_sql_console_st=>t_history_pane.

    data results_pane type zcl_2ui5_native_sql_console_st=>t_results_pane.

  protected section.

    class-data an_instace type zcl_2ui5_native_sql_console_st=>t_self.

endclass.
class zcl_2ui5_native_sql_console_st implementation.

  method instance.

    if not ( zcl_2ui5_native_sql_console_st=>an_instace is bound ).

      if i_serialized is bound.

        zcl_2ui5_native_sql_console_st=>an_instace = i_serialized.

      else.

        zcl_2ui5_native_sql_console_st=>an_instace = new #( ).

      endif.

    endif.

    r_val = zcl_2ui5_native_sql_console_st=>an_instace.

  endmethod.

endclass.
