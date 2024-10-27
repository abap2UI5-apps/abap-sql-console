class zcl_2ui5_native_sql_console_st definition
                                     public
                                     create protected.

  public section.

    interfaces: if_serializable_object.

    types t_self type ref to zcl_2ui5_native_sql_console_st.

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
           begin of t_history_entry,
             id type z2ui5_nsql_c_hst-id,
             natural_id type z2ui5_nsql_c_hst-natural_id,
             created_at type string,
             sql_statement type z2ui5_nsql_c_hst-sql_statement,
             rows_no type z2ui5_nsql_c_hst-rows_no,
             selected type abap_bool,
             infostate type string,
             highlight type string,
           end of t_history_entry,
           t_history type standard table of zcl_2ui5_native_sql_console_st=>t_history_entry with empty key
                                                                                            with non-unique sorted key by_sel components selected
                                                                                            with unique hashed key by_key components id,
           begin of t_result_view,
             db_data type ref to data,
             filtered_db_data type ref to data,
             output_data type ref to data,
             cont_size type string,
             title type string,
             column_config type zcl_2ui5_native_sql_console_st=>t_columns_config,
             search_field type string,
             filters type z2ui5_cl_util=>ty_t_filter_multi,
             rtti_data type string,
             rtti_data_back type string,
           end of t_result_view,
           begin of t_main_view,
             sql_statement type string,
             sql_max_rows type i,
             sql_cont_size type string,
             history_cont_size type string,
             history_successfully_added type z2ui5_nsql_c_hst-id,
             history_tab type zcl_2ui5_native_sql_console_st=>t_history,
             app_width_limited type abap_bool,
             columns_config type zcl_2ui5_native_sql_console_st=>t_columns_config,
           end of t_main_view.

    class-methods instance
                    importing
                      i_serialized type zcl_2ui5_native_sql_console_st=>t_self optional
                    returning
                      value(r_val) type zcl_2ui5_native_sql_console_st=>t_self.

    data is_initialized type abap_bool.

    data is_popup_interaction type abap_bool.

    data popup_clear_history_accepted type string.

    data main_view type zcl_2ui5_native_sql_console_st=>t_main_view.

    data result_view type zcl_2ui5_native_sql_console_st=>t_result_view.

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
