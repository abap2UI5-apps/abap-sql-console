*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class srtti_processor definition
                      create public ##CLASS_FINAL.

  public section.

    methods serialize
              importing
                i_data_object type any
              returning
                value(r_val) type xstring.

    methods deserialize
              importing
                i_serialized type xstring
              returning
                value(r_val) type ref to data.

endclass.
class history definition
             create public ##CLASS_FINAL.

  public section.

    types t_delete_entries type hashed table of z2ui5_nsql_c_hst-id with unique key table_line.

    types: begin of t_input_entry,
             natural_id type z2ui5_nsql_c_hst-natural_id,
             sql_statement type z2ui5_nsql_c_hst-sql_statement,
             results type ref to data,
           end of t_input_entry.

    types: begin of t_history_pane_item,
             id type z2ui5_nsql_c_hst-id,
             natural_id type z2ui5_nsql_c_hst-natural_id,
             created_at type z2ui5_nsql_c_hst-created_at,
             sql_statement type z2ui5_nsql_c_hst-sql_statement,
             rows_no type z2ui5_nsql_c_hst-rows_no,
           end of t_history_pane_item,
           t_history_pane_items type standard table of history=>t_history_pane_item with empty key.

    types: begin of t_results_entry,
             id type z2ui5_nsql_c_hst-id,
             data type ref to data,
           end of t_results_entry.

    methods get_sbar_data_for_current_user
              returning
                value(r_val) type history=>t_history_pane_items.

    methods get_sbar_data_for_user
              importing
                i_user type xubname
              returning
                value(r_val) type history=>t_history_pane_items.

    methods get_rslt_data_for_current_user
              importing
                i_id type z2ui5_nsql_c_hst-id
              returning
                value(r_val) type history=>t_results_entry.

    methods get_rslt_data_for_user
              importing
                i_id type z2ui5_nsql_c_hst-id
                i_user type xubname
              returning
                value(r_val) type history=>t_results_entry.

    methods insert_new
              importing
                i_entry type history=>t_input_entry
              returning
                value(r_val) type z2ui5_nsql_c_hst-id
              raising
                cx_uuid_error.

    methods delete_for_user
              importing
                i_entries type history=>t_delete_entries
                i_user type xubname
              returning
                value(r_val) type ref to history.

    methods delete_for_current_user
              importing
                i_entries type history=>t_delete_entries
              returning
                value(r_val) type ref to history.

endclass.
class main_view definition
                create public ##CLASS_FINAL.

  public section.

    methods constructor
              importing
                i_ui5_client type ref to z2ui5_if_client
                i_state type ref to zcl_2ui5_native_sql_console_st.

    methods set_for_display
              returning
                value(r_val) type ref to main_view.

  protected section.

    data a_ui5_client type ref to z2ui5_if_client.

    data a_parser type ref to z2ui5_cl_xml_view.

endclass.
class data_result_view definition
                       create public ##CLASS_FINAL.

  public section.

    methods constructor
              importing
                i_ui5_client type ref to z2ui5_if_client
                i_state type ref to zcl_2ui5_native_sql_console_st.

    methods set_for_display
              returning
                value(r_val) type ref to data_result_view.

    methods redraw
              returning
                value(r_val) type ref to data_result_view.

  protected section.

    data a_ui5_client type ref to z2ui5_if_client.

    data a_parser type ref to z2ui5_cl_xml_view.

endclass.
interface ui_interaction.

  methods handle
            importing
              i_ui5_client type ref to z2ui5_if_client
            returning
              value(r_val) type ref to ui_interaction
            raising
              cx_static_check.

  class-methods id
                  returning
                    value(r_Val) type string.

endinterface.
interface event.

  interfaces: ui_interaction.

  aliases: handle for ui_interaction~handle,
           event_name for ui_interaction~id.

endinterface.
class on_start definition
               create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_run definition
             create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_delete_history_items definition
                              create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_load_history_item definition
                           create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_select_all_history_items definition
                                  create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_deselect_all_history_items definition
                                    create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class on_wide_filtering definition
                        create public ##CLASS_FINAL.

  public section.

    interfaces: event.

    aliases: event_name for event~event_name.

endclass.
class user_approval definition
                    create public ##CLASS_FINAL.

  public section.

    methods constructor
              importing
                i_ui5_client type ref to z2ui5_if_client.

    methods is_confirmed
              returning
                value(r_val) type abap_bool.

  protected section.

    data a_confirmation_popup type ref to z2ui5_cl_pop_to_confirm.

endclass.
interface popup_dialog.

  interfaces: ui_interaction.

  aliases: handle for ui_interaction~handle,
           class_name for ui_interaction~id.

endinterface.
class error_acknowledged_interaction definition
                                     create public ##CLASS_FINAL.

  public section.

    interfaces: popup_dialog.

    aliases class_name for popup_dialog~class_name.

  protected section.

    data a_downcasted_ref type ref to z2ui5_cl_pop_error ##NEEDED. "as a safeguard that we are actually handling the intended popup

endclass.
class sql_statement definition
                    create public ##CLASS_FINAL.

  public section.

    types t_processor type ref to cl_sql_statement.

    types t_association_processor type ref to zif_association_processor.

    types: begin of t_result,
             updated_statement type string,
             data type ref to data,
             line_type type ref to cl_abap_structdescr,
             table_type type ref to cl_abap_tabledescr,
           end of t_result.

    methods constructor
              importing
                i_processor type sql_statement=>t_processor
                i_association_processor type sql_statement=>t_association_processor
                i_sql_statement type string
              raising
                cx_sql_exception.

    methods add_limit_if_none
              importing
                i_fallback_limit type i optional
              returning
                value(r_val) type ref to sql_statement.

    methods execute
              returning
                value(r_val) type sql_statement=>t_result
              raising
                cx_sql_exception.

    "! <p class="shorttext synchronized" lang="EN">Returns the unique path expression, when found</p>
    "! @parameter r_val | The association or empty
    "! @raising cx_sql_exception | Different associations or a complex path expression found
    methods sole_associaton_if_any
              returning
                value(r_val) type string
              raising
                cx_sql_exception.

    methods data_sources
              returning
                value(r_val) type string_table.

    methods generated_join_for_association
              importing
                i_association type string
              returning
                value(r_val) type string
              raising
                cx_sql_exception.

    "! <p class="shorttext synchronized" lang="EN">Returns the statement mapping for the possible associations</p>
    "! @parameter r_val | New statement (if associations are present) or empty (it no associations are found)
    methods assoc_mapped_to_join_if_any
              importing
                i_association type string
                i_join type string
              returning
                value(r_val) type string.

  protected section.

    data a_sql_statement type string.

    data a_processor type sql_statement=>t_processor.

    data an_association_processor type sql_statement=>t_association_processor.

endclass.

******************************************************************************************************************************************************************************************************************

class srtti_processor implementation.

  method serialize.

    data(serializable_results) = zcl_srtti_typedescr=>create_by_data_object( i_data_object ).

    call transformation id
      source srtti = serializable_results
             dobj = i_data_object
      result xml r_val.

  endmethod.
  method deserialize.

    field-symbols <data_object> type any table.

    data srtti type ref to zcl_srtti_typedescr.

    data data_object type ref to data.

    call transformation id
      source xml i_serialized
      result srtti = srtti.

    data(type) = cast cl_abap_datadescr( srtti->get_rtti( ) ).

    create data data_object type handle type.

    assign data_object->* to <data_object>.

    call transformation id
      source xml i_serialized
      result dobj = <data_object>.

    r_val = data_object.

  endmethod.

endclass.
class history implementation.

  method get_sbar_data_for_current_user.

    r_val = me->get_sbar_data_for_user( cl_abap_syst=>get_user_name( ) ).

  endmethod.
  method get_sbar_data_for_user.

    select id,
           natural_id,
           created_at,
           sql_statement,
           rows_no
      from z2ui5_nsql_c_hst
      where created_by eq @i_user
      order by created_at descending
      into table @r_val.

  endmethod.
  method get_rslt_data_for_current_user.

    r_val = me->get_rslt_data_for_user( i_id = i_id
                                        i_user = cl_abap_syst=>get_user_name( ) ).

  endmethod.
  method get_rslt_data_for_user.

    select single id,
                  serialized_results
      from z2ui5_nsql_c_hst
      where id eq @i_id
            and created_by eq @i_user
      into @data(data).

    r_val = value #( base corresponding #( data )
                     data = new srtti_processor( )->deserialize( data-serialized_results ) ).

  endmethod.
  method insert_new.

    field-symbols <results> type any table.

    data(t) = cl_os_system=>get_transaction_manager( )->create_transaction( ).

    t->start( ).

    assign i_entry-results->* to <results>.

    data(new_entry) = value z2ui5_nsql_c_hst( base corresponding #( i_entry )
                                              id = cl_system_uuid=>if_system_uuid_rfc4122_static~create_uuid_x16_by_version( 4 )
                                              created_at = utclong_current( )
                                              created_by = cl_abap_syst=>get_user_name( )
                                              rows_no = lines( <results> )
                                              serialized_results = new srtti_processor( )->serialize( <results> ) ).

    insert into z2ui5_nsql_c_hst
      values @new_entry.

    t->end( ).

    r_val = new_entry-id.

  endmethod.
  method delete_for_current_user.

    r_val = me->delete_for_user( i_entries = i_entries
                                 i_user = cl_abap_syst=>get_user_name( ) ).

  endmethod.
  method delete_for_user.

    types ids type range of z2ui5_nsql_c_hst-id.

    if i_entries is not initial.

      data(t) = cl_os_system=>get_transaction_manager( )->create_transaction( ).

      t->start( ).

      try.

        data(ids) = value ids( for <e> in i_entries
                               sign = 'I'
                               option = 'EQ'
                               ( low = <e> ) ).

        delete from z2ui5_nsql_c_hst
          where created_by eq @i_user
                and id in @ids.

      catch cx_sy_open_sql_db.

        loop at i_entries reference into data(id).

          delete from z2ui5_nsql_c_hst
            where created_by eq @i_user
                  and id eq @id->*.

        endloop.

      endtry.

      t->end( ).

    endif.

    r_val = me.

  endmethod.

endclass.
class main_view implementation.

  method constructor.

    me->a_ui5_client = i_ui5_client.

    me->a_parser = z2ui5_cl_xml_view=>factory( ).

    data(shell) = me->a_parser->shell( appwidthlimited = me->a_ui5_client->_bind_edit( i_state->page-app_width_limited ) ).

      data(page) = shell->page( title = 'Native SQL Console'(001) ).

        data(header_content) = page->header_content( ).

          data(overflow_toolbar) = header_content->overflow_toolbar( ).

            overflow_toolbar->label( 'Fallback Limit'(002)
                           )->input( width = `15%` value = me->a_ui5_client->_bind_edit( i_state->sql_editor_pane-fallback_max_rows )
                           )->button( text = 'Run'(003) press = me->a_ui5_client->_event( on_run=>event_name( ) ) type = `Emphasized` ##NO_TEXT
                           )->toolbar_spacer(
                           )->label( text = `Shell` ##NO_TEXT
                           )->switch( state = me->a_ui5_client->_bind_edit( i_state->page-app_width_limited )
                           )->link( text = 'Project on GitHub'(004) target = '_blank' href = 'https://github.com/abap2ui5-apps/abap-sql-console' ).

        data(flex_box) = page->flex_box( height = `100%` fitcontainer = abap_true rendertype = `Bare` ) ##NO_TEXT.

          data(responsive_splitter) = flex_box->responsive_splitter( defaultpane = `default` height = `100%` ) ##NO_TEXT.

            data(vertical_pane_container) = responsive_splitter->pane_container( orientation = `Vertical` ) ##NO_TEXT.

              data(horizontal_pane_container) = vertical_pane_container->pane_container( orientation = `Horizontal` ) ##NO_TEXT.

                "SQL Editor Pane
                data(editor_split_pane) = horizontal_pane_container->split_pane( requiredparentwidth = `600` ).

                  data(esp_layout_data) = editor_split_pane->layout_data( ns = `layout` )  ##NO_TEXT.

                    esp_layout_data->splitter_layout_data( size = me->a_ui5_client->_bind_edit( i_state->sql_editor_pane-layout_size ) ).

                  editor_split_pane->code_editor( type = `sql` value = me->a_ui5_client->_bind_edit( i_state->sql_editor_pane-statement ) ).

                "History Pane
                data(history_split_pane) = horizontal_pane_container->split_pane( requiredparentwidth = `400` ).

                  data(h_layout_data) = history_split_pane->layout_data( ns = `layout` ) ##NO_TEXT.

                    h_layout_data->splitter_layout_data( size = me->a_ui5_client->_bind_edit( i_state->history_pane-layout_size ) ).

                  data(h_list) = history_split_pane->list( items = me->a_ui5_client->_bind_edit( i_state->history_pane-items )
                                                           mode = `MultiSelect`
                                                           sticky = `ColumnHeaders,HeaderToolbar` ).

                    data(h_list_header_toolbar) = h_list->header_toolbar( ).

                      data(hlt_overflow_toolbar) = h_list_header_toolbar->overflow_toolbar( ).

                        hlt_overflow_toolbar->title( 'Query History'(006) ).

                        hlt_overflow_toolbar->toolbar_spacer( ).

                        hlt_overflow_toolbar->button( press = me->a_ui5_client->_event( on_select_all_history_items=>event_name( ) ) icon = `sap-icon://multiselect-all` ) ##NO_TEXT.

                        hlt_overflow_toolbar->button( press = me->a_ui5_client->_event( on_deselect_all_history_items=>event_name( ) ) icon = `sap-icon://multiselect-none` ) ##NO_TEXT.

                        hlt_overflow_toolbar->button( text = 'Delete'(007) press = me->a_ui5_client->_event( on_delete_history_items=>event_name( ) ) icon = `sap-icon://delete` ) ##NO_TEXT.

                    h_list->standard_list_item( type = `Navigation` ##NO_TEXT
                                                title = '{NATURAL_ID} - {CREATED_AT}'
                                                description = '{SQL_STATEMENT}'
                                                info = '{ROWS_NO}'
                                                infostate = '{INFOSTATE}'
                                                highlight = '{HIGHLIGHT}'
                                                press = me->a_ui5_client->_event( val = on_load_history_item=>event_name( )
                                                                                  t_arg = value #( ( `${ID}` ) ) )
                                                selected = `{SELECTED}` ).

              "Results Pane
              data(results_split_pane) = vertical_pane_container->split_pane( requiredparentwidth = `400` ).

                data(rsp_layout_data) = results_split_pane->layout_data( ns = `layout` )  ##NO_TEXT.

                  rsp_layout_data->splitter_layout_data( size = me->a_ui5_client->_bind_edit( i_state->results_pane-layout_size ) ).

                results_split_pane->vbox( id = `preview` fitcontainer = abap_true direction = `Row` )  ##NO_TEXT.

    new data_result_view( i_state = i_state
                          i_ui5_client = i_ui5_client )->set_for_display( ).

  endmethod.
  method set_for_display.

    me->a_ui5_client->view_display( me->a_parser->stringify( ) ).

    r_val = me.

  endmethod.

endclass.
class data_result_view implementation.

  method constructor.

    field-symbols <data> type standard table.

    me->a_ui5_client = i_ui5_client.

    me->a_parser = z2ui5_cl_xml_view=>factory( ).

    assign i_state->results_pane-output_data->* to <data>.

    if <data> is assigned.

      data(fields) = z2ui5_cl_util=>rtti_get_t_attri_by_any( <data> ).

      data(table) = me->a_parser->ui_table( id = `previewTab`
                                            rows = me->a_ui5_client->_bind_local( <data> )
                                            editable = abap_false
                                            alternaterowcolors = abap_true
                                            showcolumnvisibilitymenu = abap_true
                                            enablegrouping = abap_true
                                            enableselectall = abap_true
                                            enablecellfilter = abap_true
                                            selectionbehavior = `RowOnly`
                                            visiblerowcountmode = `Auto`
                                            selectionmode = `MultiToggle` ) ##NO_TEXT.

        data(table_extension) = table->ui_extension( ).

          data(te_overflow_toolbar) = table_extension->overflow_toolbar( width = `100%` ).

            te_overflow_toolbar->title( me->a_ui5_client->_bind( i_state->results_pane-title ) ).

            te_overflow_toolbar->toolbar_spacer( ).

            te_overflow_toolbar->input( width = `50%`
                                        value = me->a_ui5_client->_bind_edit( i_state->results_pane-wide_filter_string )
                                        description = 'Filter any column on enter'(008)
                                        submit = me->a_ui5_client->_event( on_wide_filtering=>event_name( ) ) ).

            te_overflow_toolbar->toolbar_spacer( ).

            te_overflow_toolbar->_z2ui5( )->spreadsheet_export( tableid = `previewTab`
                                                                icon = 'sap-icon://excel-attachment'
                                                                type = `Emphasized` ##NO_TEXT
                                                                columnconfig = me->a_ui5_client->_bind( val = i_state->results_pane-column_config
                                                                                                        custom_filter = new z2ui5_cl_cc_spreadsheet( )
                                                                                                        custom_mapper = z2ui5_cl_ajson_mapping=>create_lower_case( ) ) ).

        data(columns) = table->ui_columns( ).

        loop at fields reference into data(field).

          data(column) = columns->ui_column( width = `auto` ##NO_TEXT
                                             sortproperty = field->*-name
                                             filterproperty = field->*-name
                                             autoresizable = abap_true ).

            column->text( text = field->*-name
                          emptyindicatormode = abap_true
                          renderwhitespace = abap_true
                          wrapping = abap_false )->ui_template( )->label( text = `{` && field->*-name && `}`
                                                                          wrapping = abap_false ).

        endloop.

    else.

      me->a_parser->text( 'Data preview...'(009)  ).

    endif.

  endmethod.
  method set_for_display.

    me->a_ui5_client->nest_view_display( val = me->a_parser->stringify( )
                                         id = `preview` ##NO_TEXT
                                         method_insert = `addItem` ).

    r_val = me.

  endmethod.
  method redraw.

    me->set_for_display( ).

    me->a_ui5_client->view_model_update( ).

    r_val = me.

  endmethod.

endclass.
class on_start implementation.

  method event~handle.

    new main_view( i_state = zcl_2ui5_native_sql_console_st=>instance( )
                   i_ui5_client = i_ui5_client )->set_for_display( ).

  endmethod.
  method ui_interaction~id.

    r_val = `START`.

  endmethod.

endclass.
class on_run implementation.

  method event~handle.

"warn: only one assoc target allowed because it is otherwise difficult to distinguish between two identically named associations from different entities

"TODO: copy original statement to avoid modifying state
"TODO: consider delimited names
"TODO: consider CTEs
"TODO: handle from db_assoc?
"TODO: handle cardinality, join type, conditions? | in any case, throw error when using parameters
"TODO: create pseudo-functions? To return all views of system, all tables of system, all fields of table, all fields (and assocs) of view, on conditions of assocs

    field-symbols <table> type standard table.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    data(statement) = new sql_statement( i_processor = new cl_sql_statement( )
                                         i_association_processor = new zcl_association_processor( )
                                         i_sql_statement = state->sql_editor_pane-statement )->add_limit_if_none( state->sql_editor_pane-fallback_max_rows ).

    data(result) = statement->execute( ).

    state->sql_editor_pane-statement = result-updated_statement.

    state->results_pane-db_data = result-data.

    assign state->results_pane-db_data->* to <table>.

    state->results_pane = value #( base state->results_pane
                                   output_data = state->results_pane-db_data
                                   column_config = value #( for <e> in result-line_type->components
                                                            ( label = <e>-name
                                                              property = <e>-name
                                                              type = `String` ) ) ##NO_TEXT
                                   filters = z2ui5_cl_util=>filter_get_multi_by_data( <table> )
                                   title = |{ 'Number of Rows:'(010) } { z2ui5_cl_util=>c_trim( lines( <table> ) ) }| ).

    new data_result_view( i_state = state
                          i_ui5_client = i_ui5_client )->redraw( ).

    data(new_id) = new history( )->insert_new( value #( natural_id = value #( )
                                                        sql_statement = state->sql_editor_pane-statement
                                                        results = state->results_pane-db_data ) ).

    state->history_pane-last_item_successfully_added = new_id.

  endmethod.
  method ui_interaction~id.

    r_val = `RUN`.

  endmethod.

endclass.
class on_delete_history_items implementation.

  method ui_interaction~id.

    r_val = `DELETE_SELECTED_HISTORY`.

  endmethod.
  method event~handle.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    if state->event_awaiting_response eq me->event_name( ).

      state->event_awaiting_response = value #( ).

      if new user_approval( i_ui5_client )->is_confirmed( ).

        new history( )->delete_for_current_user( value #( for <e> in filter #( state->history_pane-items using key by_sel where selected eq abap_true )
                                                          ( <e>-id ) ) ).

        i_ui5_client->view_model_update( ).

        i_ui5_client->message_toast_display( 'All entries succesfully deleted from database'(014) ).

      endif.

    else.

      if state->history_pane-items is initial.

        i_ui5_client->message_box_display( 'No history entries found.'(012) ).

      elseif filter #( state->history_pane-items using key by_sel where selected eq abap_true ) is initial.

        i_ui5_client->message_box_display( 'No entries selected.'(015) ).

      else.

        i_ui5_client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory( conv #( 'Delete selected history entries from database?'(013) ) ) ).

        state->event_awaiting_response = me->event_name( ).

      endif.

    endif.

  endmethod.

endclass.
class on_load_history_item implementation.

  method event~handle.

    field-symbols <table> type any table.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    data(previous_result) = new history( )->get_rslt_data_for_current_user( state->history_pane-items[ key by_key
                                                                                                       id = exact #( i_ui5_client->get_event_arg( 1 ) ) ]-id ).

    data(line_type) = cast cl_abap_structdescr( cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( previous_result-data ) )->get_table_line_type( ) ).

    assign previous_result-data->* to <table>.

    state->results_pane = value #( base state->results_pane
                                   db_data = previous_result-data
                                   output_data = previous_result-data
                                   column_config = value #( for <e> in line_type->components
                                                            ( label = <e>-name
                                                              property = <e>-name
                                                              type = `String` ) ) ##NO_TEXT
                                   filters = z2ui5_cl_util=>filter_get_multi_by_data( <table> )
                                   title = |{ 'Number of Rows:'(010) } { z2ui5_cl_util=>c_trim( lines( <table> ) ) }| ).

    new data_result_view( i_state = state
                          i_ui5_client = i_ui5_client )->redraw( ).

    i_ui5_client->message_toast_display( 'History entry successfully loaded'(011) ).

  endmethod.
  method ui_interaction~id.

    r_val = `LOAD_HISTORY_ITEM`.

  endmethod.

endclass.
class user_approval implementation.

  method constructor.

    me->a_confirmation_popup = cast #( i_ui5_client->get_app( i_ui5_client->get( )-s_draft-id_prev_app ) ).

  endmethod.
  method is_confirmed.

    r_val = me->a_confirmation_popup->result( ).

  endmethod.

endclass.
class on_select_all_history_items implementation.

  method event~handle.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    state->history_pane-items = value #( let aux = state->history_pane-items in
                                         for <e> in aux
                                         ( value #( base <e>
                                                    selected = abap_true ) ) ).

    i_ui5_client->view_model_update( ).

  endmethod.
  method ui_interaction~id.

    r_val = `SELECT_FULL_HISTORY`.

  endmethod.

endclass.
class on_deselect_all_history_items implementation.

  method event~handle.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    state->history_pane-items = value #( let aux = state->history_pane-items in
                                         for <e> in aux
                                         ( value #( base <e>
                                                    selected = abap_false ) ) ).

    i_ui5_client->view_model_update( ).

  endmethod.
  method ui_interaction~id.

    r_val = `DESELECT_FULL_HISTORY`.

  endmethod.

endclass.
class error_acknowledged_interaction implementation.

  method ui_interaction~handle.

    me->a_downcasted_ref = cast #( i_ui5_client->get_app( i_ui5_client->get( )-s_draft-id_prev_app ) ).

    return.

  endmethod.
  method ui_interaction~id.

    r_val = `\CLASS=Z2UI5_CL_POP_ERROR`.

  endmethod.

endclass.
class on_wide_filtering implementation.

  method event~handle.

    field-symbols: <db_data> type any table,
                   <filtered_data> like <db_data>.

    data(state) = zcl_2ui5_native_sql_console_st=>instance( ).

    if state->results_pane-wide_filter_string is not initial.

      assign state->results_pane-db_data->* to <db_data>.

      create data state->results_pane-filtered_db_data like <db_data>.

      assign state->results_pane-filtered_db_data->* to <filtered_data>.

      <filtered_data> = <db_data>.

      z2ui5_cl_util=>itab_filter_by_val( exporting val = state->results_pane-wide_filter_string
                                         changing tab = <filtered_data> ).

      state->results_pane-output_data = state->results_pane-filtered_db_data.

    else.

      state->results_pane-output_data = state->results_pane-db_data.

    endif.

    new data_result_view( i_state = state
                          i_ui5_client = i_ui5_client )->redraw( ).

  endmethod.
  method ui_interaction~id.

    r_val = `RESULTS_WIDE_FILTER`.

  endmethod.

endclass.
class sql_statement implementation.

  method constructor.

    me->a_sql_statement = i_sql_statement.

    me->a_processor = i_processor.

    me->an_association_processor = i_association_processor.

  endmethod.
  method add_limit_if_none.

    constants ends_with_limit_clause type string value `(?i)\blimit\s+\d+\s*;?\s*\Z`.

    data(condensed) = condense( replace( val = me->a_sql_statement
                                         sub = cl_abap_char_utilities=>cr_lf
                                         with = ` `
                                         occ = 0 ) ).

    me->a_sql_statement = cond #( when find( val = condensed
                                             sub = `select top` ##NO_TEXT
                                             case = abap_false ) eq -1
                                       and find( val = condensed
                                                 pcre = ends_with_limit_clause ) eq -1
                                  then |{ replace( val = me->a_sql_statement
                                                   sub = `;`
                                                   with = `` ) }{ cl_abap_char_utilities=>cr_lf }  limit { i_fallback_limit };|
                                  else me->a_sql_statement ).

    r_val = me.

  endmethod.
  method execute.

    field-symbols <table> type standard table.

    data data type ref to data.

    data(statement_to_run) = cond #( let association = me->sole_associaton_if_any( )
                                         assoc_join = me->generated_join_for_association( association )
                                         mapping = me->assoc_mapped_to_join_if_any( i_association = association
                                                                                    i_join = assoc_join ) in
                                     when mapping is initial
                                     then me->a_sql_statement
                                     else mapping ).

    data(result) = me->a_processor->execute_query( statement_to_run ).

    data(result_metadata) = result->get_metadata( ).

    data(empty_line) = result->get_struct_ref( result_metadata ).

    data(line_type) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( empty_line ) ).

    data(table_type) = cl_abap_tabledescr=>get( line_type ).

    create data data type handle table_type.

    assign data->* to <table>.

    result->set_param_table( data ).

    result->next_package( ).

    r_val = value #( data = data
                     line_type = line_type
                     table_type = table_type
                     updated_statement = me->a_sql_statement ).

  endmethod.
  method sole_associaton_if_any.

    constants: at_least_two_path_expressions type string value `"?_[a-zA-Z\d]+"?(?:\[.*\])?\."?_[a-zA-Z\d]+"?`,
               path_expression_field type string value `("?_[\w]+"?)(?:\[.*?\])?\."?[a-zA-Z\d]+"?` ##NO_TEXT.

    if find( val = me->a_sql_statement
             pcre = at_least_two_path_expressions ) gt 0.

      raise exception new cx_sql_exception( sql_message = 'More than one associations in a single expression path are not allowed'(016) ).

    else.

      data(no_of_path_expression_fields) = count( val = me->a_sql_statement
                                                  pcre = path_expression_field ).

      do no_of_path_expression_fields times.

        if sy-index eq 1.

          data(first_association) = segment( val = match( val = me->a_sql_statement
                                                          pcre = path_expression_field
                                                          occ = sy-index )
                                             sep = `.`
                                             index = 1 ).

        else.

          data(following_association) = segment( val = match( val = me->a_sql_statement
                                                              pcre = path_expression_field
                                                              occ = sy-index )
                                                 sep = `.`
                                                 index = 1 ).

          if replace( val = following_association
                      sub = `"`
                      with = ``
                      occ = 0 ) ne replace( val = first_association
                                            sub = `"`
                                            with = ``
                                            occ = 0 ).

            data(more_than_one_associations) = abap_true.

            exit.

          endif.

        endif.

      enddo.

      r_val = cond #( when more_than_one_associations eq abap_true
                      then throw cx_sql_exception( sql_message = 'More than one unique path expression is not allowed'(005) )
                      else first_association ).

    endif.

  endmethod.
  method data_sources.

    constants data_source type string value `(?i)\b(?:from|join)\b\W"?\w+"?(?:."?\w+"?)` ##NO_TEXT.

    data(condensed) = condense( replace( val = me->a_sql_statement
                                         sub = cl_abap_char_utilities=>cr_lf
                                         with = ` `
                                         occ = 0 ) ).

    data(no_of_ds) = count( val = condensed
                            pcre = data_source ).

    data(no_of_ds_iter) = cond #( when no_of_ds lt 1
                                  then value string_table( )
                                  else reduce #( init aggr = value string_table( )
                                                 for i = 0 until i eq no_of_ds
                                                 next aggr = value #( base aggr
                                                                      ( ) ) ) ).

    r_val = value #( "for i = 0 until i eq no_of_ds jesus christ SAP what even is this expression for the love of all that is sacred
                     for <i> in no_of_ds_iter index into i
                     let ds_match = match( val = condensed
                                           pcre = data_source
                                           occ = i )
                         after_from_or_join = segment( val = ds_match
                                                       sep = ` `
                                                       index = 2 )
                         is_qualified_by_schema = xsdbool( find( val = after_from_or_join
                                                                 sub = `.` ) gt 0 ) in
                     ( replace( val = cond #( when is_qualified_by_schema eq abap_true
                                              then segment( val = after_from_or_join
                                                            sep = `.`
                                                            index = 2 )
                                              else after_from_or_join )
                                sub = `"`
                                with = ``
                                occ = 0 ) ) ).

  endmethod.
  method generated_join_for_association.

    r_val = me->an_association_processor->map_association_to_join( i_potential_data_sources = conv #( me->data_sources( ) )
                                                                   i_association = i_association ).

  endmethod.
  method assoc_mapped_to_join_if_any.
  "TODO: to add the join, first I have to detect whether it is part of a CTE or the main query (or both)
        "I should also detect to which of the queries the join corresponds when set operators are used
  "TODO: add association attributes (inner, cardinality, extra conditions) to join mapping

    data(clauses) = value string_table( ( ` WHERE` )
                                        ( ` GROUP BY` )
                                        ( ` HAVING` )
                                        ( ` ORDER BY` )
                                        ( ` LIMIT` )
                                        ( ` WITH HINT(` )
                                        ( ` ;` ) ).

    if i_join is not initial.

      data(new_statement) = condense( replace( val = me->a_sql_statement
                                               sub = cl_abap_char_utilities=>cr_lf
                                               with = ` `
                                               occ = 0 ) ).

      loop at clauses reference into data(clause).

        data(with_clause) = substring_before( val = new_statement
                                              sub = clause->*
                                              case = abap_false ).

        if with_clause is not initial.

          new_statement = with_clause && ` ` && i_join && substring_from( val = new_statement
                                                                          sub = clause->*
                                                                          case = abap_false ).

          data(replaced) = abap_true.

          exit.

        endif.

      endloop.

      if not ( replaced eq abap_true ).

        new_statement &&= ` ` && i_join.

      endif.

      new_statement = replace( val = new_statement
                               sub = i_association
                               with = `"=A0"`
                               occ = 0 ).

      r_val = new_statement.

    endif.

  endmethod.

endclass.
