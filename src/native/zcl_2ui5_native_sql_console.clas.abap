class zcl_2ui5_native_sql_console definition
                                  public
                                  create public.

  public section.

    interfaces: z2ui5_if_app.

    class-methods class_constructor.

    methods initialize.

    methods set_popup_state.

    methods handle_popup
              raising
                cx_static_check.

    methods handle_event
              raising
                cx_static_check.

    data a_state type ref to zcl_2ui5_native_sql_console_st.

  protected section.

    data a_ui5_client type ref to z2ui5_if_client.

endclass.
class zcl_2ui5_native_sql_console implementation.

  method class_constructor.

    cl_os_system=>init_and_set_modes( i_external_commit = oscon_false
                                      i_update_mode = oscon_dmode_default ).

  endmethod.
  method z2ui5_if_app~main.

    me->a_ui5_client = client.

    me->a_state = zcl_2ui5_native_sql_console_st=>instance( me->a_state ).

    try.

      data(t) = cl_os_system=>get_transaction_manager( )->create_transaction( ).

      t->start( ).

      if not ( me->a_state->is_initialized eq abap_true ).

        me->initialize( ).

      else.

        me->set_popup_state( ).

        if me->a_state->is_popup_interaction eq abap_true.

          me->handle_popup( ).

        else.

          me->handle_event( ).

        endif.

      endif.

      t->end( ).

    catch cx_root into data(e) ##CATCH_ALL.

      t->undo( ).

      me->a_ui5_client->nav_app_call( z2ui5_cl_pop_error=>factory( e ) ).

    endtry.

    "TODO: consider merging event and popup interactions into interface UI_INTERACTION
    "TODO: refactor state vars

    "TODO: add tests

    "TODO: add association mappings to join
    "TODO: clearly show nulls
    "TODO: fix ctrl + C (use another table?)
    "TODO: fix 'as json'/'as xml' queries (do not fit)

  endmethod.
  method initialize.

    data standard_connection_schema type c length 255.

    call function 'DB_DBUSER'
      importing
        dbuser = standard_connection_schema.

    me->a_ui5_client->view_display( z2ui5_cl_xml_view=>factory( )->_z2ui5( )->timer( me->a_ui5_client->_event( `START` )
                                                                           )->_generic( ns = `html` ##NO_TEXT
                                                                                        name = `script`
                                                                           )->_cc_plain_xml( z2ui5_cl_cc_spreadsheet=>get_js( )
                                                                )->stringify( ) ).

    me->a_state->main_view = value #( sql_statement = |select *{ cl_abap_char_utilities=>cr_lf }  from "{ standard_connection_schema }"."T100"{ cl_abap_char_utilities=>cr_lf }  limit 100;| ##NO_TEXT
                                      history_cont_size = `30%`
                                      sql_cont_size = `auto`
                                      sql_max_rows = 100
                                      app_width_limited = abap_true ).

    me->a_state->result_view = value #( cont_size = `auto` ) ##NO_TEXT.

    me->a_state->is_initialized = abap_true.

  endmethod.
  method set_popup_state.

    me->a_state->is_popup_interaction = xsdbool( me->a_ui5_client->get( )-check_on_navigated eq abap_true ).

  endmethod.
  method handle_popup.

    types: begin of popup,
             class type abap_abstypename,
             ref type ref to popup_interaction,
           end of popup,
           popups type hashed table of popup with unique key class.

    data(popups) = value popups( ( class = `\CLASS=Z2UI5_CL_POP_TO_CONFIRM`
                                   ref = new clear_history_interaction( ) )
                                 ( class = `\CLASS=Z2UI5_CL_POP_ERROR`
                                   ref = new error_acknowledged_interaction( ) ) ).

    data(class_name) = cl_abap_classdescr=>get_class_name( me->a_ui5_client->get_app( me->a_ui5_client->get( )-s_draft-id_prev_app ) ).

    popups[ class = class_name ]-ref->handle( me->a_ui5_client ).

    me->a_state->main_view-history_tab = value #( let aux = me->a_state->main_view-history_tab
                                                      user_tz = cl_abap_context_info=>get_user_time_zone( ) in
                                                  for <e> in new history( )->get_sbar_data_for_current_user( )
                                                  let status = cond #( when <e>-id eq me->a_state->main_view-history_successfully_added
                                                                       then `Success`
                                                                       else `None` ) in ##NO_TEXT
                                                  ( value #( base value #( aux[ key by_key
                                                                                id = <e>-id ] default corresponding #( <e> ) )
                                                             highlight = status
                                                             infostate = status
                                                             created_at = |{ <e>-created_at timestamp = user timezone = user_tz }| ) ) ).

  endmethod.
  method handle_event.

    types: begin of event,
             name type string,
             ref type ref to event,
           end of event,
           events type hashed table of event with unique key name.

    data(events) = value events( ( name = 'START'
                                   ref = new on_start( ) )
                                 ( name = 'RUN'
                                   ref = new on_run( ) )
                                 ( name = 'LOAD_HISTORY_ITEM'
                                   ref = new on_load_history_item( ) )
                                 ( name = 'SELECT_FULL_HISTORY'
                                   ref = new on_select_all_history_items( ) )
                                 ( name = 'DESELECT_FULL_HISTORY'
                                   ref = new on_deselect_all_history_items( ) )
                                 ( name = 'DELETE_SELECTED_HISTORY'
                                   ref = new on_delete_history_items( ) )
                                 ( name = 'RESULTS_WIDE_FILTER'
                                   ref = new on_wide_filtering( ) ) ).

    data(event_name) = me->a_ui5_client->get( )-event.

    events[ name = event_name ]-ref->handle( me->a_ui5_client ).

    me->a_state->main_view-history_tab = value #( let aux = me->a_state->main_view-history_tab
                                                      user_tz = cl_abap_context_info=>get_user_time_zone( ) in
                                                  for <e> in new history( )->get_sbar_data_for_current_user( )
                                                  let status = cond #( when <e>-id eq me->a_state->main_view-history_successfully_added
                                                                       then `Success`
                                                                       else `None` ) in ##NO_TEXT
                                                  ( value #( base value #( aux[ key by_key
                                                                                id = <e>-id ] default corresponding #( <e> ) )
                                                             highlight = status
                                                             infostate = status
                                                             created_at = |{ <e>-created_at timestamp = user timezone = user_tz }| ) ) ).

  endmethod.

endclass.
