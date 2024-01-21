CLASS z2ui5_sql_cl_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA:
      BEGIN OF ms_draft,
        preview_tab          TYPE REF TO data,
        preview_tab_backup   TYPE REF TO data,
        preview_cont_size    TYPE string,
        preview_title        TYPE string,
        preview_search_field TYPE string,
        sql_input            TYPE string,
        sql_s_command        TYPE z2ui5_cl_util_func=>ty_s_sql_result,
        sql_max_rows         TYPE i,
        sql_cont_size        TYPE string,
        history_cont_size    TYPE string,
        history_tab          TYPE z2ui5_sql_cl_history_api=>ty_t_entry,
        appwidthlimited      TYPE abap_bool,
      END OF ms_draft.

  PROTECTED SECTION.

    DATA:
      BEGIN OF ms_control,
        check_initialized          TYPE abap_bool,
        callback_pop_session_load  TYPE string,
        callback_pop_history_clear TYPE string,
      END OF ms_control.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS z2ui5_view_display.
    METHODS sql_db_read.
    METHODS history_db_read.
    METHODS preview_on_filter_search.
    METHODS history_on_clear_pressed.
    METHODS z2ui5_on_callback.
    METHODS z2ui5_on_event.
    METHODS sql_on_run.
    METHODS z2ui5_on_init.

    METHODS sql_view_display
      IMPORTING
        view_sql TYPE REF TO z2ui5_cl_xml_view.
    METHODS preview_view
      IMPORTING
        view_output TYPE REF TO z2ui5_cl_xml_view.
    METHODS history_view
      IMPORTING
        view_history TYPE REF TO z2ui5_cl_xml_view.
    METHODS preview_on_filter_clear.
    METHODS z2ui5_on_callback_pop_confirm
      IMPORTING
        io_popup TYPE REF TO z2ui5_if_app.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_sql_cl_app_01 IMPLEMENTATION.

  METHOD sql_db_read.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab->* TO <tab>.

    SELECT FROM (ms_draft-sql_s_command-table) FIELDS *
        INTO CORRESPONDING FIELDS OF TABLE @<tab>
        UP TO @ms_draft-sql_max_rows ROWS.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab_backup->* TO <tab2>.

    <tab2> = <tab>.
    ms_draft-preview_title = ms_draft-sql_s_command-table && ` (` && z2ui5_cl_util_func=>c_trim( lines( <tab2> ) ) && `)`.

  ENDMETHOD.


  METHOD history_db_read.

    ms_draft-history_tab = CORRESPONDING #( z2ui5_sql_cl_history_api=>db_read_by_user( ) EXCEPT result_data ).

  ENDMETHOD.


  METHOD history_on_clear_pressed.

    IF ms_draft-history_tab IS INITIAL.
      client->message_box_display( `No history entries found. No action needed.` ).
      RETURN.
    ENDIF.
    ms_control-callback_pop_history_clear = client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory( `Delete all history entries from database?` ) ).

  ENDMETHOD.


  METHOD history_view.

    view_history->list(
          items           = client->_bind_edit( ms_draft-history_tab )
          mode            = `SingleSelectMaster`
          selectionchange = client->_event( 'SELCHANGE' )
           sticky              = 'ColumnHeaders,HeaderToolbar'
             )->header_toolbar(
             )->overflow_toolbar(
                 )->title( 'History'
                 )->toolbar_spacer(
                )->toolbar_spacer(
                 )->button( text = `Clear` press = client->_event( `HISTORY_CLEAR` ) icon = `sap-icon://delete`
        )->get_parent( )->get_parent(
          )->standard_list_item(
              title       = '{TABNAME}'
              description = '{SQL_COMMAND}'
*              icon        = '{ICON}'
              info        = '{COUNTER}'
              press       = client->_event( 'TEST' )
*              selected    = `{SELECTED}`
         ).

  ENDMETHOD.


  METHOD preview_on_filter_search.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab->* TO <tab>.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab_backup->* TO <tab2>.

    <tab> = <tab2>.

    IF ms_draft-preview_search_field IS NOT INITIAL.

      z2ui5_cl_util_func=>get_tab_filter_by_val(
        EXPORTING
          val = ms_draft-preview_search_field
        CHANGING
          tab = <tab> ).

    ENDIF.

    ms_draft-preview_title = ms_draft-sql_s_command-table && ` (` && z2ui5_cl_util_func=>c_trim( lines( <tab> ) ) && `)`.
    client->view_model_update( ).
    client->message_toast_display( `Search field filter updated` ).

  ENDMETHOD.


  METHOD preview_view.

    IF ms_draft-preview_tab IS BOUND.
      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN  ms_draft-preview_tab->* TO <tab>.

      DATA(tab) = view_output->table(
              items = client->_bind( <tab> )
              growing = abap_true
              growingscrolltoload = abap_true
              growingthreshold = `100`
              sticky              = 'ColumnHeaders,HeaderToolbar'
         )->header_toolbar(
             )->overflow_toolbar(
                 )->title( client->_bind( ms_draft-preview_title )
                 )->toolbar_spacer(
                )->input( width = `30%` value = client->_bind_edit( ms_draft-preview_search_field ) description = `All Column Search`  submit = client->_event( `PREVIEW_SEARCH` )
                 )->toolbar_spacer(
                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
                 )->button( text = `Clear Filter` press = client->_event( `PREVIEW_CLEAR_FILTER` ) icon = `sap-icon://delete`
        )->get_parent( )->get_parent( ).

      DATA(lt_fields) = z2ui5_xlsx_cl_utility=>get_fieldlist_by_table( <tab> ).

      DATA(lo_columns) = tab->columns( ).
      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
        lo_columns->column( )->text( lv_field ).
      ENDLOOP.

      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
      LOOP AT lt_fields INTO lv_field FROM 1.
        lo_cells->text( `{` && lv_field && `}` ).
      ENDLOOP.

    ELSE.
      view_output->panel( headertext = `data preview...`  ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_on_run.

    TRY.

        DATA(ls_sql_command) = z2ui5_cl_util_func=>get_sql_by_string( ms_draft-sql_input ).

        IF ms_draft-sql_s_command-table <> ls_sql_command-table.
          CREATE DATA ms_draft-preview_tab TYPE STANDARD TABLE OF (ls_sql_command-table).
          CREATE DATA ms_draft-preview_tab_backup TYPE STANDARD TABLE OF (ls_sql_command-table).
          z2ui5_view_display( ).
        ELSE.
          client->view_model_update( ).
        ENDIF.

        ms_draft-sql_s_command = ls_sql_command.
        sql_db_read( ).

        FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        ASSIGN ms_draft-preview_tab->* TO <tab>.
        z2ui5_sql_cl_history_api=>db_create( VALUE #(
            sql_command = ms_draft-sql_input
            tabname     = ms_draft-sql_s_command-table
            counter     = lines( <tab> )
            result_data = z2ui5_cl_util_func=>trans_xml_any_2( <tab> ) ) ).

        history_db_read( ).

      CATCH cx_root INTO DATA(x).
        client->nav_app_call( z2ui5_cl_popup_error=>factory( x ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD sql_view_display.

    view_sql->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
*                 )->button( text = `Clear` press = client->_event( `PREVIEW_CLEAR` ) icon = `sap-icon://delete`
         )->code_editor(
                type  = `sql`
                value = client->_bind_edit( ms_draft-sql_input ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF ms_control-check_initialized = abap_false.
      ms_control-check_initialized = abap_true.
      z2ui5_on_init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      z2ui5_on_callback( ).
      RETURN.
    ENDIF.

    z2ui5_on_event(  ).

  ENDMETHOD.


  METHOD z2ui5_on_init.

*    DATA(lv_id) = z2ui5_sql_cl_history_api=>db_read_session( ).
*    TRY.
**    DATA(lo_app) = client->get_app( lv_id ).
*        ms_control-callback_pop_session_load = client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory( `Active session found, you want to return?` ) ).
*        RETURN.
*      CATCH cx_root.
*    ENDTRY.

    ms_draft-sql_input = `Select from T100 fields * .`.
    ms_draft-history_cont_size = `30%`.
    ms_draft-sql_cont_size = `auto`.
    ms_draft-preview_cont_size = `auto`.
    ms_draft-sql_max_rows = 10000.
    ms_draft-appwidthlimited = abap_true.
    z2ui5_view_display( ).

    history_db_read( ).

  ENDMETHOD.


  METHOD z2ui5_on_callback.

    TRY.
        DATA(lo_popup_confirm) = CAST z2ui5_cl_popup_to_confirm( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        IF lo_popup_confirm->result( ).
          z2ui5_on_callback_pop_confirm( lo_popup_confirm ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD z2ui5_view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( appwidthlimited = client->_bind_edit( ms_draft-appwidthlimited ) )->page(
    title = 'abap2UI5 - SQL Console'
    navbuttonpress = client->_event( 'BACK' )
    shownavbutton = abap_true
            )->header_content(
                )->overflow_toolbar(
                )->label( `max Rows`
                )->input( width = `15%` value = client->_bind_edit( ms_draft-sql_max_rows )
                  )->button(
            text  = 'Run'
            press = client->_event( 'RUN' )
            type  = 'Emphasized'
                )->toolbar_spacer(
                )->label( `Shell`
                )->switch( state = client->_bind_edit( ms_draft-appwidthlimited )
                )->link( text = 'Project on GitHub' target = '_blank' href = 'https://github.com/oblomov-dev/a2UI5-sql_console'
    )->get_parent( )->get_parent( ).

    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout' ).

    DATA(cont_main) = page->responsive_splitter( defaultpane = `default`
         )->pane_container( orientation = `Vertical` ).

    DATA(cont_sub) = cont_main->pane_container( orientation = `Horizontal` ).

    DATA(view_sql) = cont_sub->split_pane( requiredparentwidth = `600`
         )->layout_data( ns = `layout`
           )->splitter_layout_data( size = client->_bind_edit( ms_draft-sql_cont_size )
           )->get_parent( )->get_parent( ).

    sql_view_display( view_sql ).

    DATA(view_history) = cont_sub->split_pane( requiredparentwidth = `400`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-history_cont_size )
           )->get_parent( )->get_parent( ).

    history_view( view_history ).

    DATA(view_preview) = cont_main->split_pane( requiredparentwidth = `400` id = `default`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-preview_cont_size )
            )->get_parent( )->get_parent( ).

    preview_view( view_preview ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_on_event.

    CASE client->get( )-event.

      WHEN 'PREVIEW_CLEAR_FILTER'.
        preview_on_filter_clear( ).

      WHEN 'PREVIEW_SEARCH'.
        preview_on_filter_search( ).

      WHEN 'RUN'.
        sql_on_run( ).

      WHEN 'HISTORY_CLEAR'.
        history_on_clear_pressed( ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD preview_on_filter_clear.

    CLEAR ms_draft-preview_search_field.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab->* TO <tab>.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab_backup->* TO <tab2>.
    <tab> = <tab2>.

    ms_draft-preview_title = ms_draft-sql_s_command-table && ` (` && z2ui5_cl_util_func=>c_trim( lines( <tab> ) ) && `)`.

    client->view_model_update( ).
    client->message_toast_display( 'all filter deleted' ).

  ENDMETHOD.


  METHOD z2ui5_on_callback_pop_confirm.

    CASE io_popup->id_app.

      WHEN ms_control-callback_pop_session_load.


      WHEN ms_control-callback_pop_history_clear.

        CLEAR ms_draft-history_tab.
        client->view_model_update( ).
        z2ui5_sql_cl_history_api=>db_delete( ).
        client->message_toast_display( `All entries succesfully deleted from database` ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
