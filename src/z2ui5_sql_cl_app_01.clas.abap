CLASS z2ui5_sql_cl_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_history_out,
        s_db  TYPE z2ui5_sql_cl_history_api=>ty_s_entry,
        time  TYPE string,
        date  TYPE string,
        selkz TYPE abap_bool,
      END OF ty_history_out.

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
        history_tab          TYPE STANDARD TABLE OF ty_history_out WITH EMPTY KEY,
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
        io_popup TYPE REF TO z2ui5_cl_popup_to_confirm.
    METHODS history_on_load.
    METHODS history_db_save.
    METHODS z2ui5_on_init_check_draft.
    METHODS z2ui5_on_init_set_app.

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

    CLEAR ms_draft-history_tab.
    LOOP AT z2ui5_sql_cl_history_api=>db_read_multi_by_user( ) REFERENCE INTO DATA(lr_history).


      INSERT VALUE #(
          s_db =  CORRESPONDING #( lr_history->* EXCEPT result_data )
          time = |{ lr_history->timestampl TIMESTAMP = ISO }|
*          date = z2ui5_cl_util_func=>time_get_date_by_stampl( lr_history->timestampl )
          ) INTO TABLE ms_draft-history_tab.

    ENDLOOP.

    SORT ms_draft-history_tab BY s_db-timestampl DESCENDING.

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
          selectionchange = client->_event( val = 'HISTORY_LOAD' )
           sticky              = 'ColumnHeaders,HeaderToolbar'
             )->header_toolbar(
             )->overflow_toolbar(
                 )->title( 'History'
                 )->toolbar_spacer(
                )->toolbar_spacer(
                 )->button( text = `Clear` press = client->_event( `HISTORY_CLEAR` ) icon = `sap-icon://delete`
        )->get_parent( )->get_parent(
          )->standard_list_item(
              title       = '{S_DB/TABNAME} - {DATE} {TIME}'
*              title       = `{ path: 'DATE' ,  type: 'sap.ui.model.type.String' , formatter : function(value){ return '100'; } }`
*              title       = `{ path: 'DATE' ,  type: 'sap.ui.model.type.String' , formatter : function(value){ return '100'; } }`
              description = '{S_DB/SQL_COMMAND}'
*              icon        = '{ICON}'
              info        = '{S_DB/COUNTER}'
*              press       = client->_event( val = 'HISTORY_LOAD' t_arg = VALUE #( ( `{S_DB/UUID}`) ) )
              selected    = `{SELKZ}`
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

      DATA(lt_fields) = z2ui5_cl_util_func=>rtti_get_t_comp_by_data( <tab> ).

      DATA(lo_columns) = tab->columns( ).
      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
        lo_columns->column( )->text( lv_field-name ).
      ENDLOOP.

      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
      LOOP AT lt_fields INTO lv_field FROM 1.
        lo_cells->text( `{` && lv_field-name && `}` ).
      ENDLOOP.

    ELSE.
      view_output->panel( headertext = `data preview...`  ).
    ENDIF.

  ENDMETHOD.


  METHOD sql_on_run.

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

    history_db_save( ).
    history_db_read( ).
    client->message_toast_display( `Database succesfully loaded` ).

  ENDMETHOD.


  METHOD sql_view_display.

    view_sql->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
*                 )->button( text = `Clear` press = client->_event( `PREVIEW_CLEAR` ) icon = `sap-icon://delete`
         )->code_editor(
                type  = `sql`
                value = client->_bind_edit( ms_draft-sql_input ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.
    TRY.

        me->client = client.

        IF ms_control-check_initialized = abap_false.
          ms_control-check_initialized = abap_true.
          z2ui5_on_init_check_draft( ).
          RETURN.
        ENDIF.

        IF client->get( )-check_on_navigated = abap_true.
          z2ui5_on_callback( ).
          RETURN.
        ENDIF.

        z2ui5_on_event(  ).

      CATCH cx_root INTO DATA(x).
        client->nav_app_call( z2ui5_cl_popup_error=>factory( x ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD z2ui5_on_callback.

    TRY.
        DATA(lo_popup_confirm) = CAST z2ui5_cl_popup_to_confirm( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        z2ui5_on_callback_pop_confirm( lo_popup_confirm ).
        RETURN.
      CATCH cx_root.
    ENDTRY.

    TRY.
        z2ui5_view_display( ).
        RETURN.
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

    z2ui5_sql_cl_history_api=>db_create_draft( client->get( )-s_draft-id ).

    CASE client->get( )-event.

      when `PREVIEW_FILTER`.
      client->nav_app_call( z2ui5_cl_popup_get_range_multi=>factory( ms_draft-preview_tab ) ).

      WHEN 'PREVIEW_CLEAR_FILTER'.
        preview_on_filter_clear( ).

      WHEN 'PREVIEW_SEARCH'.
        preview_on_filter_search( ).

      WHEN 'RUN'.
        sql_on_run( ).

      WHEN 'HISTORY_CLEAR'.
        history_on_clear_pressed( ).

      WHEN 'HISTORY_LOAD'.
        history_on_load( ).

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

    CASE io_popup->z2ui5_if_app~id_app.

      WHEN ms_control-callback_pop_session_load.

        IF io_popup->result( ).
          DATA(lv_id) = z2ui5_sql_cl_history_api=>db_read_draft( ).
          client->nav_app_leave( client->get_app( lv_id ) ).
        ELSE.
          z2ui5_on_init_set_app( ).
        ENDIF.

      WHEN ms_control-callback_pop_history_clear.

        CLEAR ms_draft-history_tab.
        client->view_model_update( ).
        z2ui5_sql_cl_history_api=>db_delete( ).
        client->message_toast_display( `All entries succesfully deleted from database` ).

    ENDCASE.

  ENDMETHOD.

  METHOD history_on_load.

    DATA(ls_arg) = ms_draft-history_tab[ selkz = abap_true ].
    DATA(ls_entry) = z2ui5_sql_cl_history_api=>db_read_by_id( ls_arg-s_db-uuid ).
    ms_draft-sql_input = ls_entry-sql_command.
    z2ui5_cl_util_func=>rtti_xml_set_to_data(
      EXPORTING
        rtti_data = ls_entry-result_data
      IMPORTING
        e_data    = ms_draft-preview_tab
    ).

    ms_draft-preview_tab_backup = z2ui5_cl_util_func=>copy_ref_data_to_ref_data( ms_draft-preview_tab ).

    z2ui5_view_display( ).
    client->message_toast_display( `history entry successfully loaded` ).

  ENDMETHOD.


  METHOD history_db_save.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-preview_tab->* TO <tab>.
    z2ui5_sql_cl_history_api=>db_create( VALUE #(
        sql_command = ms_draft-sql_input
        tabname     = ms_draft-sql_s_command-table
        counter     = lines( <tab> )
        result_data = z2ui5_cl_util_func=>rtti_xml_get_by_data( <tab> ) ) ).

  ENDMETHOD.


  METHOD z2ui5_on_init_check_draft.

    TRY.
        DATA(lv_id) = z2ui5_sql_cl_history_api=>db_read_draft( ).
        DATA(lo_app) = client->get_app( lv_id ).
        ms_control-callback_pop_session_load = client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory( |Active draft for user { sy-uname } found, you want to return?| ) ).
      CATCH cx_root.
        z2ui5_on_init_set_app( ).
    ENDTRY.

  ENDMETHOD.


  METHOD z2ui5_on_init_set_app.

    ms_draft-sql_input = `Select from T100 fields * .`.
    ms_draft-history_cont_size = `30%`.
    ms_draft-sql_cont_size = `auto`.
    ms_draft-preview_cont_size = `auto`.
    ms_draft-sql_max_rows = 10000.
    ms_draft-appwidthlimited = abap_true.
    z2ui5_view_display( ).

    history_db_read( ).

  ENDMETHOD.

ENDCLASS.
