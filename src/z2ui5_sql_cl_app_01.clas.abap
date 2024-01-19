CLASS z2ui5_sql_cl_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mt_history TYPE z2ui5_sql_cl_history_api=>ty_t_entry.

    DATA:
      BEGIN OF ms_draft,
        input         TYPE string,
        t_tab         TYPE REF TO data,
        s_sql_command TYPE z2ui5_sql_cl_util=>ty_s_sql_result,
        size_sql      TYPE string,
        size_history  TYPE string,
        size_preview  TYPE string,
        max_rows      TYPE i,
      END OF ms_draft.

    METHODS set_view
      IMPORTING
        client TYPE REF TO z2ui5_if_client.

    METHODS db_read.
    METHODS db_read_history.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    METHODS on_run.
    METHODS on_init.
    METHODS view_sql_editor
      IMPORTING
        view_sql TYPE REF TO z2ui5_cl_xml_view.
    METHODS view_preview
      IMPORTING
        view_output TYPE REF TO z2ui5_cl_xml_view.
    METHODS view_history
      IMPORTING
        view_history TYPE REF TO z2ui5_cl_xml_view.

  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_SQL_CL_APP_01 IMPLEMENTATION.


  METHOD db_read.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-t_tab->* TO <tab>.

    SELECT FROM (ms_draft-s_sql_command-table) FIELDS *
        INTO CORRESPONDING FIELDS OF TABLE @<tab>
        UP TO @ms_draft-max_rows ROWS.

  ENDMETHOD.


  METHOD db_read_history.

    mt_history = CORRESPONDING #( z2ui5_sql_cl_history_api=>db_read_by_user( ) EXCEPT result_data ).

  ENDMETHOD.


  METHOD on_init.

    ms_draft-input = `Select from T100 fields * .`.
    ms_draft-size_history = `30%`.
    ms_draft-size_sql = `auto`.
    ms_draft-size_preview = `auto`.
    ms_draft-max_rows = 10000.
    set_view( client ).

    db_read_history( ).

  ENDMETHOD.


  METHOD on_run.

    TRY.

        DATA(ls_sql_command) = z2ui5_sql_cl_util=>get_sql_by_string( ms_draft-input ).

        IF ms_draft-s_sql_command-table <> ls_sql_command-table.
          CREATE DATA ms_draft-t_tab TYPE STANDARD TABLE OF (ls_sql_command-table).
          set_view( client ).
        ELSE.
          client->view_model_update( ).
        ENDIF.

        ms_draft-s_sql_command = ls_sql_command.
        db_read( ).

        FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        ASSIGN ms_draft-t_tab->* TO <tab>.
        z2ui5_sql_cl_history_api=>db_create( VALUE #(
            sql_command = ms_draft-input
            tabname     = ms_draft-s_sql_command-table
            counter     = lines( <tab> )
            result_data = /ui2/cl_json=>serialize( <tab> ) ) ).

        db_read_history( ).

      CATCH cx_root INTO DATA(x).
        client->nav_app_call( z2ui5_cl_popup_error=>factory( x ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD set_view.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
    title = 'abap2UI5 - SQL Console'
    navbuttonpress = client->_event( 'BACK' )
    shownavbutton = abap_true
            )->header_content(
                )->link( text = 'Project on GitHub'        target = '_blank' href = 'https://github.com/oblomov-dev/a2UI5-sql_console'
    )->get_parent( ).

    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout' ).

    DATA(cont_main) = page->responsive_splitter( defaultpane = `default`
         )->pane_container( orientation = `Vertical` ).

    DATA(cont_sub) = cont_main->pane_container( orientation = `Horizontal` ).

    DATA(view_sql) = cont_sub->split_pane( requiredparentwidth = `600`
         )->layout_data( ns = `layout`
           )->splitter_layout_data( size = client->_bind_edit( ms_draft-size_sql )
           )->get_parent( )->get_parent( ).

    view_sql_editor( view_sql ).

    DATA(view_history) = cont_sub->split_pane( requiredparentwidth = `400`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-size_history )
           )->get_parent( )->get_parent( ).

    view_history( view_history ).

    DATA(view_preview) = cont_main->split_pane( requiredparentwidth = `400` id = `default`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-size_preview )
            )->get_parent( )->get_parent( ).

    view_preview( view_preview ).

    page->footer( )->overflow_toolbar(
        )->toolbar_spacer(
        )->button(
            text  = 'Run'
            press = client->_event( 'RUN' )
            type  = 'Emphasized'
            icon = 'sap-icon://upload-to-cloud' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD view_history.

    view_history->list(
          headertext      = 'History'
          items           = client->_bind_edit( mt_history )
          mode            = `SingleSelectMaster`
          selectionchange = client->_event( 'SELCHANGE' )
          )->standard_list_item(
              title       = '{TABNAME}'
              description = '{SQL_COMMAND}'
*              icon        = '{ICON}'
              info        = '{COUNTER}'
              press       = client->_event( 'TEST' )
*              selected    = `{SELECTED}`
         ).

  ENDMETHOD.


  METHOD view_preview.

    IF ms_draft-t_tab IS BOUND.
      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN  ms_draft-t_tab->* TO <tab>.

      DATA(tab) = view_output->table(
              items = client->_bind( <tab> )
              growing = abap_true
              growingscrolltoload = abap_true
              growingthreshold = `100`
         )->header_toolbar(
             )->overflow_toolbar(
                 )->title( `(1) Data Preview`
                 )->toolbar_spacer(
                 )->button( text = `Reset` press = client->_event( `LOAD` ) icon = `sap-icon://refresh` type = `Emphasized`
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


  METHOD view_sql_editor.

    view_sql->code_editor(
                type  = `sql`
                value = client->_bind_edit( ms_draft-input ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      on_init( ).
      RETURN.

    ENDIF.

    CASE client->get( )-event.

      WHEN 'RUN'.
        on_run( ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
