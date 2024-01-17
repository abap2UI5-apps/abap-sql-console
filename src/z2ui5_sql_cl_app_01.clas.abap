CLASS z2ui5_sql_cl_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA ms_file TYPE z2ui5_file_cl_db_api=>ty_s_file.
    DATA mv_type TYPE string.
    DATA mv_path TYPE string.
    DATA mv_editor TYPE string.
    DATA mv_check_editable TYPE abap_bool.
    DATA check_initialized TYPE abap_bool.

    DATA:
      BEGIN OF ms_draft,
        input TYPE string,
        t_tab TYPE REF TO data,
        size_sql type string,
        size_history type string,
        size_preview type string,
      END OF ms_draft.

    METHODS view_display
      IMPORTING
        client TYPE REF TO z2ui5_if_client.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_sql_cl_app_01 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      ms_draft-input = `Select from USR01 fields * .`.
      ms_draft-size_history = `auto`.
      ms_draft-size_sql = `auto`.
      ms_draft-size_preview = `auto`.
      view_display( client ).
      RETURN.

    ENDIF.

    CASE client->get( )-event.

      WHEN 'RUN'.
        view_display( client ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.
    ENDCASE.



  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
    title = 'abap2UI5 - SQL Console'
    navbuttonpress = client->_event( 'BACK' )
    shownavbutton = abap_true
            )->header_content(
*                )->link( text = 'Demo'        target = '_blank' href = 'https://twitter.com/abap2UI5/status/1631562906570575875'
*                )->link( text = 'Source_Code' target = '_blank' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout' ).

    DATA(cont_main) = page->responsive_splitter( defaultpane = `default`
         )->pane_container( orientation = `Vertical` ).

    DATA(cont_sub) =  cont_main->pane_container( orientation = `Horizontal` ).

    DATA(view_sql) =  cont_sub->split_pane( requiredparentwidth = `600`
         )->layout_data( ns = `layout`
           )->splitter_layout_data( size = client->_bind_edit( ms_draft-size_sql )
           )->get_parent( )->get_parent( ).

    DATA(view_history) = cont_sub->split_pane( requiredparentwidth = `800`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-size_history )
           )->get_parent( )->get_parent(
         )->panel( headertext = `second pane`  ).

    DATA(view_output) = cont_main->split_pane( requiredparentwidth = `400` id = `default`
         )->layout_data( ns = `layout`
           )->splitter_layout_data(  size = client->_bind_edit( ms_draft-size_preview )
            )->get_parent( )->get_parent( ).

    view_sql->code_editor(
                type  = `sql`
*                    editable = mv_check_editable
                value = client->_bind_edit( ms_draft-input ) ).

    DATA(lv_sql) = ms_draft-input.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_sql  WITH ``.
    lv_sql = to_upper( lv_sql ).
    SPLIT lv_sql AT 'SELECTFROM' INTO DATA(lv_dummy) DATA(lv_tab).
    SPLIT lv_tab AT `FIELDS` INTO lv_tab lv_dummy.

    CREATE DATA ms_draft-t_tab TYPE STANDARD TABLE OF (lv_tab).

    FIELD-SYMBOLS <tab> TYPE table.
    ASSIGN  ms_draft-t_tab->* TO <tab>.

    SELECT * FROM (lv_tab) INTO CORRESPONDING FIELDS OF TABLE <tab>.



    DATA(tab) = view_output->table(
            items = client->_bind( <tab> )
       )->header_toolbar(
           )->overflow_toolbar(
               )->title( `(1) Data Preview`
               )->toolbar_spacer(
*                 )->input( description = `rows` value = client->_bind_edit( ms_draft-max_rows ) width = `10%`
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


    page->footer( )->overflow_toolbar(
        )->toolbar_spacer(
        )->button(
            text  = 'Run'
            press = client->_event( 'RUN' )
            type  = 'Emphasized'
            icon = 'sap-icon://upload-to-cloud' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
