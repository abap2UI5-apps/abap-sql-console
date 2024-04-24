CLASS z2ui5_sql_cl_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_value_map,
        pc TYPE string,
        ea TYPE string,
      END OF ty_value_map.

    TYPES:
      BEGIN OF ty_column_config,
        label             TYPE string,
        property          TYPE string,
        type              TYPE string,
        unit              TYPE string,
        delimiter         TYPE abap_bool,
        unit_property     TYPE string,
        width             TYPE string,
        scale             TYPE i,
        text_align        TYPE string,
        display_unit      TYPE string,
        true_value        TYPE string,
        false_value       TYPE string,
        template          TYPE string,
        input_format      TYPE string,
        wrap              TYPE abap_bool,
        auto_scale        TYPE abap_bool,
        timezone          TYPE string,
        timezone_property TYPE string,
        display_timezone  TYPE abap_bool,
        utc               TYPE abap_bool,
        value_map         TYPE ty_value_map,
      END OF ty_column_config.

    DATA: mt_column_config TYPE STANDARD TABLE OF ty_column_config WITH EMPTY KEY.
    DATA: mv_column_config TYPE string.


    TYPES:
      BEGIN OF ty_history_out,
        s_db  TYPE z2ui5_sql_cl_history_api=>ty_s_entry,
        time  TYPE string,
        date  TYPE string,
        selkz TYPE abap_bool,
      END OF ty_history_out.

    TYPES:
      BEGIN OF ty_s_preview,
        tab            TYPE REF TO data,
        tab_backup     TYPE REF TO data,
        cont_size      TYPE string,
        title          TYPE string,
        search_field   TYPE string,
        t_filter       TYPE z2ui5_cl_util=>ty_t_filter_multi,
        rtti_data      TYPE string,
        rtti_data_back TYPE string,
      END OF ty_s_preview.

    DATA:
      BEGIN OF ms_draft,
        s_preview         TYPE ty_s_preview,
        sql_input         TYPE string,
        sql_s_command     TYPE z2ui5_cl_util=>ty_s_sql_result,
        sql_max_rows      TYPE i,
        sql_cont_size     TYPE string,
        history_cont_size TYPE string,
        history_tab       TYPE STANDARD TABLE OF ty_history_out WITH EMPTY KEY,
        appwidthlimited   TYPE abap_bool,
      END OF ms_draft.

    TYPES:
      BEGIN OF ty_row,
        title    TYPE string,
        value    TYPE string,
        descr    TYPE string,
        icon     TYPE string,
        info     TYPE string,
        selected TYPE abap_bool,
      END OF ty_row.

    TYPES:
      BEGIN OF ty_sort,
        text     TYPE string,
        key      TYPE string,
        selected TYPE abap_bool,
      END OF ty_sort.

    DATA t_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY .
    DATA t_tab_sort TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .
    DATA t_tab_group TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .
    DATA t_tab_filter TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .


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
    METHODS preview_filter_search.
    METHODS preview_filter_range.
    METHODS history_on_clear_pressed.
    METHODS z2ui5_on_callback.
    METHODS z2ui5_on_event.
    METHODS sql_on_run.

    METHODS sql_view_display
      IMPORTING
        view_sql TYPE REF TO z2ui5_cl_xml_view.
    METHODS preview_view.
    METHODS history_view
      IMPORTING
        view_history TYPE REF TO z2ui5_cl_xml_view.
    METHODS preview_on_filter_clear.
    METHODS z2ui5_on_callback_pop_confirm
      IMPORTING
        io_popup TYPE REF TO z2ui5_cl_pop_to_confirm.
    METHODS history_on_load.
    METHODS history_db_save.
    METHODS z2ui5_on_init_set_app.
    METHODS preview_on_filter.

    METHODS result_display
      IMPORTING
        fw_table     TYPE string
        fo_result    TYPE REF TO data
        ft_fieldlist TYPE z2ui5_scl_cl_query_on_prem=>ty_fieldlist_table
        fw_title     TYPE string.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z2UI5_SQL_CL_APP_01 IMPLEMENTATION.


  METHOD history_db_read.

    CLEAR ms_draft-history_tab.
    LOOP AT z2ui5_sql_cl_history_api=>db_read_multi_by_user( ) REFERENCE INTO DATA(lr_history).

      INSERT VALUE #(
          s_db =  CORRESPONDING #( lr_history->* EXCEPT result_data )
          time = |{ lr_history->timestampl TIMESTAMP = ISO }|
          ) INTO TABLE ms_draft-history_tab.

    ENDLOOP.

    SORT ms_draft-history_tab BY s_db-timestampl DESCENDING.

    IF ms_draft-history_tab IS INITIAL.
      INSERT VALUE #( selkz = abap_true ) INTO TABLE ms_draft-history_tab.
    ENDIF.

  ENDMETHOD.


  METHOD history_db_save.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.

    FIELD-SYMBOLS <tab_back> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab_back>.

    LOOP AT ms_draft-history_tab REFERENCE INTO DATA(lr_hist) WHERE selkz = abap_true.
      EXIT.
    ENDLOOP.
    IF lr_hist IS NOT BOUND.
      CREATE DATA lr_hist.
    ENDIF.
    DATA(ls_preview) = ms_draft-s_preview.
    CLEAR ls_preview-tab.
    CLEAR ls_preview-tab_backup.

    ls_preview-rtti_data = z2ui5_cl_util=>xml_srtti_stringify( <tab> ).
    IF <tab_back> IS  ASSIGNED.
      ls_preview-rtti_data_back = z2ui5_cl_util=>xml_srtti_stringify( <tab_back> ).
    ENDIF.

    lr_hist->s_db-timestampl = z2ui5_cl_util=>time_get_timestampl( ).
    IF lr_hist->s_db-uuid IS INITIAL.
      lr_hist->s_db-uuid = z2ui5_cl_util=>uuid_get_c32( ).
    ENDIF.
    z2ui5_sql_cl_history_api=>db_create( VALUE #(
        sql_command = ms_draft-sql_input
        tabname     = ms_draft-sql_s_command-table
        timestampl  = lr_hist->s_db-timestampl
        uuid = lr_hist->s_db-uuid
        counter     = lines( <tab> )
        result_data = z2ui5_cl_util=>xml_srtti_stringify( ls_preview ) ) ).

    lr_hist->s_db-sql_command = ms_draft-sql_input.
    lr_hist->s_db-tabname = ms_draft-sql_s_command-table.
    lr_hist->s_db-counter = lines( <tab> ).

  ENDMETHOD.


  METHOD history_on_clear_pressed.

    IF ms_draft-history_tab IS INITIAL.
      client->message_box_display( `No history entries found. No action needed.` ).
      RETURN.
    ENDIF.
    ms_control-callback_pop_history_clear = client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory( `Delete all history entries from database?` ) ).

  ENDMETHOD.


  METHOD history_on_load.

    DATA(ls_arg) = ms_draft-history_tab[ selkz = abap_true ].
    DATA(ls_entry) = z2ui5_sql_cl_history_api=>db_read_by_id( ls_arg-s_db-uuid ).
    ms_draft-sql_input = ls_entry-sql_command.

    DATA lr_preview TYPE REF TO data.
    FIELD-SYMBOLS <tab2> TYPE any.
    ASSIGN ms_draft-s_preview-tab->* TO <tab2>.
    IF sy-subrc = 0.
      client->_bind_clear( `MS_DRAFT-S_PREVIEW-TAB->*` ).
      client->_bind_clear( `MT_COLUMN_CONFIG` ).                     "feng add for bug
      CLEAR ms_draft-s_preview-tab.
    ENDIF.

    IF ls_entry-result_data IS NOT INITIAL.
      lr_preview = z2ui5_cl_util=>xml_srtti_parse( rtti_data = ls_entry-result_data ).

      FIELD-SYMBOLS <any> TYPE any.
      ASSIGN lr_preview->('title') TO <any>.
      ms_draft-s_preview-title = <any>.
      ASSIGN lr_preview->('search_field') TO <any>.
      ms_draft-s_preview-search_field = <any>.
      ASSIGN lr_preview->('t_filter') TO <any>.
      ms_draft-s_preview-t_filter = <any>.

      FIELD-SYMBOLS <preview> TYPE data.
      ASSIGN lr_preview->* TO <preview>.

      FIELD-SYMBOLS <tab> TYPE any.
      DATA lr_data TYPE REF TO data.

      ASSIGN lr_preview->('rtti_data') TO <any>.
      lr_data = z2ui5_cl_util=>xml_srtti_parse( <any> ).
      ms_draft-s_preview-tab = z2ui5_cl_util=>conv_copy_ref_data( lr_data ).

      ASSIGN lr_preview->('rtti_data_back') TO <any>.
      lr_data = z2ui5_cl_util=>xml_srtti_parse( <any> ).
      ms_draft-s_preview-tab_backup = z2ui5_cl_util=>conv_copy_ref_data( lr_data ).

    ENDIF.

    client->view_model_update( ).
    preview_view( ).
    client->message_toast_display( `history entry successfully loaded` ).

  ENDMETHOD.


  METHOD history_view.

    view_history->list(
          items           = client->_bind_edit( ms_draft-history_tab )
          mode            = `SingleSelectMaster`
          selectionchange = client->_event( val = 'HISTORY_LOAD' )
           sticky              = 'ColumnHeaders,HeaderToolbar'
             )->header_toolbar(
             )->overflow_toolbar(
                 )->title( 'Query History'
                 )->toolbar_spacer(
                )->toolbar_spacer(
                 )->button( text = `New` press = client->_event( `HISTORY_CREATE` ) icon = `sap-icon://create`
                 )->button( text = `Clear` press = client->_event( `HISTORY_CLEAR` ) icon = `sap-icon://delete`
        )->get_parent( )->get_parent(
          )->standard_list_item(
              title       = '{S_DB/TABNAME} - {DATE} {TIME}'
              description = '{S_DB/SQL_COMMAND}'
              info        = '{S_DB/COUNTER}'
              selected    = `{SELKZ}`
         ).

  ENDMETHOD.


  METHOD preview_filter_range.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.

    LOOP AT ms_draft-s_preview-t_filter INTO DATA(ls_tab).

      DATA(lv_clause) = ls_tab-name && ` not in ls_tab-t_range`.
      DELETE <tab> WHERE (lv_clause).

    ENDLOOP.

  ENDMETHOD.


  METHOD preview_filter_search.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.

    IF ms_draft-s_preview-search_field IS NOT INITIAL.

      z2ui5_cl_util=>itab_filter_by_val(
        EXPORTING
          val = ms_draft-s_preview-search_field
        CHANGING
          tab = <tab> ).

    ENDIF.

  ENDMETHOD.


  METHOD preview_on_filter.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.
    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.
    <tab> = <tab2>.
    preview_filter_search( ).
    ms_draft-s_preview-title = `Number of Rows: ` && ` (` && z2ui5_cl_util=>c_trim( lines( <tab> ) ) && `)`.

    preview_view( ).
    history_db_save( ).

  ENDMETHOD.


  METHOD preview_on_filter_clear.

    CLEAR ms_draft-s_preview-search_field.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.
    <tab> = <tab2>.

    ms_draft-s_preview-title = `Number of Rows: ` && ` ` && z2ui5_cl_util=>c_trim( lines( <tab> ) ) && ``.

    preview_view( ).
    client->message_toast_display( 'all filter deleted' ).

  ENDMETHOD.


  METHOD preview_view.

    DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory( ).

    IF ms_draft-s_preview-tab IS BOUND.
      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN  ms_draft-s_preview-tab->* TO <tab>.

      DATA(tab) = lo_view_nested->ui_table(
                            id = `previewTab`
                            rows = client->_bind_local( <tab> )
                            editable = abap_false
                            alternaterowcolors = abap_true
                            showcolumnvisibilitymenu = abap_true
                            enableselectall = abap_false
                            selectionbehavior = `RowOnly`
                            visiblerowcountmode = `Interactive`
                            visiblerowcount = `7`
                            selectionmode = 'None' ).
      tab->ui_extension( )->overflow_toolbar( width = `100%`
                 )->title( client->_bind( ms_draft-s_preview-title )
                 )->toolbar_spacer(
                 )->input( width = `30%` value = client->_bind_edit( ms_draft-s_preview-search_field ) description = `All Column Search`
                    submit = client->_event( `PREVIEW_SEARCH` )
                 )->toolbar_spacer(
                 )->_z2ui5( )->spreadsheet_export( tableid = `previewTab` icon = 'sap-icon://excel-attachment' type = 'Emphasized'
                                                   columnconfig = client->_bind( val = mt_column_config
                                                   custom_filter = NEW z2ui5_cl_cc_spreadsheet( )
                                                   custom_mapper = z2ui5_cl_ajson_mapping=>create_lower_case( )
                                                 )
                  )->get_parent(
                 ).

      DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_struc( <tab> ).

      DATA(lo_columns) = tab->ui_columns( ).
      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
        lo_columns->ui_column( width = `auto`  sortproperty = `'` && lv_field-name && `'` filterproperty = `'` && lv_field-name && `'`
          )->text( text = lv_field-name )->ui_template( )->label( text = `{` && lv_field-name && `}` wrapping = abap_true ).
      ENDLOOP.

    ELSE.
      lo_view_nested->text( `data preview...`  ).
    ENDIF.

    client->nest_view_display( val = lo_view_nested->stringify( ) id = `preview`  method_insert = 'addItem'  ).

  ENDMETHOD.


  METHOD result_display.

    DATA : lo_descr_table TYPE REF TO cl_abap_tabledescr,
           lo_descr_line  TYPE REF TO cl_abap_structdescr,
           lt_comp        TYPE cl_abap_structdescr=>component_table,
           ls_comp        TYPE cl_abap_structdescr=>component.


    FIELD-SYMBOLS: <lft_data> TYPE ANY TABLE,
                   <fs_comp>  TYPE any.

    ASSIGN fo_result->* TO <lft_data>.

    lo_descr_table ?=
      cl_abap_typedescr=>describe_by_data_ref( fo_result ).
    lo_descr_line ?= lo_descr_table->get_table_line_type( ).

    lt_comp = lo_descr_line->get_components( ).

    LOOP AT lt_comp INTO ls_comp.

      ls_comp-name = ft_fieldlist[ sy-tabix ]-ref_field.

      MODIFY lt_comp FROM ls_comp.

      APPEND VALUE ty_column_config( label = ls_comp-name property = ls_comp-name type = `String` ) TO mt_column_config.

    ENDLOOP.

    DATA(lo_new_type) = cl_abap_structdescr=>create( lt_comp ).
    DATA(lo_new_tab) = cl_abap_tabledescr=>create(
                        p_line_type  = lo_new_type
                        p_table_kind = cl_abap_tabledescr=>tablekind_std
                        p_unique     = abap_false ).

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    CREATE DATA ms_draft-s_preview-tab TYPE HANDLE lo_new_tab.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.
    <tab> = <lft_data>.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    CREATE DATA ms_draft-s_preview-tab_backup TYPE HANDLE lo_new_tab.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.
    <tab2> = <tab>.

    client->_bind_clear( `MS_DRAFT-S_PREVIEW-TAB->*` ).
    client->_bind_clear( `MT_COLUMN_CONFIG` ).
    preview_view( ).

    ms_draft-s_preview-t_filter = z2ui5_cl_util=>filter_get_multi_by_data( <tab> ).
    ms_draft-s_preview-title = `Number of Rows: ` && ` ` && z2ui5_cl_util=>c_trim( lines( <tab2> ) ) && ``.

    history_db_save( ).
    client->view_model_update( ).


  ENDMETHOD.


  METHOD sql_db_read.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.

    SELECT FROM (ms_draft-sql_s_command-table) FIELDS *
        INTO CORRESPONDING FIELDS OF TABLE @<tab>
        UP TO @ms_draft-sql_max_rows ROWS.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.

    ms_draft-s_preview-t_filter = z2ui5_cl_util=>filter_get_multi_by_data( <tab> ).
    <tab2> = <tab>.
    ms_draft-s_preview-title = `Number of Rows: ` && ` ` && z2ui5_cl_util=>c_trim( lines( <tab2> ) ) && ``.

  ENDMETHOD.


  METHOD sql_on_run.

    z2ui5_scl_cl_query_on_prem=>sql_logic(
      EXPORTING
        query         = ms_draft-sql_input
        max_rows = ms_draft-sql_max_rows
     IMPORTING
        lw_from       = DATA(lw_from)
        lo_result     = DATA(lo_result)
        lw_query     = DATA(lw_query)
        lt_fieldlist2 = DATA(lt_fieldlist2)
    ).

    result_display(
        fw_table     = lw_from
        fo_result    = lo_result
        ft_fieldlist = lt_fieldlist2
        fw_title     = lw_query
    ).

  ENDMETHOD.


  METHOD sql_view_display.

    view_sql->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
         )->code_editor(
                type  = `sql`
                value = client->_bind_edit( ms_draft-sql_input ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.
    TRY.

        me->client = client.

        IF ms_control-check_initialized = abap_false.
          ms_control-check_initialized = abap_true.

          client->view_display( z2ui5_cl_xml_view=>factory(
            )->_z2ui5( )->timer(  client->_event( `START` )
              )->_generic( ns = `html` name = `script` )->_cc_plain_xml( z2ui5_cl_cc_spreadsheet=>get_js( )
              )->stringify( ) ).

          RETURN.
        ENDIF.

        IF client->get( )-check_on_navigated = abap_true.
          z2ui5_on_callback( ).
          RETURN.
        ENDIF.

        z2ui5_on_event(  ).

      CATCH cx_root INTO DATA(x).
        client->nav_app_call( z2ui5_cl_pop_error=>factory( x ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD z2ui5_on_callback.

    TRY.
        DATA(lo_popup_confirm) = CAST z2ui5_cl_pop_to_confirm( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        z2ui5_on_callback_pop_confirm( lo_popup_confirm ).
        RETURN.
      CATCH cx_root.
    ENDTRY.

    TRY.
        DATA(lo_popup_range) = CAST z2ui5_cl_pop_get_range_m( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        IF lo_popup_range->result( )-check_confirmed = abap_true.
          ms_draft-s_preview-t_filter = lo_popup_range->result( )-t_sql.
          preview_on_filter( ).
        ENDIF.
        RETURN.
      CATCH cx_root.
    ENDTRY.

    TRY.
        z2ui5_view_display( ).
        RETURN.
      CATCH cx_root.
    ENDTRY.

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


  METHOD z2ui5_on_event.

    z2ui5_sql_cl_history_api=>db_create_draft( client->get( )-s_draft-id ).

    CASE client->get( )-event.
      WHEN 'START'.
        z2ui5_on_init_set_app( ).

      WHEN `PREVIEW_FILTER`.
        client->nav_app_call( z2ui5_cl_pop_get_range_m=>factory( ms_draft-s_preview-t_filter ) ).

      WHEN 'PREVIEW_CLEAR_FILTER'.
        preview_on_filter_clear( ).

      WHEN 'PREVIEW_SEARCH'.
        preview_on_filter( ).

      WHEN 'RUN'.
        sql_on_run( ).

      WHEN 'HISTORY_CLEAR'.
        history_on_clear_pressed( ).

      WHEN 'HISTORY_CREATE'.
        INSERT VALUE #( selkz = abap_true ) INTO TABLE ms_draft-history_tab.
        client->view_model_update( ).

      WHEN 'HISTORY_LOAD'.
        history_on_load( ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD z2ui5_on_init_set_app  .

    ms_draft-sql_input = `Select * from T100`.
    ms_draft-history_cont_size = `30%`.
    ms_draft-sql_cont_size = `auto`.
    ms_draft-s_preview-cont_size = `auto`.
    ms_draft-sql_max_rows = 500.
    ms_draft-appwidthlimited = abap_true.
    z2ui5_view_display( ).

    history_db_read( ).

  ENDMETHOD.


  METHOD z2ui5_view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    DATA(page) = view->shell( appwidthlimited = client->_bind_edit( ms_draft-appwidthlimited ) )->page(
    title = 'ABAP SQL Console'
    navbuttonpress = client->_event( 'BACK' )
    shownavbutton = abap_true
            )->header_content(
                )->overflow_toolbar(
                )->label( `Max Rows`
                )->input( width = `15%` value = client->_bind_edit( ms_draft-sql_max_rows )
                  )->button(
            text  = 'Run'
            press = client->_event( 'RUN' )
            type  = 'Emphasized'
                )->toolbar_spacer(
                )->label( `Shell`
                )->switch( state = client->_bind_edit( ms_draft-appwidthlimited )
                )->link( text = 'Project on GitHub' target = '_blank' href = 'https://github.com/abap2ui5-apps/abap-sql-console'
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

    DATA(view_preview) = cont_main->split_pane( requiredparentwidth = `400`
         )->layout_data( ns = `layout`
           )->splitter_layout_data( size = client->_bind_edit( ms_draft-s_preview-cont_size )
            )->get_parent( )->get_parent(
            )->vbox( id = `preview` ).

    preview_view(  ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
