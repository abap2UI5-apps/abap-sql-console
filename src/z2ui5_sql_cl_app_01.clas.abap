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
      END OF ty_row .
    TYPES:
      BEGIN OF ty_sort,
        text     TYPE string,
        key      TYPE string,
        selected TYPE abap_bool,
      END OF ty_sort .

    DATA:
      t_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY .
    DATA:
      t_tab_sort TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .
    DATA:
      t_tab_group TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .
    DATA:
      t_tab_filter TYPE STANDARD TABLE OF ty_sort WITH EMPTY KEY .

* Global types
    TYPES : BEGIN OF ty_fieldlist,
              field     TYPE string,
              ref_table TYPE string,
              ref_field TYPE string,
            END OF ty_fieldlist,
            ty_fieldlist_table TYPE STANDARD TABLE OF ty_fieldlist.

* Constants
    CONSTANTS : c_ddic_col1          TYPE mtreeitm-item_name
                            VALUE 'col1',                   "#EC NOTEXT
                c_ddic_col2          TYPE mtreeitm-item_name
                            VALUE 'col2',                   "#EC NOTEXT
*                c_visibility_all       TYPE ztoad-visibility VALUE '2',
*                c_visibility_shared    TYPE ztoad-visibility VALUE '1',
*                c_visibility_my        TYPE ztoad-visibility VALUE '0',
*                c_nodekey_repo_my      TYPE mtreesnode-node_key VALUE 'MY',
*                c_nodekey_repo_shared  TYPE mtreesnode-node_key
*                                      VALUE 'SHARED',
*                c_nodekey_repo_history TYPE mtreesnode-node_key
*                                       VALUE 'HISTO',
                c_line_max           TYPE i VALUE 255,
                c_msg_success        TYPE c VALUE 'S',
                c_msg_error          TYPE c VALUE 'E',
                c_vers_active        TYPE c VALUE 'A',
                c_ddic_dtelm         TYPE c VALUE 'E',
                c_native_command     TYPE string VALUE 'NATIVE',
                c_query_max_exec     TYPE i VALUE 1000,

                c_xmlnode_root       TYPE string VALUE 'root', "#EC NOTEXT
                c_xmlnode_file       TYPE string VALUE 'query', "#EC NOTEXT
                c_xmlattr_visibility TYPE string VALUE 'visibility', "#EC NOTEXT
                c_xmlattr_text       TYPE string VALUE 'description'. "#EC NOTEXT



  PROTECTED SECTION.

    DATA:
      BEGIN OF ms_control,
        check_initialized          TYPE abap_bool,
        callback_pop_session_load  TYPE string,
        callback_pop_history_clear TYPE string,
      END OF ms_control.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS z2ui5_view_display.
    METHODS z2ui5_view_settings_popup.
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
*      IMPORTING
*        view_output TYPE REF TO z2ui5_cl_xml_view.
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
    METHODS preview_on_filter.

    METHODS editor_get_query
      CHANGING fw_query TYPE string.

    METHODS query_parse
      IMPORTING
        fw_query     TYPE string
      CHANGING
        fw_select    TYPE string
        fw_from      TYPE string
        fw_where     TYPE string
        fw_union     TYPE string
        fw_rows      TYPE i
        fw_noauth    TYPE c
        fw_newsyntax TYPE c
        fw_error     TYPE c.

    METHODS query_generate
      IMPORTING
        fw_select    TYPE string
        fw_from      TYPE string
        fw_where     TYPE string
        fw_display   TYPE c
        fw_newsyntax TYPE c
      CHANGING
        fw_program   TYPE sy-repid
        fw_rows      TYPE i
        ft_fieldlist TYPE ty_fieldlist_table
        fw_count     TYPE c.

    METHODS result_display
      IMPORTING
        fw_table     TYPE string
        fo_result    TYPE REF TO data
        ft_fieldlist TYPE ty_fieldlist_table
        fw_title     TYPE string.

  PRIVATE SECTION.

ENDCLASS.



CLASS Z2UI5_SQL_CL_APP_01 IMPLEMENTATION.


  METHOD editor_get_query.

    DATA : lt_query TYPE string_table,
           lv_query TYPE string.

    SPLIT fw_query AT cl_abap_char_utilities=>newline INTO TABLE lt_query.

    CLEAR fw_query.
    LOOP AT lt_query INTO lv_query.
      CONDENSE lv_query.
      SHIFT lv_query LEFT DELETING LEADING space.
      CONCATENATE fw_query lv_query INTO fw_query SEPARATED BY space.
    ENDLOOP.
    CONDENSE lv_query.

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
    ms_control-callback_pop_history_clear = client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory( `Delete all history entries from database?` ) ).

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
      CLEAR ms_draft-s_preview-tab.
    ENDIF.

*    create data ls_preview like ms_draft-s_preview.
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
*    z2ui5_cl_util_func=>rtti_xml_set_to_data(
*        EXPORTING
*            rtti_data = <preview>-rtti_data_back
*        IMPORTING
*            e_data    = <preview>-tab_backup
*        ).
*
*


*    z2ui5_view_display( ).
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
*              title       = `{ path: 'DATE' ,  type: 'sap.ui.model.type.String' , formatter : function(value){ return '100'; } }`
*              title       = `{ path: 'DATE' ,  type: 'sap.ui.model.type.String' , formatter : function(value){ return '100'; } }`
              description = '{S_DB/SQL_COMMAND}'
*              icon        = '{ICON}'
              info        = '{S_DB/COUNTER}'
*              press       = client->_event( val = 'HISTORY_LOAD' t_arg = VALUE #( ( `{S_DB/UUID}`) ) )
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


*    DATA(lr_tab) = z2ui5_cl_util=>conv_copy_ref_data( ms_draft-s_preview-tab_backup ).
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.
    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.
    <tab> = <tab2>.
*    ms_draft-s_preview-tab =
*    preview_filter_range( ).
    preview_filter_search( ).
    ms_draft-s_preview-title = `Number of Rows: ` && ` (` && z2ui5_cl_util=>c_trim( lines( <tab> ) ) && `)`.

*    client->_bind_clear( `MS_DRAFT-S_PREVIEW-TAB->*` ).
*    client->_bind( <tab> ).
    preview_view( ).
*    client->bind_update( <tab> ).
*    client->nest_view_model_update( ).
    history_db_save( ).
*    client->message_toast_display( `Search field filter updated` ).

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

*    client->view_model_update( ).
    client->message_toast_display( 'all filter deleted' ).

  ENDMETHOD.


  METHOD preview_view.

    DATA(lo_view_nested) = z2ui5_cl_xml_view=>factory( ).

    IF ms_draft-s_preview-tab IS BOUND.
      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN  ms_draft-s_preview-tab->* TO <tab>.

*      DATA(tab) = lo_view_nested->table(
*              items = client->_bind_local( <tab> )
*              growing = abap_true
*              growingscrolltoload = abap_true
*              fixedlayout = abap_false
*              growingthreshold = `100`
*              sticky              = 'ColumnHeaders,HeaderToolbar'
*         )->header_toolbar(
*             )->overflow_toolbar( width = `100%`
*                 )->title( client->_bind( ms_draft-s_preview-title )
*                 )->toolbar_spacer(
*                 )->input( width = `30%` value = client->_bind_edit( ms_draft-s_preview-search_field ) description = `All Column Search`  submit = client->_event( `PREVIEW_SEARCH` )
*                 )->toolbar_spacer(
*                 )->button( icon = `sap-icon://action-settings` tooltip  = `Settings` press = client->_event( `VIEW_SETTINGS_DIALOG` )
**                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
**                 )->button( text = `Clear Filter` press = client->_event( `PREVIEW_CLEAR_FILTER` ) icon = `sap-icon://delete`
*        )->get_parent( )->get_parent( ).
*
*      DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_struc( <tab> ).
*
*      DATA(lo_columns) = tab->columns( ).
*      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
*        lo_columns->column( width = `auto` )->label( text = lv_field-name wrapping = abap_true ).
*      ENDLOOP.
*
*      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
*      LOOP AT lt_fields INTO lv_field FROM 1.
*        lo_cells->text( `{` && lv_field-name && `}` ).
*      ENDLOOP.



    DATA(tab) = lo_view_nested->ui_table(
                          rows = client->_bind_local( <tab> )
                          editable = abap_false
                          alternaterowcolors = abap_true
                          showcolumnvisibilitymenu = abap_true
                          enableselectall = abap_false
                          selectionbehavior = `RowOnly`
                          visiblerowcountmode = `Interactive`
*                          visiblerowcountmode = `Auto`
                          visiblerowcount = `7`
*                          minautorowcount = `7`
                          selectionmode = 'None' ).
      tab->ui_extension( )->overflow_toolbar( width = `100%`
                 )->title( client->_bind( ms_draft-s_preview-title )
                 )->toolbar_spacer(
                 )->input( width = `30%` value = client->_bind_edit( ms_draft-s_preview-search_field ) description = `All Column Search`
                    submit = client->_event( `PREVIEW_SEARCH` )
                 )->toolbar_spacer(
*                 )->button( icon = `sap-icon://action-settings` tooltip  = `Settings` press = client->_event( `VIEW_SETTINGS_DIALOG` )
                 ).

      DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_struc( <tab> ).

      DATA(lo_columns) = tab->ui_columns( ).
      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
        lo_columns->ui_column( width = `auto`  sortproperty = `'` && lv_field-name && `'` filterproperty = `'` && lv_field-name && `'`
          )->text( text = lv_field-name )->ui_template( )->label( text = `{` && lv_field-name && `}` wrapping = abap_true ).
      ENDLOOP.

*      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
*      LOOP AT lt_fields INTO lv_field FROM 1.
*        lo_cells->text( `{` && lv_field-name && `}` ).
*      ENDLOOP.



    ELSE.
      lo_view_nested->text( `data preview...`  ).
    ENDIF.

    client->nest_view_display( val = lo_view_nested->stringify( ) id = `preview`  method_insert = 'addItem'  ).


*    IF ms_draft-s_preview-tab IS BOUND.
*      FIELD-SYMBOLS <tab> TYPE table.
*      ASSIGN  ms_draft-s_preview-tab->* TO <tab>.
*
*      DATA(tab) = view_output->table(
*              items = client->_bind( <tab> )
*              growing = abap_true
*              growingscrolltoload = abap_true
*              growingthreshold = `100`
*              sticky              = 'ColumnHeaders,HeaderToolbar'
*         )->header_toolbar(
*             )->overflow_toolbar(
*                 )->title( client->_bind( ms_draft-s_preview-title )
*                 )->toolbar_spacer(
*                )->input( width = `30%` value = client->_bind_edit( ms_draft-s_preview-search_field ) description = `All Column Search`  submit = client->_event( `PREVIEW_SEARCH` )
*                 )->toolbar_spacer(
*                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
*                 )->button( text = `Clear Filter` press = client->_event( `PREVIEW_CLEAR_FILTER` ) icon = `sap-icon://delete`
*        )->get_parent( )->get_parent( ).
*
*      DATA(lt_fields) = z2ui5_cl_util_func=>rtti_get_t_comp_by_data( <tab> ).
*
*      DATA(lo_columns) = tab->columns( ).
*      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
*        lo_columns->column( )->text( lv_field-name ).
*      ENDLOOP.
*
*      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
*      LOOP AT lt_fields INTO lv_field FROM 1.
*        lo_cells->text( `{` && lv_field-name && `}` ).
*      ENDLOOP.
*
*    ELSE.
*      view_output->panel( headertext = `data preview...`  ).
*    ENDIF.

  ENDMETHOD.


  METHOD query_generate.

    DATA : lt_code_string TYPE TABLE OF string,
           lt_split       TYPE TABLE OF string,
           lw_string      TYPE string,
           lw_string2     TYPE string,
           BEGIN OF ls_table_alias,
             table(50) TYPE c,
             alias(50) TYPE c,
           END OF ls_table_alias,
           lt_table_alias      LIKE TABLE OF ls_table_alias,
           lw_select           TYPE string,
           lw_from             TYPE string,
           lw_index            TYPE i,
           lw_select_distinct  TYPE c,
           lw_select_length    TYPE i,
           lw_char_10(10)      TYPE c,
           lw_field_number(6)  TYPE n,
           lw_current_line     TYPE i,
           lw_current_length   TYPE i,
           lw_struct_line      TYPE string,
           lw_struct_line_type TYPE string,
           lw_select_table     TYPE string,
           lw_select_field     TYPE string,
           lw_dd03l_fieldname  TYPE dd03l-fieldname,
           lw_position_dummy   TYPE dd03l-position,
           lw_mess(255),
           lw_line             TYPE i,
           lw_word(30),
           ls_fieldlist        TYPE ty_fieldlist,
           lw_strlen_string    TYPE string,
           lw_explicit         TYPE string.

    DATA : cw_length TYPE i,
           cw_offset TYPE i,
           ls_find   TYPE match_result.

    DEFINE c.
      lw_strlen_string = &1.

      cw_length = strlen( lw_strlen_string ).
      cw_offset = 0.
      DO.
        IF cw_length LE c_line_max.
          APPEND lw_strlen_string+cw_offset(cw_length) TO lt_code_string.
          EXIT. "exit do
        ELSE.
          FIND ALL OCCURRENCES OF REGEX '\s' "search space
               IN SECTION OFFSET cw_offset LENGTH c_line_max
               OF lw_strlen_string RESULTS ls_find.
          IF sy-subrc NE 0.
            APPEND lw_strlen_string+cw_offset(c_line_max) TO lt_code_string.
            cw_length = cw_length - c_line_max.
            cw_offset = cw_offset + c_line_max.
          ELSE.
            ls_find-length = ls_find-offset - cw_offset.
            APPEND lw_strlen_string+cw_offset(ls_find-length) TO lt_code_string.
            cw_length = cw_length + cw_offset - ls_find-offset - 1.
            cw_offset = ls_find-offset + 1.
          ENDIF.
        ENDIF.
      ENDDO.

    END-OF-DEFINITION.

    CLEAR : lw_select_distinct.",
*            fw_count.

* Write Header
    c 'PROGRAM SUBPOOL.'.
    c '** GENERATED PROGRAM * DO NOT CHANGE IT **'.
    c 'TYPE-POOLS: slis.'.                                  "#EC NOTEXT
    c ''.

    lw_select = fw_select.
    TRANSLATE lw_select TO UPPER CASE.

    lw_from = fw_from.
    TRANSLATE lw_from TO UPPER CASE.

* Search special term "single" or "distinct"
    lw_select_length = strlen( lw_select ).
    IF lw_select_length GE 7.
      lw_char_10 = lw_select(7).
      IF lw_char_10 = 'SINGLE'.
* Force rows number = 1 for select single
        fw_rows = 1.
        lw_select = lw_select+7.
        lw_select_length = lw_select_length - 7.
      ENDIF.
    ENDIF.
    IF lw_select_length GE 9.
      lw_char_10 = lw_select(9).
      IF lw_char_10 = 'DISTINCT'.
        lw_select_distinct = abap_true.
        lw_select = lw_select+9.
        lw_select_length = lw_select_length - 9.
      ENDIF.
    ENDIF.

* Search for special syntax "count( * )"
    IF lw_select = 'COUNT( * )'.
      fw_count = abap_true.
    ENDIF.

* Create alias table mapping
    SPLIT lw_from AT space INTO TABLE lt_split.
    LOOP AT lt_split INTO lw_string.
      IF lw_string IS INITIAL OR lw_string CO space.
        DELETE lt_split.
      ENDIF.
    ENDLOOP.
    DO.
      READ TABLE lt_split TRANSPORTING NO FIELDS WITH KEY table_line = 'AS'.
      IF sy-subrc NE 0.
        EXIT. "exit do
      ENDIF.
      lw_index = sy-tabix - 1.
      READ TABLE lt_split INTO lw_string INDEX lw_index.
      ls_table_alias-table = lw_string.
      DELETE lt_split INDEX lw_index. "delete table field
      DELETE lt_split INDEX lw_index. "delete keywork AS
      READ TABLE lt_split INTO lw_string INDEX lw_index.
      ls_table_alias-alias = lw_string.
      DELETE lt_split INDEX lw_index. "delete alias field
      APPEND ls_table_alias TO lt_table_alias.
    ENDDO.
* If no alias table found, create just an entry for "*"
    IF lt_table_alias[] IS INITIAL.
      READ TABLE lt_split INTO lw_string INDEX 1.
      ls_table_alias-table = lw_string.
      ls_table_alias-alias = '*'.
      APPEND ls_table_alias TO lt_table_alias.
    ENDIF.
    SORT lt_table_alias BY alias.

* Write Data declaration
    c '***************************************'.            "#EC NOTEXT
    c '*      Begin of data declaration      *'.            "#EC NOTEXT
    c '*   Used to store lines of the query  *'.            "#EC NOTEXT
    c '***************************************'.            "#EC NOTEXT
    c 'DATA: BEGIN OF s_result'.                            "#EC NOTEXT
    lw_field_number = 1.

    lw_string = lw_select.
    IF fw_newsyntax = abap_true.
      TRANSLATE lw_string USING ', '.
      CONDENSE lw_string.
    ENDIF.
    SPLIT lw_string AT space INTO TABLE lt_split.

    LOOP AT lt_split INTO lw_string.
      lw_current_line = sy-tabix.
      IF lw_string IS INITIAL OR lw_string CO space.
        CONTINUE.
      ENDIF.
      IF lw_string = 'AS'.
        DELETE lt_split INDEX lw_current_line. "delete AS
        DELETE lt_split INDEX lw_current_line. "delete the alias name
        CONTINUE.
      ENDIF.
      lw_current_length = strlen( lw_string ).

      CLEAR ls_fieldlist.
      ls_fieldlist-ref_field = lw_string.

* Manage new syntax "Case"
      IF fw_newsyntax = abap_true AND lw_string = 'CASE'.
        lw_index = lw_current_line.
        DO.
          lw_index = lw_index + 1.
          READ TABLE lt_split INTO lw_string INDEX lw_index.
          IF sy-subrc NE 0.
            MESSAGE 'Incorrect syntax in Case statement'(m62)
                     TYPE c_msg_success DISPLAY LIKE c_msg_error.
            RETURN.
          ENDIF.
          IF lw_string = 'END'.
            lw_index = lw_index + 1.
            READ TABLE lt_split INTO lw_string INDEX lw_index.
            IF lw_string NE 'AS'.
              lw_index = lw_index - 1.
              CONTINUE.
            ENDIF.
            lw_index = lw_index + 1.
            READ TABLE lt_split INTO lw_string INDEX lw_index.

            CLEAR ls_fieldlist.
            CONCATENATE 'F' lw_field_number INTO ls_fieldlist-field.
            CONCATENATE ',' ls_fieldlist-field INTO lw_struct_line.
            CONCATENATE lw_struct_line 'TYPE string'        "#EC NOTEXT
                        INTO lw_struct_line SEPARATED BY space.
            c lw_struct_line.
            ls_fieldlist-ref_table = ''.
            ls_fieldlist-ref_field = lw_string.
            APPEND ls_fieldlist TO ft_fieldlist.
            lw_field_number = lw_field_number + 1.

            lw_index = lw_index - lw_current_line + 1.
            DO lw_index TIMES.
              DELETE lt_split INDEX lw_current_line. "delete the case element
            ENDDO.
            EXIT.
          ENDIF.
        ENDDO.
        CONTINUE.
      ENDIF.

* Manage "Count"
      IF lw_current_length GE 6.
        lw_char_10 = lw_string(6).
      ELSE.
        CLEAR lw_char_10.
      ENDIF.
      IF lw_char_10 = 'COUNT('.
        CONCATENATE 'F' lw_field_number INTO ls_fieldlist-field.
        CONCATENATE ',' ls_fieldlist-field INTO lw_struct_line.

        lw_index = lw_current_line + 1.
        DO.
          SEARCH lw_string FOR ')'.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
* If there is space in the "count()", delete next lines
            READ TABLE lt_split INTO lw_string INDEX lw_index.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            CONCATENATE ls_fieldlist-ref_field lw_string
                        INTO ls_fieldlist-ref_field SEPARATED BY space.
            DELETE lt_split INDEX lw_index.
          ENDIF.
        ENDDO.
        CONCATENATE lw_struct_line 'TYPE i'                 "#EC NOTEXT
                    INTO lw_struct_line SEPARATED BY space.
        c lw_struct_line.
        APPEND ls_fieldlist TO ft_fieldlist.
        lw_field_number = lw_field_number + 1.
        CONTINUE.
      ENDIF.

* Manage Agregate AVG
      IF lw_current_length GE 4.
        lw_char_10 = lw_string(4).
      ELSE.
        CLEAR lw_char_10.
      ENDIF.
      IF lw_char_10 = 'AVG('.
        CONCATENATE 'F' lw_field_number INTO ls_fieldlist-field.
        CONCATENATE ',' ls_fieldlist-field INTO lw_struct_line.

        lw_index = lw_current_line + 1.
        DO.
          SEARCH lw_string FOR ')'.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
* If there is space in the agregate, delete next lines
            READ TABLE lt_split INTO lw_string INDEX lw_index.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            CONCATENATE ls_fieldlist-ref_field lw_string
                        INTO ls_fieldlist-ref_field SEPARATED BY space.
            DELETE lt_split INDEX lw_index.
          ENDIF.
        ENDDO.
        CONCATENATE lw_struct_line 'TYPE f'                 "#EC NOTEXT
                    INTO lw_struct_line SEPARATED BY space.
        c lw_struct_line.
        APPEND ls_fieldlist TO ft_fieldlist.
        lw_field_number = lw_field_number + 1.
        CONTINUE.
      ENDIF.

* Manage agregate SUM, MAX, MIN
      IF lw_current_length GE 4.
        lw_char_10 = lw_string(4).
      ELSE.
        CLEAR lw_char_10.
      ENDIF.
      IF lw_char_10 = 'SUM(' OR lw_char_10 = 'MAX('
      OR lw_char_10 = 'MIN('.
        CLEAR lw_string2.
        lw_index = lw_current_line + 1.
        DO.
          SEARCH lw_string FOR ')'.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
* Search name of the field in next lines.
            READ TABLE lt_split INTO lw_string INDEX lw_index.
            IF sy-subrc NE 0.
              EXIT.
            ENDIF.
            CONCATENATE ls_fieldlist-ref_field lw_string
                        INTO ls_fieldlist-ref_field SEPARATED BY space.
            IF lw_string2 IS INITIAL.
              lw_string2 = lw_string.
            ENDIF.
* Delete lines of agregage in field table
            DELETE lt_split INDEX lw_index.
          ENDIF.
        ENDDO.
        lw_string = lw_string2.
      ENDIF.

* Now lw_string contain a field name.
* We have to find the field description
      SPLIT lw_string AT '~' INTO lw_select_table lw_select_field.
      IF lw_select_field IS INITIAL.
        lw_select_field = lw_select_table.
        lw_select_table = '*'.
      ENDIF.
* Search if alias table used
      CLEAR ls_table_alias.
      READ TABLE lt_table_alias INTO ls_table_alias
                 WITH KEY alias = lw_select_table           "#EC WARNOK
                 BINARY SEARCH.
      IF sy-subrc = 0.
        lw_select_table = ls_table_alias-table.
      ENDIF.
      ls_fieldlist-ref_table = lw_select_table.
      IF lw_string = '*' OR lw_select_field = '*'. " expansion table~*
        CLEAR lw_explicit.
        SELECT fieldname position
        INTO   (lw_dd03l_fieldname,lw_position_dummy)
        FROM   dd03l
        WHERE  tabname    = lw_select_table
*        AND    fieldname <> 'MANDT'
        AND    as4local   = c_vers_active
        AND    as4vers    = space
        AND (  comptype   = c_ddic_dtelm
            OR comptype   = space )
        ORDER BY position.

          lw_select_field = lw_dd03l_fieldname.

          CONCATENATE 'F' lw_field_number INTO ls_fieldlist-field.
          ls_fieldlist-ref_field = lw_select_field.
          APPEND ls_fieldlist TO ft_fieldlist.
          CONCATENATE ',' ls_fieldlist-field INTO lw_struct_line.

          CONCATENATE lw_select_table '-' lw_select_field
                      INTO lw_struct_line_type.
          CONCATENATE lw_struct_line 'TYPE' lw_struct_line_type
                      INTO lw_struct_line
                      SEPARATED BY space.
          c lw_struct_line.
          lw_field_number = lw_field_number + 1.
* Explicit list of fields instead of *
* Generate longer query but mandatory in case of T1~* or MARA~*
* Required also in some special cases, for example if table use include
          IF ls_table_alias-alias = space OR ls_table_alias-alias = '*'.
            CONCATENATE lw_explicit lw_select_table
                        INTO lw_explicit SEPARATED BY space.
          ELSE.
            CONCATENATE lw_explicit ls_table_alias-alias
                        INTO lw_explicit SEPARATED BY space.
          ENDIF.
          CONCATENATE lw_explicit '~' lw_select_field INTO lw_explicit.
        ENDSELECT.
        IF sy-subrc NE 0.
          MESSAGE e701(1r) WITH lw_select_table. "table does not exist
        ENDIF.
        IF NOT lw_explicit IS INITIAL.
          REPLACE FIRST OCCURRENCE OF lw_string
                  IN lw_select WITH lw_explicit.
        ENDIF.

      ELSE. "Simple field
        CONCATENATE 'F' lw_field_number INTO ls_fieldlist-field.
        ls_fieldlist-ref_field = lw_select_field.
        APPEND ls_fieldlist TO ft_fieldlist.

        CONCATENATE ',' ls_fieldlist-field INTO lw_struct_line.

        CONCATENATE lw_select_table '-' lw_select_field
                    INTO lw_struct_line_type.
        CONCATENATE lw_struct_line 'TYPE' lw_struct_line_type
                    INTO lw_struct_line
                    SEPARATED BY space.
        c lw_struct_line.
        lw_field_number = lw_field_number + 1.
      ENDIF.
    ENDLOOP.

* Add a count field
    CLEAR ls_fieldlist.
*    ls_fieldlist-field = 'COUNT'.
*    ls_fieldlist-ref_table = ''.
*    ls_fieldlist-ref_field = 'Count'.                       "#EC NOTEXT
    APPEND ls_fieldlist TO ft_fieldlist.
*    c ', COUNT type i'.                                     "#EC NOTEXT

* End of data definition
    c ', END OF s_result'.                                  "#EC NOTEXT
    c ', t_result like table of s_result'.                  "#EC NOTEXT
    c ', w_timestart type timestampl'.                      "#EC NOTEXT
    c ', w_timeend type timestampl.'.                       "#EC NOTEXT

* Write the dynamic subroutine that run the SELECT
    c 'FORM run_sql CHANGING fo_result TYPE REF TO data'.   "#EC NOTEXT
    c '                      fw_time type p'.               "#EC NOTEXT
    c '                      fw_count type i.'.             "#EC NOTEXT
    c 'field-symbols <fs_result> like s_result.'.           "#EC NOTEXT
    c '***************************************'.            "#EC NOTEXT
    c '*            Begin of query           *'.            "#EC NOTEXT
    c '***************************************'.            "#EC NOTEXT
    c 'get TIME STAMP FIELD w_timestart.'.                  "#EC NOTEXT
    IF fw_count = abap_true.
      CONCATENATE 'SELECT SINGLE' lw_select                 "#EC NOTEXT
                  INTO lw_select SEPARATED BY space.
      c lw_select.
      IF fw_newsyntax = abap_true.
        c 'INTO @s_result-f000001'.                         "#EC NOTEXT
      ELSE.
        c 'INTO s_result-f000001'.                          "#EC NOTEXT
      ENDIF.
    ELSE.
      IF lw_select_distinct NE space.
        CONCATENATE 'SELECT DISTINCT' lw_select             "#EC NOTEXT
                    INTO lw_select SEPARATED BY space.
      ELSE.
        CONCATENATE 'SELECT' lw_select                      "#EC NOTEXT
                    INTO lw_select SEPARATED BY space.
      ENDIF.
      c lw_select.
      IF fw_newsyntax = abap_true.
        c 'INTO TABLE @t_result'.                           "#EC NOTEXT
      ELSE.
        c 'INTO TABLE t_result'.                            "#EC NOTEXT
      ENDIF.

* Add UP TO xxx ROWS
      IF NOT fw_rows IS INITIAL.
        c 'UP TO'.                                          "#EC NOTEXT
        c fw_rows.
        c 'ROWS'.                                           "#EC NOTEXT
      ENDIF.
    ENDIF.

    c 'FROM'.                                               "#EC NOTEXT
    c lw_from.

* Where, group by, having, order by
    IF NOT fw_where IS INITIAL.
      c fw_where.
    ENDIF.
    c '.'.

* Display query execution time
    c 'get TIME STAMP FIELD w_timeend.'.                    "#EC NOTEXT
    c 'fw_time = w_timeend - w_timestart.'.                 "#EC NOTEXT
    c 'fw_count = sy-dbcnt.'.                               "#EC NOTEXT

* If select count( * ), display number of results
    IF fw_count NE space.
      c 'MESSAGE i753(TG) WITH s_result-f000001.'.          "#EC NOTEXT
    ENDIF.
*    c 'loop at t_result assigning <fs_result>.'.            "#EC NOTEXT
*    c ' <fs_result>-count = 1.'.                            "#EC NOTEXT
*    c 'endloop.'.                                           "#EC NOTEXT
    c 'GET REFERENCE OF t_result INTO fo_result.'.          "#EC NOTEXT
    c 'ENDFORM.'.                                           "#EC NOTEXT
    CLEAR : lw_line,
            lw_word,
            lw_mess.
    SYNTAX-CHECK FOR lt_code_string PROGRAM sy-repid
                 MESSAGE lw_mess LINE lw_line WORD lw_word.
    IF sy-subrc NE 0 AND fw_display = space.
      MESSAGE lw_mess TYPE c_msg_success DISPLAY LIKE c_msg_error.
      CLEAR fw_program.
      RETURN.
    ENDIF.

    IF fw_display = space.
      GENERATE SUBROUTINE POOL lt_code_string NAME fw_program.
    ELSE.
      IF lw_mess IS NOT INITIAL.
        lw_explicit = lw_line.
        CONCATENATE lw_mess '(line'(m28) lw_explicit ',word'(m29)
                    lw_word ')'(m30)
                    INTO lw_mess SEPARATED BY space.
        MESSAGE lw_mess TYPE c_msg_success DISPLAY LIKE c_msg_error.
      ENDIF.
      EDITOR-CALL FOR lt_code_string DISPLAY-MODE
                  TITLE 'Generated code for current query'(t01).
    ENDIF.


  ENDMETHOD.


  METHOD query_parse.

    DATA : ls_find_select TYPE match_result,
           ls_find_from   TYPE match_result,
           ls_find_where  TYPE match_result,
           ls_sub         LIKE LINE OF ls_find_select-submatches,
           lw_offset      TYPE i,
           lw_length      TYPE i,
           lw_query       TYPE string,
           lo_regex       TYPE REF TO cl_abap_regex,
           lt_split       TYPE TABLE OF string,
           lw_string      TYPE string,
           lw_tabix       TYPE i,
           lw_table       TYPE tabname.

*    CLEAR : fw_select,
*            fw_from,
*            fw_where,
*            fw_rows,
*            fw_union,
*            fw_noauth,
*            fw_newsyntax.

    lw_query = fw_query.

* Search union
    FIND FIRST OCCURRENCE OF ' UNION SELECT ' IN lw_query
         RESULTS ls_find_select IGNORING CASE.
    IF sy-subrc = 0.
      lw_offset = ls_find_select-offset + 7.
      fw_union = lw_query+lw_offset.
      lw_query = lw_query(ls_find_select-offset).
    ENDIF.

* Search UP TO xxx ROWS.
* Catch the number of rows, delete command in query
    CREATE OBJECT lo_regex
      EXPORTING
        pattern     = 'UP TO ([0-9]+) ROWS'
        ignore_case = abap_true.
    FIND FIRST OCCURRENCE OF REGEX lo_regex
         IN lw_query RESULTS ls_find_select.
    IF sy-subrc = 0.
      READ TABLE ls_find_select-submatches INTO ls_sub INDEX 1.
      IF sy-subrc = 0.
        fw_rows = lw_query+ls_sub-offset(ls_sub-length).
      ENDIF.
      REPLACE FIRST OCCURRENCE OF REGEX lo_regex IN lw_query WITH ''.
    ELSE.
* Set default number of rows - sent in method
*      fw_rows = fw_rows.
    ENDIF.

* Remove unused INTO (CORRESPONDING FIELDS OF)(TABLE)
* Detect new syntax in internal table name
    CONCATENATE '(INTO|APPENDING)( TABLE'
                '| CORRESPONDING FIELDS OF TABLE |'
                'CORRESPONDING FIELDS OF | )(\S*)'
                INTO lw_string SEPARATED BY space.
    CREATE OBJECT lo_regex
      EXPORTING
        pattern     = lw_string
        ignore_case = abap_true.
    FIND FIRST OCCURRENCE OF REGEX lo_regex
         IN lw_query RESULTS ls_find_select.
    IF sy-subrc = 0.
      IF ls_find_select-length NE 0
      AND fw_query+ls_find_select-offset(ls_find_select-length) CS '@'.
        fw_newsyntax = abap_true.
      ENDIF.
      REPLACE FIRST OCCURRENCE OF REGEX lo_regex IN lw_query WITH ''.
    ENDIF.

* Search SELECT
    FIND FIRST OCCURRENCE OF 'SELECT ' IN lw_query
         RESULTS ls_find_select IGNORING CASE.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

* Search FROM
    FIND FIRST OCCURRENCE OF ' FROM '
         IN SECTION OFFSET ls_find_select-offset OF lw_query
         RESULTS ls_find_from IGNORING CASE.
    IF sy-subrc NE 0.
      fw_error = abap_true.
      RETURN.
    ENDIF.

* Search WHERE / GROUP BY / HAVING / ORDER BY
    FIND FIRST OCCURRENCE OF ' WHERE '
         IN SECTION OFFSET ls_find_from-offset OF lw_query
         RESULTS ls_find_where IGNORING CASE.
    IF sy-subrc NE 0.
      FIND FIRST OCCURRENCE OF ' GROUP BY ' IN lw_query
           RESULTS ls_find_where IGNORING CASE.
    ENDIF.
    IF sy-subrc NE 0.
      FIND FIRST OCCURRENCE OF ' HAVING ' IN lw_query
           RESULTS ls_find_where IGNORING CASE.
    ENDIF.
    IF sy-subrc NE 0.
      FIND FIRST OCCURRENCE OF ' ORDER BY ' IN lw_query
           RESULTS ls_find_where IGNORING CASE.
    ENDIF.

    lw_offset = ls_find_select-offset + 7.
    lw_length = ls_find_from-offset - ls_find_select-offset - 7.
    IF lw_length LE 0.
      fw_error = abap_true.
      RETURN.
    ENDIF.
    fw_select = lw_query+lw_offset(lw_length).

* Detect new syntax in comma field select separator
    IF fw_select CS ','.
      fw_newsyntax = abap_true.
    ENDIF.

    lw_offset = ls_find_from-offset + 6.
    IF ls_find_where IS INITIAL.
      fw_from = lw_query+lw_offset.
      fw_where = ''.
    ELSE.
      lw_length = ls_find_where-offset - ls_find_from-offset - 6.
      fw_from = lw_query+lw_offset(lw_length).
      lw_offset = ls_find_where-offset.
      fw_where = lw_query+lw_offset.
    ENDIF.

* Authority-check on used select tables
*    IF s_customize-auth_object NE space OR s_customize-auth_select NE '*'.
*      CONCATENATE 'JOIN' fw_from INTO lw_string SEPARATED BY space.
*      TRANSLATE lw_string TO UPPER CASE.
*      SPLIT lw_string AT space INTO TABLE lt_split.
*      LOOP AT lt_split INTO lw_string.
*        lw_tabix = sy-tabix + 1.
*        CHECK lw_string = 'JOIN'.
** Read next line (table name)
*        READ TABLE lt_split INTO lw_table INDEX lw_tabix.
*        CHECK sy-subrc = 0.
*
*        IF s_customize-auth_object NE space.
*          AUTHORITY-CHECK OBJECT s_customize-auth_object
*                   ID 'TABLE' FIELD lw_table
*                   ID 'ACTVT' FIELD s_customize-actvt_select.
*        ELSEIF s_customize-auth_select NE '*'
*        AND NOT lw_table CP s_customize-auth_select.
*          sy-subrc = 4.
*        ENDIF.
*        IF sy-subrc NE 0.
*          CONCATENATE 'No authorisation for table'(m13) lw_table
*                      INTO lw_string SEPARATED BY space.
*          MESSAGE lw_string TYPE c_msg_success DISPLAY LIKE c_msg_error.
*          CLEAR fw_from.
*          fw_noauth = abap_true.
*          RETURN.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

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

    ENDLOOP.

    DATA(lo_new_type) = cl_abap_structdescr=>create( lt_comp ).
    DATA(lo_new_tab) = cl_abap_tabledescr=>create(
                        p_line_type  = lo_new_type
                        p_table_kind = cl_abap_tabledescr=>tablekind_std
                        p_unique     = abap_false ).

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
*    CREATE DATA ms_draft-s_preview-tab TYPE STANDARD TABLE OF (fw_table).
    CREATE DATA ms_draft-s_preview-tab TYPE HANDLE lo_new_tab.
    ASSIGN ms_draft-s_preview-tab->* TO <tab>.
    <tab> = <lft_data>.

    FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
*    CREATE DATA ms_draft-s_preview-tab_backup TYPE STANDARD TABLE OF (fw_table).
    CREATE DATA ms_draft-s_preview-tab_backup TYPE HANDLE lo_new_tab.
    ASSIGN ms_draft-s_preview-tab_backup->* TO <tab2>.
    <tab2> = <tab>.

    client->_bind_clear( `MS_DRAFT-S_PREVIEW-TAB->*` ).
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

*    DATA(ls_sql_command) = z2ui5_cl_util=>sql_get_by_string( ms_draft-sql_input ).
*
*    IF ms_draft-sql_s_command-table <> ls_sql_command-table.
*      CREATE DATA ms_draft-s_preview-tab TYPE STANDARD TABLE OF (ls_sql_command-table).
*      CREATE DATA ms_draft-s_preview-tab_backup TYPE STANDARD TABLE OF (ls_sql_command-table).
*      client->_bind_clear( `MS_DRAFT-S_PREVIEW-TAB->*` ).
*      preview_view( ).
*    ENDIF.
*    client->view_model_update( ).
**    ENDIF.
*
*    ms_draft-sql_s_command = ls_sql_command.
*    sql_db_read( ).
*
*    history_db_save( ).
**    history_db_read( ).
*    client->message_toast_display( `Database succesfully loaded` ).

    DATA : lw_query         TYPE string,
           lw_select        TYPE string,
           lw_from          TYPE string,
           lw_where         TYPE string,
           lw_union         TYPE string,
           lw_query2        TYPE string,
           lw_command       TYPE string,
           lw_rows(6)       TYPE n,
           lw_program       TYPE sy-repid,
           lo_result        TYPE REF TO data,
           lo_result2       TYPE REF TO data,
           lt_fieldlist     TYPE ty_fieldlist_table,
           lt_fieldlist2    TYPE ty_fieldlist_table,
           lw_count_only(1) TYPE c,
           lw_time          TYPE p LENGTH 8 DECIMALS 2,
           lw_time2         LIKE lw_time,
           lw_count         TYPE i,
           lw_count2        LIKE lw_count,
           lw_charnumb(12)  TYPE c,
           lw_msg           TYPE string,
           lw_noauth(1)     TYPE c,
           lw_newsyntax(1)  TYPE c,
           lw_answer(1)     TYPE c,
           lw_from_concat   LIKE lw_from,
           lw_error(1)      TYPE c,
           lv_wquery        TYPE string.

    FIELD-SYMBOLS : <lft_data>  TYPE STANDARD TABLE,
                    <lft_data2> TYPE STANDARD TABLE.

    lv_wquery = ms_draft-sql_input.

    editor_get_query(
      CHANGING
        fw_query = lv_wquery
    ).

    query_parse(
      EXPORTING
        fw_query     = lv_wquery
      CHANGING
        fw_select    = lw_select
        fw_from      = lw_from
        fw_where     = lw_where
        fw_union     = lw_union
        fw_rows      = ms_draft-sql_max_rows
        fw_noauth    = lw_noauth
        fw_newsyntax = lw_newsyntax
        fw_error     = lw_error
    ).

    IF lw_error NE space.
*      MESSAGE 'Cannot parse the query'(m07) TYPE c_msg_error.
    ENDIF.

    query_generate(
      EXPORTING
        fw_select    = lw_select
        fw_from      = lw_from
        fw_where     = lw_where
        fw_display   = ' '
        fw_newsyntax = ' '
      CHANGING
        fw_program   = lw_program
        fw_rows      = ms_draft-sql_max_rows
        ft_fieldlist = lt_fieldlist2
        fw_count     = lw_count_only
    ).

    IF NOT lw_program IS INITIAL.
      PERFORM run_sql IN PROGRAM (lw_program)
                      CHANGING lo_result lw_time lw_count.
      lw_from_concat = lw_from.
* For union, process second (and further) query
      WHILE NOT lw_union IS INITIAL.
* Parse Query
        lw_query2 = lw_union.
*      PERFORM query_parse USING lw_query2
*                          CHANGING lw_select lw_from lw_where
*                                   lw_union lw_rows lw_noauth
*                                   lw_newsyntax lw_error.

        query_parse(
         EXPORTING
           fw_query     = lw_query2
         CHANGING
           fw_select    = lw_select
           fw_from      = lw_from
           fw_where     = lw_where
           fw_union     = lw_union
           fw_rows      = ms_draft-sql_max_rows
           fw_noauth    = lw_noauth
           fw_newsyntax = lw_newsyntax
           fw_error     = lw_error
       ).


        CONCATENATE lw_from_concat 'JOIN' lw_from INTO lw_from_concat.
        IF lw_noauth NE space.
*        PERFORM ddic_set_tree USING lw_from_concat.
          RETURN.
        ELSEIF lw_select IS INITIAL OR lw_from IS INITIAL
        OR lw_error = abap_true.
*        PERFORM ddic_set_tree USING lw_from_concat.
*        MESSAGE 'Cannot parse the unioned query'(m08) TYPE c_msg_error.
          EXIT. "exit while
        ENDIF.
* Generate subroutine
*      IF w_run LT c_query_max_exec.
*        PERFORM query_generate USING lw_select lw_from
*                                     lw_where fw_display
*                                     lw_newsyntax
*                               CHANGING lw_program lw_rows
*                                        lt_fieldlist2 lw_count_only.


        query_generate(
          EXPORTING
            fw_select    = lw_select
            fw_from      = lw_from
            fw_where     = lw_where
            fw_display   = ' '
            fw_newsyntax = ' '
          CHANGING
            fw_program   = lw_program
            fw_rows      = ms_draft-sql_max_rows
            ft_fieldlist = lt_fieldlist2
            fw_count     = lw_count_only
        ).

        IF lw_program IS INITIAL.
*          PERFORM ddic_set_tree USING lw_from_concat.
          RETURN.
        ENDIF.
*        w_run = w_run + 1.
*      ELSE.
*        MESSAGE 'No more run available. Please restart program'(m50)
*                TYPE c_msg_error.
*      ENDIF.
* Call the generated subroutine
        PERFORM run_sql IN PROGRAM (lw_program)
                        CHANGING lo_result2 lw_time2 lw_count2.

* Append lines of the further queries to the first query
        ASSIGN lo_result->* TO <lft_data>.
        ASSIGN lo_result2->* TO <lft_data2>.
        APPEND LINES OF <lft_data2> TO <lft_data>.
        CLEAR <lft_data2>.
        lw_time = lw_time + lw_time2.
        lw_count = lw_count + lw_count2.
      ENDWHILE.


      result_display(
        EXPORTING
          fw_table     = lw_from
          fo_result    = lo_result
          ft_fieldlist = lt_fieldlist2
          fw_title     = lw_query
      ).

    ENDIF.

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
        DATA(lo_popup_range) = CAST z2ui5_cl_popup_get_range_multi( client->get_app( client->get( )-s_draft-id_prev_app ) ).
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
      WHEN 'VIEW_SETTINGS_DIALOG'.
        z2ui5_view_settings_popup( ).

      WHEN `PREVIEW_FILTER`.
        client->nav_app_call( z2ui5_cl_popup_get_range_multi=>factory( ms_draft-s_preview-t_filter ) ).

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


  METHOD z2ui5_on_init_check_draft.

*    TRY.
*        DATA(lv_id) = z2ui5_sql_cl_history_api=>db_read_draft( ).
*        DATA(lo_app) = client->get_app( lv_id ).
*        ms_control-callback_pop_session_load = client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory( |Active draft for user { sy-uname } found, you want to return?| ) ).
*      CATCH cx_root.
    z2ui5_on_init_set_app( ).
*    ENDTRY.

  ENDMETHOD.


  METHOD z2ui5_on_init_set_app.

*    ms_draft-sql_input = `Select from T100 fields * .`.
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

*    preview_view( view_preview ).
    preview_view(  ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_view_settings_popup.
    DATA(popup_settings) = z2ui5_cl_xml_view=>factory_popup( ).

    popup_settings = popup_settings->view_settings_dialog(
                                    confirm = client->_event( 'ALL_EVENT' )
                                    sortitems = client->_bind_edit( t_tab_sort )
                                    groupitems = client->_bind_edit( t_tab_group )
                                    filteritems = client->_bind_edit( t_tab_filter )
                        )->sort_items(
                          )->view_settings_item( text = `{TEXT}` key = `{KEY}` selected = `{SELECTED}` )->get_parent( )->get_parent(
                        )->group_items(
                          )->view_settings_item( text = `{TEXT}` key = `{KEY}` selected = `{SELECTED}` )->get_parent( )->get_parent(
                        )->filter_items(
                          )->view_settings_filter_item( text = `{TEXT}` key = `{KEY}` multiselect = abap_true
                            )->items(
                              )->view_settings_item( text = `{TEXT}` key = `{KEY}` ).

    client->popup_display( popup_settings->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
