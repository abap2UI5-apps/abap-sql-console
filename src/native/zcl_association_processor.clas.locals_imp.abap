*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
INTERFACE lif_name_generator.
  METHODS create_sql_view_name
    RETURNING
      VALUE(r_name) TYPE string .
ENDINTERFACE.

 class cl_adt_dp_cds_assoc_osql_map definition deferred.

INTERFACE lif_data_source_utils.

  METHODS:
    get_columns
      IMPORTING
        VALUE(i_data_source_name) TYPE string
        VALUE(i_data_source_kind) TYPE ddtypekind OPTIONAL
      RETURNING
        VALUE(r_columns)          TYPE ddentity_column_tab
      RAISING
        cx_dd_sobject_get,

    get_client_field
      IMPORTING
        VALUE(i_data_source_name) TYPE string
        VALUE(i_data_source_kind) TYPE ddtypekind OPTIONAL
      RETURNING
        VALUE(r_name)             TYPE string
      RAISING
        cx_dd_sobject_get.

ENDINTERFACE.

CLASS ltc_create_select_list DEFINITION DEFERRED.
CLASS ltc_conv_statement_to_open_sql DEFINITION DEFERRED.
CLASS ltc_get_ddl_source DEFINITION DEFERRED.
CLASS ltc_create_where_clause DEFINITION DEFERRED.

 CLASS lcl_name_generator DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    INTERFACES: lif_name_generator.
    CLASS-METHODS:
      create
        RETURNING VALUE(r_instance) TYPE REF TO lcl_name_generator.
ENDCLASS.

CLASS lcl_name_generator IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT r_instance.
  ENDMETHOD.

  METHOD lif_name_generator~create_sql_view_name.
    DATA(random_int) = cl_abap_random_int=>create(
      EXPORTING
        min  = 0
        seed = CONV i( sy-uzeit ) ).

    r_name = |Z{ random_int->get_next( ) }{ sy-uname(3) }|.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_data_source_utils DEFINITION DEFERRED.
CLASS lcl_data_source_utils DEFINITION CREATE PRIVATE FRIENDS ltc_data_source_utils.

  PUBLIC SECTION.
    INTERFACES: lif_data_source_utils.
    CLASS-METHODS:
      create
        RETURNING VALUE(r_instance) TYPE REF TO lcl_data_source_utils.

  PRIVATE SECTION.

    TYPES:
      ty_tabl_fields TYPE STANDARD TABLE OF dd03p WITH EMPTY KEY,
      ty_view_fields TYPE STANDARD TABLE OF dd27p WITH EMPTY KEY.

    DATA dd_sobject_util TYPE REF TO if_dd_sobject_util.

    METHODS:
      read_entity_columns
        IMPORTING
          !i_entity_name  TYPE string
        RETURNING
          VALUE(r_result) TYPE ddentity_column_tab
        RAISING
          cx_dd_sobject_get,

      tabl_fields_2_columns
        IMPORTING
          !i_fields       TYPE ty_tabl_fields
        RETURNING
          VALUE(r_result) TYPE ddentity_column_tab,

      view_fields_2_columns
        IMPORTING
          !i_fields       TYPE ty_view_fields
        RETURNING
          VALUE(r_result) TYPE ddentity_column_tab.

ENDCLASS.

CLASS lcl_data_source_utils IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT r_instance.
    r_instance->dd_sobject_util = cl_dd_sobject_factory=>create_util( ).
  ENDMETHOD.

  METHOD lif_data_source_utils~get_columns.

    DATA: type_name   TYPE typename,
          tabl_fields TYPE ty_tabl_fields,
          view_fields TYPE ty_view_fields,
          object_name TYPE ddobjname.

    CONDENSE i_data_source_name.

    IF i_data_source_kind IS NOT SUPPLIED
    OR i_data_source_kind IS INITIAL.
      type_name = i_data_source_name.
      CALL FUNCTION 'DDIF_TYPEINFO_GET'
        EXPORTING
          typename = type_name
        IMPORTING
          typekind = i_data_source_kind.
    ENDIF.

    CASE i_data_source_kind.

      WHEN 'STOB'.

        r_columns = read_entity_columns( i_data_source_name ).

      WHEN 'TABL'.

        object_name = i_data_source_name.
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name      = object_name
          TABLES
            dd03p_tab = tabl_fields.
        r_columns = tabl_fields_2_columns( tabl_fields ).

      WHEN 'VIEW'.

        object_name = i_data_source_name.
        CALL FUNCTION 'DDIF_VIEW_GET'
          EXPORTING
            name      = object_name
          TABLES
            dd27p_tab = view_fields.
        r_columns = view_fields_2_columns( view_fields ).

    ENDCASE.

    DELETE r_columns WHERE fieldname IS INITIAL OR fieldname(1) = `.`.

  ENDMETHOD.

  METHOD read_entity_columns.

    dd_sobject_util->get_columns(
      EXPORTING
        entitynames = VALUE if_dd_sobject_types=>ty_t_sobjnames(  ( |{ i_entity_name }| ) )
      IMPORTING
        columns = r_result ).

  ENDMETHOD.

  METHOD view_fields_2_columns.

    DATA: column LIKE LINE OF r_result.

    LOOP AT i_fields ASSIGNING FIELD-SYMBOL(<field>).
      MOVE-CORRESPONDING <field> TO column.
      column-entityname  = <field>-viewname.
      column-description = <field>-ddtext.
      column-fieldname = <field>-viewfield.
      APPEND column TO r_result.
    ENDLOOP.

  ENDMETHOD.

  METHOD tabl_fields_2_columns.

    DATA: column LIKE LINE OF r_result.

    LOOP AT i_fields ASSIGNING FIELD-SYMBOL(<field>).
      MOVE-CORRESPONDING <field> TO column.
      column-entityname  = <field>-tabname.
      column-description = <field>-ddtext.
      APPEND column TO r_result.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_data_source_utils~get_client_field.

    CONDENSE i_data_source_name.
    DATA(columns) = lif_data_source_utils~get_columns( i_data_source_name = i_data_source_name
                                                       i_data_source_kind = i_data_source_kind ).

    CHECK columns IS NOT INITIAL.

    DATA(first_column) = columns[ 1 ].
    IF first_column-datatype = 'CLNT'.
      r_name = to_upper( first_column-fieldname ).
      CONDENSE r_name.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS cl_adt_dp_cds_assoc_osql_map DEFINITION

  FINAL
  CREATE PUBLIC
  FRIENDS
  ltc_create_select_list
  ltc_conv_statement_to_open_sql
  ltc_get_ddl_source
  ltc_create_where_clause .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_component_value,
        component_name TYPE string,
        value          TYPE string,
      END OF ty_component_value .
    TYPES:
      ty_component_values TYPE STANDARD TABLE OF ty_component_value WITH EMPTY KEY .

    METHODS constructor
      IMPORTING
        !i_entity_name        TYPE string
        !i_association_name   TYPE string
        !i_target_data_source TYPE string
        !i_source_data        TYPE ty_component_values
        !i_param_values       TYPE string
      RAISING
        cx_dd_sobject_get
        cx_dd_sobject.
    METHODS get_statement
      EXPORTING
        !e_statement TYPE string
      RAISING
        cx_dd_sobject_get
        cx_dd_ddl_check.


  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_open_sql_statement,
        statement    TYPE string,
        source_alias TYPE string,
        target_alias TYPE string,
      END OF ty_open_sql_statement .

    DATA source_entity TYPE string .
    DATA association_name TYPE string .
    DATA target_data_source TYPE string .
    DATA source_data TYPE ty_component_values .
    DATA target_columns TYPE ddentity_column_tab .
    DATA source_columns TYPE ddentity_column_tab .
    DATA param_values   TYPE string.

    DATA data_source_utils TYPE REF TO lif_data_source_utils.
    DATA dd_sobject TYPE REF TO if_dd_sobject.
    DATA name_generator TYPE REF TO lif_name_generator.

    METHODS get_ddl_source
      RETURNING
        VALUE(r_source) TYPE ddddlsrcv
      RAISING
        cx_dd_sobject_get .

    METHODS create_select_list
      RETURNING
        VALUE(r_select_list) TYPE string .

    METHODS convert_statement
      IMPORTING
        !i_statement            TYPE string
      RETURNING
        VALUE(r_osql_statement) TYPE ty_open_sql_statement
      RAISING
        cx_dd_sobject_get.

    METHODS get_db_select_statement
      IMPORTING
        i_ddl_source              TYPE ddddlsrcv
      RETURNING
        VALUE(r_select_statement) TYPE string
      RAISING
        cx_dd_ddl_check.

    METHODS create_where_clause_w_client
      IMPORTING
        i_osql_statement      TYPE ty_open_sql_statement
      RETURNING
        VALUE(r_where_clause) TYPE string
      RAISING
        cx_dd_sobject_get.

ENDCLASS.
CLASS CL_ADT_DP_CDS_ASSOC_OSQL_MAP IMPLEMENTATION.

  METHOD constructor.

    dd_sobject = cl_dd_sobject_factory=>create( ).
    data_source_utils = lcl_data_source_utils=>create( ).
    name_generator = lcl_name_generator=>create( ).

    source_entity = to_upper( i_entity_name ).
    association_name = to_upper( i_association_name ).
    target_data_source = to_upper( i_target_data_source ).
    source_data = i_source_data.

    LOOP AT source_data ASSIGNING FIELD-SYMBOL(<data>).
      <data>-component_name = to_upper( <data>-component_name ).
      CONDENSE <data>-component_name.
    ENDLOOP.
    param_values = i_param_values.

    source_columns = data_source_utils->get_columns( i_data_source_name = source_entity i_data_source_kind = 'STOB' ).
    target_columns = data_source_utils->get_columns( target_data_source ).

  ENDMETHOD.
  METHOD convert_statement.

    TYPES: BEGIN OF ty_replacement,
             old TYPE string,
             new TYPE string,
           END OF ty_replacement.

    DATA: literal_replacements TYPE STANDARD TABLE OF ty_replacement WITH EMPTY KEY,
          source_name          TYPE string,
          submatch_1           TYPE string,
          submatch_2           TYPE string,
          submatch_3           TYPE string.

    r_osql_statement-statement = i_statement.

    "save the view name in src_name
    if param_values is not initial .
        DATA(source_alias_regex) = |FROM([[:space:]]+)([^[:space:]]+[[:space:]]+[^[:space:]])([[:space:]]+)([^[:space:]]+)|.
        FIND FIRST OCCURRENCE OF REGEX source_alias_regex IN r_osql_statement-statement SUBMATCHES submatch_1 source_name submatch_3 r_osql_statement-source_alias.
        REPLACE FIRST OCCURRENCE OF REGEX source_alias_regex IN r_osql_statement-statement WITH `FROM $2 AS $4`.

        SPLIT r_osql_statement-statement AT '(' INTO TABLE data(itab).
        SPLIT itab[ 2 ] AT ')' INTO TABLE data(itab2).
        data src_name type string.
        split source_name at '(' into TABLE data(itab_src).
        src_name = itab_src[ 1 ].

    else.
        "Find and fix source alias definition
        source_alias_regex = |FROM([[:space:]]+)([^[:space:]]+)([[:space:]]+)([^[:space:]]+)|.
        FIND FIRST OCCURRENCE OF REGEX source_alias_regex IN r_osql_statement-statement SUBMATCHES submatch_1 source_name submatch_3 r_osql_statement-source_alias.
        REPLACE FIRST OCCURRENCE OF REGEX source_alias_regex IN r_osql_statement-statement WITH `FROM $2 AS $4`.
    endif.

    "Find and fix target alias definition
    DATA(target_alias_regex) = |JOIN([[:space:]]+)([^[:space:]]+)([[:space:]]+)([^[:space:]]+)|.
    FIND FIRST OCCURRENCE OF REGEX target_alias_regex IN r_osql_statement-statement SUBMATCHES submatch_1 submatch_2 submatch_3 r_osql_statement-target_alias.
    REPLACE FIRST OCCURRENCE OF REGEX target_alias_regex IN r_osql_statement-statement WITH `JOIN $2 AS $4`.

    "Replace src_name( 'val' ) in parameter values with src_name( param = param_value )
    if param_values is not initial.
         data src_name_parameters type string.
         concatenate 'FROM ' src_name param_values into src_name_parameters respecting blanks.
         REPLACE FIRST OCCURRENCE OF regex `FROM([[:space:]]+)([^[:space:]]+)\([^\)]*\)` in r_osql_statement-statement with src_name_parameters.
    endif.

  ENDMETHOD.
  METHOD create_select_list.

    LOOP AT target_columns INTO DATA(column).
      IF sy-tabix = 1.
        r_select_list = |{ association_name }.{ column-fieldname }|.
        CONTINUE.
      ENDIF.

      r_select_list = r_select_list && |, { association_name }.{ column-fieldname }|.

    ENDLOOP.

  ENDMETHOD.
  METHOD create_where_clause_w_client.

    " Create filter for source fields
    LOOP AT source_data INTO DATA(component).

      IF r_where_clause IS NOT INITIAL.
        r_where_clause = r_where_clause && ` AND `.
      ENDIF.
      REPLACE ALL OCCURRENCES OF `'` IN component-value WITH `''`. "Escape single quotes
      r_where_clause = |{ r_where_clause }{ i_osql_statement-source_alias }.{ component-component_name } = '{ component-value }'|.

    ENDLOOP.

    " Add WHERE keyword
    IF r_where_clause IS NOT INITIAL.
      r_where_clause = `WHERE ` && r_where_clause.
    ENDIF.

  ENDMETHOD.
  METHOD get_db_select_statement.

    DATA(ddl_handler) = cl_dd_ddl_handler_factory=>create( ).

    "does this method exist in older releases?
    r_select_statement = ddl_handler->generate_create_statement(
      EXPORTING
        iv_ddlname           = i_ddl_source-ddlname
*        iv_prid              = -1
        is_source            = i_ddl_source
*        iv_state             = 'A'
*        iv_dbsys             = SY-DBSYS
*        checkmode            = cl_dd_cds_check=>co_dd_checkmode-online
*        io_parser_src_filter = io_parser_src_filter
    ).

***    ddl_handler->get_select(
***      EXPORTING
***        name            = i_ddl_source-ddlname
***        ddlsrcv_wa      = i_ddl_source
***      IMPORTING
***        select_stmt     = r_select_statement ).

  ENDMETHOD.
  METHOD get_ddl_source.

    r_source-ddlname = name_generator->create_sql_view_name( ).

    dd_sobject->read(
      EXPORTING
        sobjnames         = VALUE if_dd_sobject_types=>ty_t_sobjnames( ( |{ source_entity }| ) )
      IMPORTING
        dd02bndv_tab      = DATA(structure_headers)
    ).

    IF structure_headers[ 1 ]-clientfield IS INITIAL.
      r_source-source = |@ClientDependent: 'false'|.
    ENDIF.

    DATA(select_list) = create_select_list( ).
    if structure_headers[ 1 ]-with_parameters = 'X'.
        data parameters type string.
        concatenate ' ' parameters into parameters.
        parameters = param_values.
        replace all occurrences of '=' in parameters with ':'.
        r_source-source = |{ r_source-source } | &&
                          |@AbapCatalog.sqlViewName: '{ r_source-ddlname }' | &&
                          |DEFINE VIEW { r_source-ddlname && `E` } AS SELECT FROM { source_entity } { parameters } \{ | &&
                          |{ select_list } \}|.
    else.
        r_source-source = |{ r_source-source } | &&
                          |@AbapCatalog.sqlViewName: '{ r_source-ddlname }' | &&
                          |DEFINE VIEW { r_source-ddlname && `E` } AS SELECT FROM { source_entity } \{ | &&
                          |{ select_list } \}|.
    endif.

  ENDMETHOD.
  METHOD get_statement.

    DATA(ddl_source) = get_ddl_source( ).

    DATA(db_select_statement) = get_db_select_statement( ddl_source ).

***    DATA(osql_statement) = convert_statement_to_open_sql( db_select_statement ).
    DATA(osql_statement) = convert_statement( db_select_statement ). "no thanks


***    DATA(where_clause) = create_where_clause( osql_statement ).
    DATA(where_clause) = create_where_clause_w_client( osql_statement ).

    e_statement = osql_statement-statement && ` ` && where_clause.

***    REMOVE_MANDT(
***      changing
***        E_STATEMENT = e_statement
***    ).

  ENDMETHOD.

ENDCLASS.
