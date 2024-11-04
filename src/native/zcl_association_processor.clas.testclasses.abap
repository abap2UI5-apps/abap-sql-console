*"* use this source file for your ABAP unit test classes
CLASS ltc_create_select_list DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      co_empty_entity_name        TYPE string VALUE ``,
      co_empty_target_data_source TYPE string VALUE ``,
      co_association_name         TYPE string VALUE `ASSOCIATION_NAME`.
    DATA:
      co_empty_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_values.

    DATA:
      act_select_list TYPE string,
      cut             TYPE REF TO cl_adt_dp_cds_assoc_osql_map.

    METHODS: setup RAISING cx_static_check,

      no_columns FOR TESTING RAISING cx_static_check,
      one_column FOR TESTING RAISING cx_static_check,
      two_columns FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_create_select_list IMPLEMENTATION.

  METHOD setup.
    CLEAR co_empty_source_data.

    CREATE OBJECT cut
      EXPORTING
        i_entity_name        = co_empty_entity_name
        i_association_name   = co_association_name
        i_target_data_source = co_empty_target_data_source
        i_source_data        = co_empty_source_data
        i_param_values       = ''.
  ENDMETHOD.

  METHOD no_columns.

    CLEAR cut->target_columns.

    act_select_list = cut->create_select_list( ).

    cl_abap_unit_assert=>assert_initial( act_select_list ).

  ENDMETHOD.

  METHOD one_column.

    cut->target_columns = VALUE #( ( fieldname = `FIELDNAME` ) ).

    act_select_list = cut->create_select_list( ).

    cl_abap_unit_assert=>assert_equals( act = act_select_list
                                        exp = |{ co_association_name }.FIELDNAME| ).

  ENDMETHOD.

  METHOD two_columns.

    cut->target_columns = VALUE #( ( fieldname = `FIRST_FIELDNAME` ) ( fieldname = `SECOND_FIELDNAME` ) ).

    act_select_list = cut->create_select_list( ).

    cl_abap_unit_assert=>assert_equals( act = act_select_list
                                        exp = |{ co_association_name }.FIRST_FIELDNAME, { co_association_name }.SECOND_FIELDNAME| ).

  ENDMETHOD.



ENDCLASS.

CLASS ltd_dd_sobject DEFINITION FINAL FOR TESTING .
  PUBLIC SECTION.
    INTERFACES: if_dd_sobject PARTIALLY IMPLEMENTED.
    DATA: structure_headers TYPE dd02bndvtab.
ENDCLASS.


CLASS ltd_dd_sobject IMPLEMENTATION.
  METHOD if_dd_sobject~read.
    dd02bndv_tab = structure_headers.
  ENDMETHOD.
ENDCLASS.


CLASS ltd_name_generator DEFINITION FINAL FOR TESTING .
  PUBLIC SECTION.
    INTERFACES: lif_name_generator PARTIALLY IMPLEMENTED.
    DATA: sql_view_name TYPE string.
ENDCLASS.


CLASS ltd_name_generator IMPLEMENTATION.
  METHOD lif_name_generator~create_sql_view_name.
    r_name = sql_view_name.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_get_ddl_source DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      co_target_entity_name       TYPE string VALUE `TARGET_ENTITY_NAME`,
      co_source_entity_name       TYPE string VALUE `SOURCE_ENTITY_NAME`,
      co_empty_target_data_source TYPE string VALUE ``,
      co_association_name         TYPE string VALUE `ASSOCIATION_NAME`,
      co_sql_view_name            TYPE string VALUE `SQL_VIEW_NAME`.
    DATA:
      co_empty_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_values.

    DATA:
      td_dd_sobject     TYPE REF TO ltd_dd_sobject,
      td_name_generator TYPE REF TO ltd_name_generator,
      cut               TYPE REF TO cl_adt_dp_cds_assoc_osql_map,
      act_ddl_source    TYPE ddddlsrcv,
      exp_source        TYPE ddddlsource.
    METHODS:
      setup RAISING cx_static_check,

      simple_association_no_filter FOR TESTING RAISING cx_static_check,
      simple_association_with_filter FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_get_ddl_source IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT cut
      EXPORTING
        i_entity_name        = co_source_entity_name
        i_association_name   = co_association_name
        i_target_data_source = co_empty_target_data_source
        i_source_data        = co_empty_source_data
        i_param_values       = ''.

    CREATE OBJECT td_dd_sobject.
    cut->dd_sobject = td_dd_sobject.

    CREATE OBJECT td_name_generator.
    cut->name_generator = td_name_generator.

  ENDMETHOD.

  METHOD simple_association_no_filter.

    td_dd_sobject->structure_headers = VALUE #( ( clientfield = 'MANDT' ) ).

    td_name_generator->sql_view_name = co_sql_view_name.

    cut->target_columns = VALUE #( ( entityname = co_target_entity_name fieldname = 'FIRST_FIELD' )
                                   ( entityname = co_target_entity_name fieldname = 'SECOND_FIELD' )
                                   ( entityname = co_target_entity_name fieldname = 'THIRD_FIELD' )
                                 ).

    act_ddl_source = cut->get_ddl_source( ).

    cl_abap_unit_assert=>assert_equals( act = act_ddl_source-ddlname exp = co_sql_view_name ).

    exp_source =  |@AbapCatalog.sqlViewName: '{ co_sql_view_name }' | &&
                  |define view { co_sql_view_name && `E` } as select from { co_source_entity_name } \{ | &&
                  |{ co_association_name }.FIRST_FIELD, { co_association_name }.SECOND_FIELD, { co_association_name }.THIRD_FIELD \}|.

    CONDENSE: exp_source, act_ddl_source-source.
    exp_source = to_upper( exp_source ).
    act_ddl_source-source = to_upper( act_ddl_source-source ).

    cl_abap_unit_assert=>assert_equals( act = act_ddl_source-source exp = exp_source ).

  ENDMETHOD.

  METHOD simple_association_with_filter.

    td_dd_sobject->structure_headers = VALUE #( ( clientfield = 'MANDT' ) ).

    td_name_generator->sql_view_name = co_sql_view_name.

    cut->target_columns = VALUE #( ( entityname = co_target_entity_name fieldname = 'TARGET_FIELD' ) ).

    cut->source_data = VALUE #( ( component_name = 'CLNT' value = '000' )
                                ( component_name = 'KEY'  value = 'A' )
                                ( component_name = 'FIELD' value = 'Z' )
                              ).

    cut->source_columns = VALUE #( ( entityname = co_source_entity_name fieldname = 'CLNT' keyflag = 'X' datatype = 'CLNT' )
                                   ( entityname = co_source_entity_name fieldname = 'KEY' keyflag = 'X' )
                                   ( entityname = co_source_entity_name fieldname = 'FIELD' )
                                 ).

    act_ddl_source = cut->get_ddl_source( ).

    cl_abap_unit_assert=>assert_equals( act = act_ddl_source-ddlname exp = co_sql_view_name ).

    exp_source =  |@AbapCatalog.sqlViewName: '{ co_sql_view_name }' | &&
                  |define view { co_sql_view_name && `E` } as select from { co_source_entity_name } \{ | &&
                  |{ co_association_name }.TARGET_FIELD \}|.

    CONDENSE: exp_source, act_ddl_source-source.
    exp_source = to_upper( exp_source ).
    act_ddl_source-source = to_upper( act_ddl_source-source ).

    cl_abap_unit_assert=>assert_equals( act = act_ddl_source-source exp = exp_source ).

  ENDMETHOD.

ENDCLASS.


CLASS ltd_data_source_utils DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: lif_data_source_utils PARTIALLY IMPLEMENTED.

    METHODS:

      set_client_field
        IMPORTING
          i_name TYPE string.

  PRIVATE SECTION.
    DATA: client_field TYPE string.

ENDCLASS.


CLASS ltd_data_source_utils IMPLEMENTATION.

  METHOD set_client_field.
    client_field = i_name.
  ENDMETHOD.

  METHOD lif_data_source_utils~get_client_field.
    r_name = client_field.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_conv_statement_to_open_sql DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      co_target_entity_name     TYPE string VALUE `TARGET_ENTITY_NAME`,
      co_source_entity_name     TYPE string VALUE `SOURCE_ENTITY_NAME`,
      co_empty_association_name TYPE string VALUE ``.
    DATA:
      co_empty_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_values.

    DATA:
      td_dd_sobject        TYPE REF TO ltd_dd_sobject,
      td_name_generator    TYPE REF TO ltd_name_generator,
      td_data_source_utils TYPE REF TO ltd_data_source_utils,
      cut                  TYPE REF TO cl_adt_dp_cds_assoc_osql_map.


    METHODS: setup RAISING cx_static_check,
      setup_stubs RAISING cx_static_check.

ENDCLASS.


CLASS ltc_conv_statement_to_open_sql IMPLEMENTATION.

  METHOD setup.
    CLEAR co_empty_source_data.

    CREATE OBJECT cut
      EXPORTING
        i_entity_name        = co_source_entity_name
        i_association_name   = co_empty_association_name
        i_target_data_source = co_target_entity_name
        i_source_data        = co_empty_source_data
        i_param_values       = ''.
  ENDMETHOD.

  METHOD setup_stubs.
    CREATE OBJECT td_dd_sobject.
    cut->dd_sobject = td_dd_sobject.

    CREATE OBJECT td_name_generator.
    cut->name_generator = td_name_generator.

    CREATE OBJECT td_data_source_utils.
    cut->data_source_utils = td_data_source_utils.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_create_where_clause DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      co_empty_entity_name        TYPE string VALUE ``,
      co_empty_target_data_source TYPE string VALUE ``,
      co_empty_association_name   TYPE string VALUE ``.
    DATA:
      co_empty_source_data        TYPE cl_adt_dp_cds_assoc_osql_map=>ty_component_values.

    DATA:
      cut               TYPE REF TO cl_adt_dp_cds_assoc_osql_map.

    METHODS: setup RAISING cx_static_check.


ENDCLASS.


CLASS ltc_create_where_clause IMPLEMENTATION.

  METHOD setup.
    CLEAR co_empty_source_data.

    CREATE OBJECT cut
      EXPORTING
        i_entity_name        = co_empty_entity_name
        i_association_name   = co_empty_association_name
        i_target_data_source = co_empty_target_data_source
        i_source_data        = co_empty_source_data
        i_param_values       = ''.

  ENDMETHOD.

ENDCLASS.


CLASS ltd_dd_sobject_util DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: if_dd_sobject_util PARTIALLY IMPLEMENTED.
    DATA: columns TYPE ddentity_column_tab.
ENDCLASS.


CLASS ltd_dd_sobject_util IMPLEMENTATION.
  METHOD if_dd_sobject_util~get_columns.
    columns = me->columns.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_data_source_utils DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS: co_empty_data_source_name TYPE string VALUE ``,
               co_data_source_kind_stob  TYPE ddtypekind VALUE 'STOB'.

    DATA: cut                TYPE REF TO lcl_data_source_utils,
          td_dd_sobject_util TYPE REF TO ltd_dd_sobject_util,
          act_columns        TYPE ddentity_column_tab,
          act_client_field   TYPE string.

    METHODS:

      setup RAISING cx_static_check,

      viewfield_mapped_to_fieldname FOR TESTING RAISING cx_static_check,
      include_field_is_ignored FOR TESTING RAISING cx_static_check,
      clnt_set_as_client_field FOR TESTING RAISING cx_static_check,
      non_clnt_not_set_as_clnt_field FOR TESTING RAISING cx_static_check,
      clnt_field_must_be_first_field FOR TESTING RAISING cx_static_check,
      first_clnt_field_is_used FOR TESTING RAISING cx_static_check,
      no_columns_clnt_field_empty FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_data_source_utils IMPLEMENTATION.

  METHOD setup.
    cut = lcl_data_source_utils=>create( ).

    CREATE OBJECT td_dd_sobject_util.
    cut->dd_sobject_util = td_dd_sobject_util.

  ENDMETHOD.

  METHOD viewfield_mapped_to_fieldname.

    DATA(td_view_fields) = VALUE lcl_data_source_utils=>ty_view_fields( ( fieldname = 'FIELDNAME' viewfield = 'VIEWFIELD' ) ).

    DATA(act_columns) = cut->view_fields_2_columns( td_view_fields ).

    cl_abap_unit_assert=>assert_not_initial( act_columns ).

    cl_abap_unit_assert=>assert_equals(  act = act_columns[ 1 ]-fieldname
                                         exp = 'VIEWFIELD' ).

  ENDMETHOD.

  METHOD include_field_is_ignored.

    td_dd_sobject_util->columns = VALUE #( ( fieldname = '.INCLUDE' ) ).

    act_columns = cut->lif_data_source_utils~get_columns( i_data_source_name = co_empty_data_source_name
                                                          i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_initial( act_columns ).

  ENDMETHOD.

  METHOD clnt_set_as_client_field.

    td_dd_sobject_util->columns = VALUE #( ( fieldname = 'CLIENT_FIELD' keyflag = 'X' datatype = 'CLNT' ) ).

    act_client_field = cut->lif_data_source_utils~get_client_field( i_data_source_name = co_empty_data_source_name
                                                                    i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_equals( act = act_client_field exp = 'CLIENT_FIELD' ).

  ENDMETHOD.

  METHOD non_clnt_not_set_as_clnt_field.

    td_dd_sobject_util->columns = VALUE #( ( fieldname = 'NON_CLIENT_FIELD' keyflag = 'X' datatype = 'CHAR1' ) ).

    act_client_field = cut->lif_data_source_utils~get_client_field( i_data_source_name = co_empty_data_source_name
                                                                    i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_initial( act_client_field ).

  ENDMETHOD.

  METHOD clnt_field_must_be_first_field.

    td_dd_sobject_util->columns = VALUE #(
                                            ( fieldname = 'NON_CLIENT_FIELD' keyflag = 'X' datatype = 'CHAR1' )
                                            ( fieldname = 'CLIENT_FIELD' keyflag = 'X' datatype = 'CLNT' )
                                         ).

    act_client_field = cut->lif_data_source_utils~get_client_field( i_data_source_name = co_empty_data_source_name
                                                                    i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_initial( act_client_field ).

  ENDMETHOD.

  METHOD first_clnt_field_is_used.

    td_dd_sobject_util->columns = VALUE #(
                                            ( fieldname = 'FIRST_CLIENT_FIELD' keyflag = 'X' datatype = 'CLNT' )
                                            ( fieldname = 'SECOND_CLIENT_FIELD' keyflag = 'X' datatype = 'CLNT' )
                                         ).

    act_client_field = cut->lif_data_source_utils~get_client_field( i_data_source_name = co_empty_data_source_name
                                                                    i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_equals( act = act_client_field exp = 'FIRST_CLIENT_FIELD' ).

  ENDMETHOD.

  METHOD no_columns_clnt_field_empty.

    act_client_field = cut->lif_data_source_utils~get_client_field( i_data_source_name = co_empty_data_source_name
                                                                    i_data_source_kind = co_data_source_kind_stob ).

    cl_abap_unit_assert=>assert_initial( act_client_field ).

  ENDMETHOD.

ENDCLASS.
