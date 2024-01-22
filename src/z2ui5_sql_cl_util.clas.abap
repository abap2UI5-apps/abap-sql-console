CLASS z2ui5_sql_cl_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_uuid32
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS c_trim
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_t_attri_by_object
      IMPORTING
        !val          TYPE REF TO object
      RETURNING
        VALUE(result) TYPE abap_attrdescr_tab.

    CLASS-METHODS rtti_get_t_comp_by_data
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE cl_abap_structdescr=>component_table.

    CLASS-METHODS rtti_get_type_name
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_check_type_kind_dref
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS rtti_get_classes_impl_intf
      IMPORTING
        !val          TYPE clike
      RETURNING
        VALUE(result) TYPE string_table.

    CLASS-METHODS rtti_get_type_kind
      IMPORTING
        !val          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS rtti_get_classname_by_ref
      IMPORTING
        !in           TYPE REF TO object
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_sql_cl_util IMPLEMENTATION.


  METHOD get_uuid32.

    TRY.
        DATA uuid TYPE c LENGTH 32.

        TRY.
            CALL METHOD (`CL_SYSTEM_UUID`)=>if_system_uuid_static~create_uuid_c32
              RECEIVING
                uuid = uuid.

          CATCH cx_sy_dyn_call_illegal_class.

            DATA(lv_fm) = `GUID_CREATE`.
            CALL FUNCTION lv_fm
              IMPORTING
                ev_guid_32 = uuid.

        ENDTRY.

        result = uuid.

      CATCH cx_root.
        ASSERT 1 = 0.
    ENDTRY.

  ENDMETHOD.


  METHOD c_trim.

    DATA temp3 TYPE string.
    temp3 = val.
    result = shift_left( shift_right( temp3 ) ).
    result = shift_right( val = result sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( val = result sub = cl_abap_char_utilities=>horizontal_tab ).
    result = shift_left( shift_right( result ) ).

  ENDMETHOD.

  METHOD rtti_check_type_kind_dref.

    DATA(lv_type_kind) = cl_abap_datadescr=>get_data_type_kind( val ).
    result = xsdbool( lv_type_kind = cl_abap_typedescr=>typekind_dref ).

  ENDMETHOD.


  METHOD rtti_get_classes_impl_intf.

    DATA obj TYPE REF TO object.
    FIELD-SYMBOLS <any> TYPE any.
    DATA lt_implementation_names TYPE string_table.
    TYPES BEGIN OF ty_s_impl.
    TYPES clsname TYPE c LENGTH 30.
    TYPES refclsname TYPE c LENGTH 30.
    TYPES END OF ty_s_impl.
    TYPES temp1 TYPE STANDARD TABLE OF ty_s_impl WITH DEFAULT KEY.
    DATA lt_impl TYPE temp1.
    TYPES BEGIN OF ty_s_key.
    TYPES intkey TYPE c LENGTH 30.
    TYPES END OF ty_s_key.
    DATA ls_key TYPE ty_s_key.
    DATA lv_fm TYPE string.
    DATA temp10 LIKE LINE OF lt_impl.
    DATA lr_impl LIKE REF TO temp10.
    DATA temp11 TYPE string.

    TRY.

        CALL METHOD ('XCO_CP_ABAP')=>interface
          EXPORTING
            iv_name      = val
          RECEIVING
            ro_interface = obj.

        ASSIGN obj->('IF_XCO_AO_INTERFACE~IMPLEMENTATIONS') TO <any>.
        obj = <any>.

        ASSIGN obj->('IF_XCO_INTF_IMPLEMENTATIONS_FC~ALL') TO <any>.
        obj = <any>.

        CALL METHOD obj->('IF_XCO_INTF_IMPLEMENTATIONS~GET').

        CALL METHOD obj->('IF_XCO_INTF_IMPLEMENTATIONS~GET_NAMES')
          RECEIVING
            rt_names = lt_implementation_names.

        result = lt_implementation_names.

      CATCH cx_sy_dyn_call_illegal_class.

        ls_key-intkey = val.

        lv_fm = `SEO_INTERFACE_IMPLEM_GET_ALL`.
        CALL FUNCTION lv_fm
          EXPORTING
            intkey       = ls_key
          IMPORTING
            impkeys      = lt_impl
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.

        LOOP AT lt_impl REFERENCE INTO lr_impl.

          temp11 = lr_impl->clsname.
          INSERT temp11 INTO TABLE result.
        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD rtti_get_classname_by_ref.

    DATA lv_classname TYPE abap_abstypename.
    lv_classname = cl_abap_classdescr=>get_class_name( in ).
    result = substring_after( val = lv_classname
                              sub = `\CLASS=` ).

  ENDMETHOD.


  METHOD rtti_get_type_kind.

    result = cl_abap_datadescr=>get_data_type_kind( val ).

  ENDMETHOD.


  METHOD rtti_get_type_name.

    DATA lo_descr TYPE REF TO cl_abap_typedescr.
    DATA temp12 TYPE REF TO cl_abap_elemdescr.
    DATA lo_ele LIKE temp12.
    lo_descr = cl_abap_elemdescr=>describe_by_data( val ).

    temp12 ?= lo_descr.

    lo_ele = temp12.
    result  = lo_ele->get_relative_name( ).

  ENDMETHOD.


  METHOD rtti_get_t_attri_by_object.

    DATA lo_obj_ref TYPE REF TO cl_abap_typedescr.
    DATA temp13 TYPE REF TO cl_abap_classdescr.
    lo_obj_ref = cl_abap_objectdescr=>describe_by_object_ref( val ).

    temp13 ?= lo_obj_ref.
    result   = temp13->attributes.

  ENDMETHOD.


  METHOD rtti_get_t_comp_by_data.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA temp14 TYPE REF TO cl_abap_tabledescr.
    DATA lo_tab LIKE temp14.
    DATA temp15 TYPE REF TO cl_abap_structdescr.
    DATA lo_struct LIKE temp15.
    DATA temp16 TYPE REF TO cl_abap_structdescr.
    lo_type = cl_abap_structdescr=>describe_by_data( val ).
    TRY.

        temp14 ?= lo_type.

        lo_tab = temp14.

        temp15 ?= lo_tab->get_table_line_type( ).

        lo_struct = temp15.
      CATCH cx_root.

        temp16 ?= lo_type.
        lo_struct = temp16.
    ENDTRY.

    result = lo_struct->get_components( ).

  ENDMETHOD.


ENDCLASS.
