CLASS z2ui5_sql_cl_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_sql_result,
        table TYPE string,
      END OF ty_s_sql_result.

    CLASS-METHODS get_sql_by_string
      IMPORTING
        val           TYPE clike
      RETURNING
        VALUE(result) TYPE ty_s_sql_result.

    CLASS-METHODS get_uuid32
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_sql_cl_util IMPLEMENTATION.
  METHOD get_sql_by_string.

    DATA(lv_sql) = val.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_sql  WITH ``.
    lv_sql = to_upper( lv_sql ).
    SPLIT lv_sql AT 'SELECTFROM' INTO DATA(lv_dummy) DATA(lv_tab).
    SPLIT lv_tab AT `FIELDS` INTO lv_tab lv_dummy.

    result-table = lv_tab.

  ENDMETHOD.

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

ENDCLASS.
