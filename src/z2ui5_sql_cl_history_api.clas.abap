CLASS z2ui5_sql_cl_history_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_s_entry TYPE z2ui5_sql_t_01.
    TYPES ty_t_entry TYPE STANDARD TABLE OF z2ui5_sql_t_01 WITH EMPTY KEY.

    CLASS-METHODS db_create
      IMPORTING
        VALUE(val) TYPE ty_s_entry.

    CLASS-METHODS db_read_multi_by_user
      IMPORTING
        val           TYPE clike DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE ty_t_entry.

    CLASS-METHODS db_read_by_id
      IMPORTING
        val           TYPE clike DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE ty_s_entry.

    CLASS-METHODS db_delete
      IMPORTING
        user TYPE clike DEFAULT sy-uname.

    CLASS-METHODS db_create_draft
      IMPORTING
        VALUE(val) TYPE clike.

    CLASS-METHODS db_read_draft
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_sql_cl_history_api IMPLEMENTATION.

  METHOD db_create.

    val-uuid = z2ui5_cl_util_func=>func_get_uuid_32( ).
    val-uname = sy-uname.
    val-timestampl = z2ui5_cl_util_func=>time_get_timestampl( ).
    MODIFY z2ui5_sql_t_01 FROM @val.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD db_read_multi_by_user.

    SELECT FROM z2ui5_sql_t_01
        FIELDS *
    WHERE uname = @val
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.

  METHOD db_delete.

    DELETE FROM z2ui5_sql_t_01 WHERE uname = user.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD db_create_draft.

    MODIFY z2ui5_sql_t_01 FROM @( VALUE #( uuid = sy-uname result_data = val ) ).
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD db_read_draft.

    SELECT SINGLE FROM z2ui5_sql_t_01
         FIELDS result_data
     WHERE uuid = @sy-uname
     INTO @result.

  ENDMETHOD.

  METHOD db_read_by_id.

    SELECT single FROM z2ui5_sql_t_01
        FIELDS *
    WHERE uuid = @val
    INTO CORRESPONDING FIELDS OF @result.

  ENDMETHOD.

ENDCLASS.
