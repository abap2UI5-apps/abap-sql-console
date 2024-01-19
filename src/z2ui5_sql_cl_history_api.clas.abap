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

    CLASS-METHODS db_read_by_user
      IMPORTING
        val           TYPE clike DEFAULT sy-uname
      RETURNING
        VALUE(result) TYPE ty_t_entry.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_sql_cl_history_api IMPLEMENTATION.

  METHOD db_create.

    val-uuid = z2ui5_sql_cl_util=>get_uuid32( ).
    val-uname = sy-uname.
    MODIFY z2ui5_sql_t_01 FROM @val.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD db_read_by_user.

    SELECT FROM z2ui5_sql_t_01
        FIELDS *
    WHERE uname = @val
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.

ENDCLASS.
