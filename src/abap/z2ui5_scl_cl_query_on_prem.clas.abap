CLASS z2ui5_scl_cl_query_on_prem DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES :
      BEGIN OF ty_fieldlist,
        field     TYPE string,
        ref_table TYPE string,
        ref_field TYPE string,
      END OF ty_fieldlist,
      ty_fieldlist_table TYPE STANDARD TABLE OF ty_fieldlist.

    CONSTANTS:
      c_ddic_col1          TYPE mtreeitm-item_name VALUE 'col1',
      c_ddic_col2          TYPE mtreeitm-item_name VALUE 'col2',
      c_line_max           TYPE i VALUE 255,
      c_msg_success        TYPE c VALUE 'S',
      c_msg_error          TYPE c VALUE 'E',
      c_vers_active        TYPE c VALUE 'A',
      c_ddic_dtelm         TYPE c VALUE 'E',
      c_native_command     TYPE string VALUE 'NATIVE',
      c_query_max_exec     TYPE i VALUE 1000,
      c_xmlnode_root       TYPE string VALUE 'root',
      c_xmlnode_file       TYPE string VALUE 'query',
      c_xmlattr_visibility TYPE string VALUE 'visibility',
      c_xmlattr_text       TYPE string VALUE 'description'.

    CLASS-METHODS editor_get_query
      CHANGING fw_query TYPE string.

    CLASS-METHODS query_parse
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

    CLASS-METHODS query_generate
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

    CLASS-METHODS sql_logic
      IMPORTING
        query         TYPE string
        max_rows      TYPE i
      EXPORTING
        lw_from       TYPE string
        lo_result     TYPE REF TO data
        lw_query      TYPE string
        lt_fieldlist2 TYPE ty_fieldlist_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_scl_cl_query_on_prem IMPLEMENTATION.


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

    CLEAR ls_fieldlist.
    APPEND ls_fieldlist TO ft_fieldlist.

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

  ENDMETHOD.




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



  METHOD sql_logic.

    DATA :
*           lw_query         TYPE string,
      lw_select        TYPE string,
*           lw_from          TYPE string,
      lw_where         TYPE string,
      lw_union         TYPE string,
      lw_query2        TYPE string,
      lw_command       TYPE string,
      lw_rows(6)       TYPE n,
      lw_program       TYPE sy-repid,
*           lo_result        TYPE REF TO data,
      lo_result2       TYPE REF TO data,
      lt_fieldlist     TYPE ty_fieldlist_table,
*           lt_fieldlist2    TYPE ty_fieldlist_table,
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
      lw_from_concat   TYPE string,
      lw_error(1)      TYPE c
*          lw_wquery        TYPE string
      .

    FIELD-SYMBOLS : <lft_data>  TYPE STANDARD TABLE,
                    <lft_data2> TYPE STANDARD TABLE.

    DATA(lv_wquery) = query.
    DATA(lv_max_rows) = max_rows.

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
        fw_rows      = lv_max_rows
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
        fw_rows      = lv_max_rows
        ft_fieldlist = lt_fieldlist2
        fw_count     = lw_count_only
    ).

    IF lw_program IS INITIAL.
      RETURN.
    ENDIF.

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
         fw_rows      = lv_max_rows
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
          fw_rows      = lv_max_rows
          ft_fieldlist = lt_fieldlist2
          fw_count     = lw_count_only
      ).

      IF lw_program IS INITIAL.
*          PERFORM ddic_set_tree USING lw_from_concat.
*        RETURN.
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


  ENDMETHOD.




ENDCLASS.
