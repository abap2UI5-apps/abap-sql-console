*"* use this source file for your ABAP unit test classes

class _sql_statement_t_double definition
                              create public
                              inheriting from sql_statement ##CLASS_FINAL.
		
  public section.

    methods value
              returning
                value(r_val) type string.

endclass.
class _sql_statement_t_double implementation.

  method value.

    r_val = me->a_sql_statement.

  endmethod.

endclass.
class _sql_statement_spy definition
                         create public
                         inheriting from _sql_statement_t_double ##CLASS_FINAL.

  public section.

    methods sole_associaton_if_any redefinition.

    methods generated_join_for_association redefinition.

    methods assoc_mapped_to_join_if_any redefinition.

    data sole_assoc_out type string.

    data generated_join_in type string.

    data generated_join_out type string.

    data assoc_mapped_in_1 type string.

    data assoc_mapped_in_2 type string.

    data assoc_mapped_out type string.

endclass.
class _sql_statement_spy implementation.

  method sole_associaton_if_any.

    me->sole_assoc_out = `SOLE_ASSOCIATON_IF_ANY`.

    r_val = me->sole_assoc_out.

  endmethod.
  method generated_join_for_association.

    me->generated_join_in = i_association.

    me->generated_join_out = `GENERATED_JOIN_FOR_ASSOCIATION`.

    r_val = me->generated_join_out.

  endmethod.
  method assoc_mapped_to_join_if_any.

    me->assoc_mapped_in_1 = i_association.

    me->assoc_mapped_in_2 = i_join.

    me->assoc_mapped_out = `ASSOC_MAPPED_TO_JOIN_IF_ANY`.

    r_val = me->assoc_mapped_out.

  endmethod.

endclass.
class _sql_statement_stub definition
                          create public
                          inheriting from _sql_statement_t_double ##CLASS_FINAL.
		
  public section.

    methods constructor
              importing
                i_processor type sql_statement=>t_processor
                i_association_processor type sql_statement=>t_association_processor
                i_sql_statement type string
                i_data_sources type string_table optional
                i_sole_association_return type string optional
                i_generated_join_return type string optional
                i_assoc_mapped_return type string optional
              raising
                cx_sql_exception.

    methods data_sources redefinition.

    methods sole_associaton_if_any redefinition.

    methods generated_join_for_association redefinition.

    methods assoc_mapped_to_join_if_any redefinition.

  protected section.

    data a_data_source_coll type string_table.

    data a_sole_association_return type string.

    data a_generated_join_return type string.

    data an_assoc_mapped_return type string.

endclass.
class _sql_statement_stub implementation.

  method constructor.

    super->constructor( i_processor = i_processor
                        i_association_processor = i_association_processor
                        i_sql_statement = i_sql_statement ).

    me->a_data_source_coll = i_data_sources.

    me->a_sole_association_return = i_sole_association_return.

    me->a_generated_join_return = i_generated_join_return.

    me->an_assoc_mapped_return = i_assoc_mapped_return.

  endmethod.
  method data_sources.

    r_val = cond #( when me->a_data_source_coll is initial
                    then super->data_sources( )
                    else me->a_data_source_coll ).

  endmethod.
  method assoc_mapped_to_join_if_any.

    r_val = cond #( when me->an_assoc_mapped_return is initial
                    then super->assoc_mapped_to_join_if_any( i_association = i_association
                                                             i_join = i_join )
                    else me->an_assoc_mapped_return ).

  endmethod.
  method generated_join_for_association.

    r_val = cond #( when me->a_generated_join_return is initial
                    then super->generated_join_for_association( i_association )
                    else me->a_generated_join_return ).

  endmethod.
  method sole_associaton_if_any.

    r_val = cond #( when me->a_sole_association_return is initial
                    then super->sole_associaton_if_any( )
                    else me->a_sole_association_return ).

  endmethod.

endclass.
class _cl_sql_statement_spy definition
                            create public
                            inheriting from cl_sql_statement ##CLASS_FINAL.
		
  public section.

    methods execute_query redefinition.

    data execute_query_in type string.

endclass.
class _cl_sql_statement_spy implementation.

  method execute_query.

    me->execute_query_in = statement.

    result_set = super->execute_query( `select 1 as test from dummy;` ) ##NO_TEXT.

  endmethod.

endclass.
class association_processor_dummy definition
                                  create public ##CLASS_FINAL.

  public section.

    interfaces: zif_association_processor.

endclass.
class association_processor_dummy implementation.

  method zif_association_processor~map_association_to_join.

    return.

  endmethod.

endclass.
class association_processor_spy definition
                                create public ##CLASS_FINAL.
		
  public section.

    interfaces: zif_association_processor.

    data data_sources type zif_association_processor=>t_data_sources.

    data association type string.

endclass.
class association_processor_spy implementation.

  method zif_association_processor~map_association_to_join.

    me->data_sources = i_potential_data_sources.

    me->association = i_association.

  endmethod.

endclass.

"! Tests for {@link sql_statement}
"! <br/>
"! ABAP has a ridiculous character limitation for names so all the names of the tests are useless, have fun
class _t_sql_statement definition
                       create private
                       final
                       for testing
                       duration short
                       risk level harmless.

  private section.

    types: begin of t_basic,
             field type ref to sql_statement,
             field_w_alias type ref to sql_statement,
             qualified_field type ref to sql_statement,
             qualified_field_w_alias type ref to sql_statement,
           end of t_basic,
           begin of t_simple_path_expressions,
             field type ref to sql_statement,
             field_w_alias type ref to sql_statement,
             qualified_field type ref to sql_statement,
             qualified_field_w_alias type ref to sql_statement,
             field_with_attr type ref to sql_statement,
             qualified_field_w_attr type ref to sql_statement,
             field_with_attr_w_alias type ref to sql_statement,
             qualified_field_w_attr_w_alias type ref to sql_statement,
           end of t_simple_path_expressions,
           begin of t_complex_path_expressions,
             field type ref to sql_statement,
             field_w_alias type ref to sql_statement,
             qualified_field type ref to sql_statement,
             qualified_field_w_alias type ref to sql_statement,
             field_with_attr type ref to sql_statement,
             qualified_field_w_attr type ref to sql_statement,
             field_with_attr_w_alias type ref to sql_statement,
             qualified_field_w_attr_w_alias type ref to sql_statement,
             field_with_several_assocs type ref to sql_statement,
           end of t_complex_path_expressions,
           begin of t_path_expressions,
             simple type _t_sql_statement=>t_simple_path_expressions,
             complex type _t_sql_statement=>t_complex_path_expressions,
           end of t_path_expressions,
           begin of t_statements,
             begin of non_delimited,
               path_expressions type _t_sql_statement=>t_path_expressions,
               basic type _t_sql_statement=>t_basic,
             end of non_delimited,
             begin of delimited,
               path_expressions type _t_sql_statement=>t_path_expressions,
               basic type _t_sql_statement=>t_basic,
             end of delimited,
           end of t_statements.

    methods constructor
              raising
                cx_sql_exception ##RELAX.

    methods assert_gt_1_assoc_throws_exc
              importing
                i_previous type any
                i_name type string optional.

    methods assert_le_1_assoc_no_exc
              importing
                i_previous type any
                i_name type string optional.

    methods assert_1_assoc_returned
              importing
                i_previous type any
                i_name type string optional
              raising
                cx_sql_exception.

    methods assert_le_1_assoc_rets_empty
              importing
                i_previous type any
                i_name type string optional
              raising
                cx_sql_exception.

    "! <p class="shorttext synchronized" lang="EN">TOP or LIMIT clause</p>
    "! {@link sql_statement.METH:add_limit_if_none} should respect the original statement
    methods with_row_cap_does_nothing for testing raising cx_static_check.

    "! <p class="shorttext synchronized" lang="EN">No TOP or LIMIT clause</p>
    "! {@link sql_statement.METH:add_limit_if_none} should add a limit clause at the end of the original statement
    methods without_row_cap_adds_limit for testing raising cx_static_check.

    methods execute_calls_methods for testing raising cx_static_check.

    methods execute_uses_original for testing raising cx_static_check.

    methods generated_join_calls_methods for testing raising cx_static_check.

    methods sole_assoc_throws_for_compl_pe for testing raising cx_static_check.

    methods sole_assoc_no_exc_for_simpl_pe for testing raising cx_static_check.

    methods sole_assoc_throws_for_non_u_pe for testing raising cx_static_check.

    methods sole_assoc_no_exc_for_u_pe for testing raising cx_static_check.

    methods sole_assoc_returns_u_pe for testing raising cx_static_check.

    methods sole_assoc_ret_empty_for_no_pe for testing raising cx_static_check.

    methods data_sources_returned for testing raising cx_static_check.

    methods assoc_mapping_added for testing raising cx_static_check.

    data statements type _t_sql_statement=>t_statements.

endclass.
class _t_sql_statement implementation.

  method constructor.

    data(assoc_proc) = new zcl_association_processor( ).

    me->statements = value #( non_delimited = value #( basic = value #( field = new sql_statement( i_sql_statement = `SELECT field,`
                                                                                                   i_processor = new cl_sql_statement( )
                                                                                                   i_association_processor = assoc_proc )
                                                                        field_w_alias = new sql_statement( i_sql_statement = `SELECT field AS f,`
                                                                                                           i_processor = new cl_sql_statement( )
                                                                                                           i_association_processor = assoc_proc )
                                                                        qualified_field = new sql_statement( i_sql_statement = `SELECT dbtab.field,`
                                                                                                             i_processor = new cl_sql_statement( )
                                                                                                             i_association_processor = assoc_proc )
                                                                        qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT dbtab.field AS f,`
                                                                                                                     i_processor = new cl_sql_statement( )
                                                                                                                     i_association_processor = assoc_proc ) )
                                                       path_expressions = value #( simple = value #( field = new sql_statement( i_sql_statement = `SELECT _assoc.field,`
                                                                                                                                i_processor = new cl_sql_statement( )
                                                                                                                                i_association_processor = assoc_proc )
                                                                                                     field_w_alias = new sql_statement( i_sql_statement = `SELECT _assoc.field AS f,`
                                                                                                                                        i_processor = new cl_sql_statement( )
                                                                                                                                        i_association_processor = assoc_proc )
                                                                                                     qualified_field = new sql_statement( i_sql_statement = `SELECT entity._assoc.field,`
                                                                                                                                          i_processor = new cl_sql_statement( )
                                                                                                                                          i_association_processor = assoc_proc )
                                                                                                     qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT entity._assoc.field AS f,`
                                                                                                                                                  i_processor = new cl_sql_statement( )
                                                                                                                                                  i_association_processor = assoc_proc )
                                                                                                     field_with_attr = new sql_statement( i_sql_statement = `SELECT _assoc[ asdfa kljkl ].field,`
                                                                                                                                          i_processor = new cl_sql_statement( )
                                                                                                                                          i_association_processor = assoc_proc )
                                                                                                     qualified_field_w_attr = new sql_statement( i_sql_statement = `SELECT entity._assoc[ asdfa kljkl ].field,`
                                                                                                                                                 i_processor = new cl_sql_statement( )
                                                                                                                                                 i_association_processor = assoc_proc )
                                                                                                     field_with_attr_w_alias = new sql_statement( i_sql_statement = `SELECT _assoc[ asdfa kljkl ].field AS f,`
                                                                                                                                                  i_processor = new cl_sql_statement( )
                                                                                                                                                  i_association_processor = assoc_proc )
                                                                                                     qualified_field_w_attr_w_alias = new sql_statement( i_sql_statement = `SELECT entity._assoc[ asdfa kljkl ].field AS f,`
                                                                                                                                                         i_processor = new cl_sql_statement( )
                                                                                                                                                         i_association_processor = assoc_proc ) )
                                                                                   complex = value #( field = new sql_statement( i_sql_statement = `SELECT _assoc1._assoc2.field,`
                                                                                                                                 i_processor = new cl_sql_statement( )
                                                                                                                                 i_association_processor = assoc_proc )
                                                                                                      field_w_alias = new sql_statement( i_sql_statement = `SELECT _assoc1._assoc2.field AS f,`
                                                                                                                                         i_processor = new cl_sql_statement( )
                                                                                                                                         i_association_processor = assoc_proc )
                                                                                                      qualified_field = new sql_statement( i_sql_statement = `SELECT entity._assoc1._assoc2.field,`
                                                                                                                                           i_processor = new cl_sql_statement( )
                                                                                                                                           i_association_processor = assoc_proc )
                                                                                                      qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT entity._assoc1._assoc2.field AS f,`
                                                                                                                                                   i_processor = new cl_sql_statement( )
                                                                                                                                                   i_association_processor = assoc_proc )
                                                                                                      field_with_attr = new sql_statement( i_sql_statement = `SELECT _assoc1[ asdfa kljkl ]._assoc2[ asdfa kljkl ].field,`
                                                                                                                                           i_processor = new cl_sql_statement( )
                                                                                                                                           i_association_processor = assoc_proc )
                                                                                                      qualified_field_w_attr = new sql_statement( i_sql_statement = `SELECT entity._assoc1[ asdfa kljkl ]._assoc2[ asdfa kljkl ].field,`
                                                                                                                                                  i_processor = new cl_sql_statement( )
                                                                                                                                                  i_association_processor = assoc_proc )
                                                                                                      field_with_attr_w_alias = new sql_statement( i_sql_statement = `SELECT _assoc1[ asdfa kljkl ]._assoc2[ asdfa kljkl ].field AS f,`
                                                                                                                                                   i_processor = new cl_sql_statement( )
                                                                                                                                                   i_association_processor = assoc_proc )
                                                                                                      qualified_field_w_attr_w_alias = new sql_statement( i_sql_statement = `SELECT entity._assoc1[ asdfa kljkl ]._assoc2[ asdfa kljkl ].field AS f,`
                                                                                                                                                          i_processor = new cl_sql_statement( )
                                                                                                                                                          i_association_processor = assoc_proc )
                                                                                                      field_with_several_assocs = new sql_statement( i_sql_statement = `SELECT _assoc1._assoc2._assoc3._assoc4.field,`
                                                                                                                                                     i_processor = new cl_sql_statement( )
                                                                                                                                                     i_association_processor = assoc_proc ) ) ) )
                              delimited = value #( basic = value #( field = new sql_statement( i_sql_statement = `SELECT "field",`
                                                                                               i_processor = new cl_sql_statement( )
                                                                                               i_association_processor = assoc_proc )
                                                                    field_w_alias = new sql_statement( i_sql_statement = `SELECT "field" AS "f",`
                                                                                                       i_processor = new cl_sql_statement( )
                                                                                                       i_association_processor = assoc_proc )
                                                                    qualified_field = new sql_statement( i_sql_statement = `SELECT "dbtab"."field",`
                                                                                                         i_processor = new cl_sql_statement( )
                                                                                                         i_association_processor = assoc_proc )
                                                                    qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT "dbtab"."field" AS "f",`
                                                                                                                 i_processor = new cl_sql_statement( )
                                                                                                                 i_association_processor = assoc_proc ) )
                                                   path_expressions = value #( simple = value #( field = new sql_statement( i_sql_statement = `SELECT "_assoc"."field",`
                                                                                                                            i_processor = new cl_sql_statement( )
                                                                                                                            i_association_processor = assoc_proc )
                                                                                                 field_w_alias = new sql_statement( i_sql_statement = `SELECT "_assoc"."field" AS "f",`
                                                                                                                                    i_processor = new cl_sql_statement( )
                                                                                                                                    i_association_processor = assoc_proc )
                                                                                                 qualified_field = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc"."field",`
                                                                                                                                      i_processor = new cl_sql_statement( )
                                                                                                                                      i_association_processor = assoc_proc )
                                                                                                 qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc"."field" AS "f",`
                                                                                                                                              i_processor = new cl_sql_statement( )
                                                                                                                                              i_association_processor = assoc_proc )
                                                                                                 field_with_attr = new sql_statement( i_sql_statement = `SELECT "_assoc"[ asdfa kljkl ]."field",`
                                                                                                                                      i_processor = new cl_sql_statement( )
                                                                                                                                      i_association_processor = assoc_proc )
                                                                                                 qualified_field_w_attr = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc"[ asdfa kljkl ]."field" AS "f",`
                                                                                                                                             i_processor = new cl_sql_statement( )
                                                                                                                                             i_association_processor = assoc_proc )
                                                                                                 field_with_attr_w_alias = new sql_statement( i_sql_statement = `SELECT "_assoc"[ asdfa kljkl ]."field" AS "f",`
                                                                                                                                              i_processor = new cl_sql_statement( )
                                                                                                                                              i_association_processor = assoc_proc )
                                                                                                 qualified_field_w_attr_w_alias = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc"[ asdfa kljkl ]."field" AS "f",`
                                                                                                                                                     i_processor = new cl_sql_statement( )
                                                                                                                                                     i_association_processor = assoc_proc ) )
                                                                               complex = value #( field = new sql_statement( i_sql_statement = `SELECT "_assoc1"."_assoc2"."field",`
                                                                                                                             i_processor = new cl_sql_statement( )
                                                                                                                             i_association_processor = assoc_proc )
                                                                                                  field_w_alias = new sql_statement( i_sql_statement = `SELECT "_assoc1"."_assoc2"."field" AS "f",`
                                                                                                                                     i_processor = new cl_sql_statement( )
                                                                                                                                     i_association_processor = assoc_proc )
                                                                                                  qualified_field = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc1"."_assoc2"."field",`
                                                                                                                                       i_processor = new cl_sql_statement( )
                                                                                                                                       i_association_processor = assoc_proc )
                                                                                                  qualified_field_w_alias = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc1"."_assoc2"."field" AS "f",`
                                                                                                                                               i_processor = new cl_sql_statement( )
                                                                                                                                               i_association_processor = assoc_proc )
                                                                                                  field_with_attr = new sql_statement( i_sql_statement = `SELECT "_assoc1"[ asdfa kljkl ]."_assoc2"[ asdfa kljkl ]."field",`
                                                                                                                                       i_processor = new cl_sql_statement( )
                                                                                                                                       i_association_processor = assoc_proc )
                                                                                                  qualified_field_w_attr = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc1"[ asdfa kljkl ]."_assoc2"[ asdfa kljkl ]."field",`
                                                                                                                                              i_processor = new cl_sql_statement( )
                                                                                                                                              i_association_processor = assoc_proc )
                                                                                                  field_with_attr_w_alias = new sql_statement( i_sql_statement = `SELECT "_assoc1"[ asdfa kljkl ]."_assoc2"[ asdfa kljkl ]."field" AS "f",`
                                                                                                                                               i_processor = new cl_sql_statement( )
                                                                                                                                               i_association_processor = assoc_proc )
                                                                                                  qualified_field_w_attr_w_alias = new sql_statement( i_sql_statement = `SELECT "entity"."_assoc1"[ asdfa kljkl ]."_assoc2"[ asdfa kljkl ]."field" AS "f",`
                                                                                                                                                      i_processor = new cl_sql_statement( )
                                                                                                                                                      i_association_processor = assoc_proc )
                                                                                                  field_with_several_assocs = new sql_statement( i_sql_statement = `SELECT "_assoc1"."_assoc2"."_assoc3"."_assoc4"."field",`
                                                                                                                                                 i_processor = new cl_sql_statement( )
                                                                                                                                                 i_association_processor = assoc_proc ) ) ) ) ).

  endmethod.
  method assert_gt_1_assoc_throws_exc.

    data(type) = cl_abap_typedescr=>describe_by_data( i_previous ).

    if type->kind eq cl_abap_typedescr=>kind_ref.

      try.

        "act
        cast sql_statement( i_previous )->sole_associaton_if_any( ).

        "assert
        cl_abap_unit_assert=>fail( msg = |{ i_name }->SOLE_ASSOCIATON_IF_ANY( ) should throw an exception|
                                   quit = if_abap_unit_constant=>quit-no ).

      catch cx_sql_exception ##NO_HANDLER.

      endtry.

    else.

      loop at cast cl_abap_structdescr( type )->get_components( ) reference into data(component).

        assign component component->*-name of structure i_previous to field-symbol(<component>).

        assert_gt_1_assoc_throws_exc( i_previous = <component>
                                      i_name = |{ i_name }-{ component->*-name }| ).

      endloop.

    endif.

  endmethod.
  method assert_le_1_assoc_no_exc.

    data(type) = cl_abap_typedescr=>describe_by_data( i_previous ).

    if type->kind eq cl_abap_typedescr=>kind_ref.

      try.

        "act
        cast sql_statement( i_previous )->sole_associaton_if_any( ).

      catch cx_sql_exception.

        "assert
        cl_abap_unit_assert=>fail( msg = |{ i_name }->SOLE_ASSOCIATON_IF_ANY( ) should not throw an exception|
                                   quit = if_abap_unit_constant=>quit-no ).

      endtry.

    else.

      loop at cast cl_abap_structdescr( type )->get_components( ) reference into data(component).

        assign component component->*-name of structure i_previous to field-symbol(<component>).

        assert_le_1_assoc_no_exc( i_previous = <component>
                                  i_name = |{ i_name }-{ component->*-name }| ).

      endloop.

    endif.

  endmethod.
  method assert_1_assoc_returned.

    data(type) = cl_abap_typedescr=>describe_by_data( i_previous ).

    if type->kind eq cl_abap_typedescr=>kind_ref.

      "act & assert
      cl_abap_unit_assert=>assert_not_initial( act = cast sql_statement( i_previous )->sole_associaton_if_any( )
                                               msg = |{ i_name }->SOLE_ASSOCIATON_IF_ANY( ) should return the found path expression|
                                               quit = if_abap_unit_constant=>quit-no ).

    else.

      loop at cast cl_abap_structdescr( type )->get_components( ) reference into data(component).

        assign component component->*-name of structure i_previous to field-symbol(<component>).

        assert_1_assoc_returned( i_previous = <component>
                                 i_name = |{ i_name }-{ component->*-name }| ).

      endloop.

    endif.

  endmethod.
  method assert_le_1_assoc_rets_empty.

    data(type) = cl_abap_typedescr=>describe_by_data( i_previous ).

    if type->kind eq cl_abap_typedescr=>kind_ref.

      "act & assert
      cl_abap_unit_assert=>assert_initial( act = cast sql_statement( i_previous )->sole_associaton_if_any( )
                                           msg = |{ i_name }->SOLE_ASSOCIATON_IF_ANY( ) should return an empty string|
                                           quit = if_abap_unit_constant=>quit-no ).

    else.

      loop at cast cl_abap_structdescr( type )->get_components( ) reference into data(component).

        assign component component->*-name of structure i_previous to field-symbol(<component>).

        assert_le_1_assoc_rets_empty( i_previous = <component>
                                      i_name = |{ i_name }-{ component->*-name }| ).

      endloop.

    endif.

  endmethod.
  method sole_assoc_throws_for_non_u_pe.

    "arrange
    data(statements) = value string_table( ( `SELECT entity._assocA.fieldA1, "_assocB"."fieldB1",` )
                                           ( `SELECT entity._assocA[ asdf ].fieldA1 AS f, _assocB.fieldB1,` )
                                           ( `SELECT entity._assocA.fieldA1, entity.field, entity._assocB.fieldB1,` )
                                           ( `SELECT entity._assocA.fieldA1, entity._assocA[ inner where ].fieldA2,` ) ).

    loop at statements reference into data(statement).

      try.

        "act
        new sql_statement( i_sql_statement = statement->*
                           i_association_processor = new zcl_association_processor( )
                           i_processor = new cl_sql_statement( ) )->sole_associaton_if_any( ).

        "assert
        cl_abap_unit_assert=>fail( quit = if_abap_unit_constant=>quit-no
                                   msg = |'{ statement->* }' has more than one unique path expression and it should throw an exception| ).

      catch cx_sql_exception ##NO_HANDLER.

      endtry.

    endloop.

  endmethod.
  method sole_assoc_no_exc_for_u_pe.

    "arrange
    data(statements) = value string_table( ( `SELECT entity._assocA.fieldA1, "_assocA"."fieldA2",` )
                                           ( `SELECT _assocA.fieldA1 AS f, entity._assocA.fieldA2,` )
                                           ( `SELECT _assocA.field,` )
                                           ( `SELECT field,` )
                                           ( `SELECT entity._assocA.fieldA1, entity.field, entity._assocA.fieldA2,` )
                                           ( `SELECT entity._assocA[ inner where ].fieldA1, entity._assocA[ inner where ].fieldA2,` ) ).

    loop at statements reference into data(statement).

      try.

        "act
        new sql_statement( i_sql_statement = statement->*
                           i_association_processor = new zcl_association_processor( )
                           i_processor = new cl_sql_statement( ) )->sole_associaton_if_any( ).

      catch cx_sql_exception.

        "assert
        cl_abap_unit_assert=>fail( quit = if_abap_unit_constant=>quit-no
                                   msg = |'{ statement->* }' has only one unique path expression and should not throw exception| ).

      endtry.

    endloop.

  endmethod.
  method with_row_cap_does_nothing.

    "arrange
    data(assoc_proc) = new zcl_association_processor( ).

    data(statements) = value string_table( ( `SELECT TOP 1 * FROM dbtab;` )
                                           ( |SELECT{ cl_abap_char_utilities=>cr_lf }TOP{ cl_abap_char_utilities=>cr_lf }1{ cl_abap_char_utilities=>cr_lf }*{ cl_abap_char_utilities=>cr_lf }FROM dbtab| )
                                           ( `SELECT * FROM dbtab LIMIT 1;` )
                                           ( |SELECT * FROM dbtab{ cl_abap_char_utilities=>cr_lf }LIMIT{ cl_abap_char_utilities=>cr_lf }1| ) ).

    loop at statements reference into data(statement).

      "act & assert
      cl_abap_unit_assert=>assert_equals( act = cast _sql_statement_t_double( new _sql_statement_t_double( i_sql_statement = statement->*
                                                                                                           i_processor = new cl_sql_statement( )
                                                                                                           i_association_processor = assoc_proc )->add_limit_if_none( 1 ) )->value( )
                                          exp = cast _sql_statement_t_double( new _sql_statement_t_double( i_sql_statement = statement->*
                                                                                                           i_processor = new cl_sql_statement( )
                                                                                                           i_association_processor = assoc_proc ) )->value( )
                                          quit = if_abap_unit_constant=>quit-no
                                          msg = |'{ statement->* }' modified unnecessarily| ).

    endloop.

  endmethod.
  method without_row_cap_adds_limit.

    "arrange
    data(statements) = value string_table( ( `SELECT * FROM dbtab;` )
                                           ( |SELECT{ cl_abap_char_utilities=>cr_lf }*{ cl_abap_char_utilities=>cr_lf }FROM{ cl_abap_char_utilities=>cr_lf }dbtab{ cl_abap_char_utilities=>cr_lf }| ) ).

    loop at statements reference into data(statement).

      data(max_no_of_rows) = cl_abap_random_int=>create( seed = conv #( cl_abap_context_info=>get_system_time( ) )
                                                         min = 1 )->get_next( ).

      "act & assert
      cl_abap_unit_assert=>assert_char_cp( act = cast _sql_statement_t_double( new _sql_statement_t_double( i_sql_statement = statement->*
                                                                                                            i_association_processor = new zcl_association_processor( )
                                                                                                            i_processor = new cl_sql_statement( ) )->add_limit_if_none( max_no_of_rows ) )->value( )
                                           exp = |* LIMIT { max_no_of_rows };|
                                           quit = if_abap_unit_constant=>quit-no
                                           msg = |'{ statement->* }' should have added a LIMIT clause| ).

    endloop.

  endmethod.
  method execute_calls_methods.

    "arrange
    data(processor) = new _cl_sql_statement_spy( ).

    data(statement) = new _sql_statement_spy( i_processor = processor
                                              i_association_processor = new zcl_association_processor( )
                                              i_sql_statement = `test` ).

    "act
    statement->execute( ).

    "assert
    cl_abap_unit_assert=>assert_true( act = xsdbool( statement->sole_assoc_out eq statement->generated_join_in
                                                     and ( statement->sole_assoc_out eq statement->assoc_mapped_in_1 and statement->generated_join_out eq statement->assoc_mapped_in_2 )
                                                     and statement->assoc_mapped_out eq processor->execute_query_in )
                                      msg = `ASSOC_MAPPED_TO_JOIN_IF_ANY should use result of GENERATED_JOIN_FOR_ASSOCIATION, which should in turn use result of SOLE_ASSOCIATON_IF_ANY` ).

  endmethod.
  method execute_uses_original.

    "arrange
    data(original_statement) = `select 1 as "Test" from DUMMY;`.

    data(processor) = new _cl_sql_statement_spy( ).

    data(statement) = new _sql_statement_stub( i_processor = processor
                                               i_association_processor = new association_processor_dummy( )
                                               i_sql_statement = original_statement
                                               i_assoc_mapped_return = value #( ) ).

    "act
    statement->execute( ).

    "assert
    cl_abap_unit_assert=>assert_equals( act = processor->execute_query_in
                                        exp = original_statement
                                        msg = `Since ASSOC_MAPPED_TO_JOIN_IF_ANY returned empty, we use the original statement` ). "this way we let it crash if it was wrong

  endmethod.
  method generated_join_calls_methods.

    "arrange
    data(assoc_proc) = new association_processor_spy( ).

    data(data_sources) = value string_table( ( `6` )
                                             ( `J` ) ).

    data(statement) = new _sql_statement_stub( i_processor = value #( )
                                               i_association_processor = assoc_proc
                                               i_sql_statement = `test`
                                               i_data_sources = data_sources ).

    data(association) = `asdfasdfasdf`.

    "act
    statement->generated_join_for_association( association ).

    "assert
    cl_abap_unit_assert=>assert_true( act = xsdbool( assoc_proc->association eq association
                                                     and assoc_proc->data_sources eq conv zif_association_processor=>t_data_sources( data_sources ) )
                                      msg = `GENERATED_JOIN_FOR_ASSOCIATION calls AN_ASSOCIATION_PROCESSOR->MAP_ASSOCIATION_TO_JOIN using the result of DATA_SOURCES` ).

  endmethod.
  method sole_assoc_throws_for_compl_pe.

    assert_gt_1_assoc_throws_exc( i_previous = me->statements-delimited-path_expressions-complex
                                  i_name = `STATEMENTS-DELIMITED-PATH_EXPRESSIONS-COMPLEX` ).

    assert_gt_1_assoc_throws_exc( i_previous = me->statements-non_delimited-path_expressions-complex
                                  i_name = `STATEMENTS-NON_DELIMITED-PATH_EXPRESSIONS-COMPLEX` ).

  endmethod.
  method sole_assoc_no_exc_for_simpl_pe.

    assert_le_1_assoc_no_exc( i_previous = me->statements-delimited-basic
                              i_name = `STATEMENTS-DELIMITED-BASIC` ).

    assert_le_1_assoc_no_exc( i_previous = me->statements-non_delimited-basic
                              i_name = `STATEMENTS-NON_DELIMITED-BASIC` ).

    assert_le_1_assoc_no_exc( i_previous = me->statements-delimited-path_expressions-simple
                              i_name = `STATEMENTS-DELIMITED-PATH_EXPRESSIONS-SIMPLE` ).

    assert_le_1_assoc_no_exc( i_previous = me->statements-non_delimited-path_expressions-simple
                              i_name = `STATEMENTS-NON_DELIMITED-PATH_EXPRESSIONS-SIMPLE` ).

  endmethod.
  method sole_assoc_returns_u_pe.

    assert_1_assoc_returned( i_previous = me->statements-delimited-path_expressions-simple
                             i_name = `STATEMENTS-DELIMITED-PATH_EXPRESSIONS-SIMPLE` ).

    assert_1_assoc_returned( i_previous = me->statements-non_delimited-path_expressions-simple
                             i_name = `STATEMENTS-NON_DELIMITED-PATH_EXPRESSIONS-SIMPLE` ).

  endmethod.
  method sole_assoc_ret_empty_for_no_pe.

    assert_le_1_assoc_rets_empty( i_previous = me->statements-delimited-basic
                                  i_name = `STATEMENTS-DELIMITED-BASIC` ).

    assert_le_1_assoc_rets_empty( i_previous = me->statements-non_delimited-basic
                                  i_name = `STATEMENTS-NON_DELIMITED-BASIC` ).

  endmethod.
  method data_sources_returned.

    types: begin of statement,
             val type string,
             ds type string_table,
           end of statement,
           statements type standard table of statement with empty key.

    "arrange
    data(statements) = value statements( ( val = `SELECT * FROM dbtab1;`
                                           ds = value #( ( `dbtab1` ) ) )
                                         ( val = |SELECT{ cl_abap_char_utilities=>cr_lf }*{ cl_abap_char_utilities=>cr_lf }FROM{ cl_abap_char_utilities=>cr_lf }dbtab2{ cl_abap_char_utilities=>cr_lf }|
                                           ds = value #( ( `dbtab2` ) ) )
                                         ( val = |SELECT * FROM schema.dbtab3|
                                           ds = value #( ( `dbtab3` ) ) )
                                         ( val = |SELECT * FROM "schema"."dbtab4A" INNER JOIN "dbtab4B"|
                                           ds = value #( ( `dbtab4A` )
                                                         ( `dbtab4B` ) ) )
                                         ( val = |WITH aux AS ( SELECT * FROM "dbtab5A" ) SELECT * FROM aux INNER JOIN schema."dbtab5B"|
                                           ds = value #( ( `dbtab5A` )
                                                         ( `aux` ) "technically not a ds that we care about buuuuuuuuut it shouldn't matter for the use case
                                                         ( `dbtab5B` ) ) ) ).

    loop at statements reference into data(statement).

      "act & assert
      cl_abap_unit_assert=>assert_equals( act = new sql_statement( i_sql_statement = statement->*-val
                                                                   i_association_processor = new zcl_association_processor( )
                                                                   i_processor = new cl_sql_statement( ) )->data_sources( )
                                          exp = statement->*-ds
                                          quit = if_abap_unit_constant=>quit-no
                                          msg = |'{ statement->*-val }' has datasources: { concat_lines_of( table = statement->*-ds
                                                                                                            sep = `, ` ) }| ).

    endloop.

  endmethod.
  method assoc_mapping_added.

    types: begin of statement,
             original type string,
             association type string,
             join_fragment type string,
             mapped type string,
           end of statement,
           statements type standard table of statement with empty key.

    "arrange
    data(join_clause) = `LEFT OUTER JOIN ON 1 = 1`.

    data(statements) = value statements( ( original = `SELECT * FROM dbt`
                                           mapped = value #( ) )
                                         ( original = `SELECT "_assoc"."field" FROM entity`
                                           association = `"_assoc"`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0"."field" FROM entity { join_clause }| )
                                         ( original = `SELECT _assoc[ attrs ].field FROM entity WHERE _assoc[ attrs ].field IS NOT NULL;`
                                           association = `_assoc[ attrs ]`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } WHERE "=A0".field IS NOT NULL;| )
                                         ( original = `SELECT _assoc.field FROM entity GROUP BY _assoc.field`
                                           association = `_assoc`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } GROUP BY "=A0".field| )
                                         ( original = `SELECT _assoc[ attrs ].field FROM entity HAVING _assoc[ attrs ].field > 0`
                                           association = `_assoc[ attrs ]`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } HAVING "=A0".field > 0| )
                                         ( original = `SELECT _assoc."field" FROM entity ORDER BY _assoc.field`
                                           association = `_assoc`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0"."field" FROM entity { join_clause } ORDER BY "=A0".field| )
                                         ( original = `SELECT _assoc.field FROM entity LIMIT 5;`
                                           association = `_assoc`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } LIMIT 5;| )
                                         ( original = `SELECT "_assoc".field FROM entity WITH HINT( INDEX_JOIN )`
                                           association = `"_assoc"`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } WITH HINT( INDEX_JOIN )| )
                                         ( original = `SELECT _assoc.field FROM entity WhErE _assoc.field IS NOT NULL gRoUp BY _assoc.field HAviNG cOUNt(*) > 0`
                                           association = `_assoc`
                                           join_fragment = join_clause
                                           mapped = |SELECT "=A0".field FROM entity { join_clause } WhErE "=A0".field IS NOT NULL gRoUp BY "=A0".field HAviNG cOUNt(*) > 0| ) ).

    loop at statements reference into data(statement).

      "act & assert
      cl_abap_unit_assert=>assert_equals( act = new _sql_statement_stub( i_sql_statement = statement->*-original
                                                                         i_association_processor = new zcl_association_processor( )
                                                                         i_processor = new cl_sql_statement( ) )->assoc_mapped_to_join_if_any( i_association = statement->*-association
                                                                                                                                               i_join = statement->*-join_fragment )
                                          exp = statement->*-mapped
                                          quit = if_abap_unit_constant=>quit-no
                                          msg = `Original: ` && statement->*-original ).

    endloop.

  endmethod.

endclass.
