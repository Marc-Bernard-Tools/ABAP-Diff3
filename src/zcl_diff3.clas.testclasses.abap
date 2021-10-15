************************************************************************
* Helper Class
************************************************************************

CLASS lcl_helper DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      split
        IMPORTING
          iv_string        TYPE string
        RETURNING
          VALUE(rt_result) TYPE string_table,

      concat
        IMPORTING
          it_strings       TYPE string_table
        RETURNING
          VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD split.
    IF iv_string CS '\n'.
      SPLIT iv_string AT '\n' INTO TABLE rt_result.
    ELSE.
      SPLIT iv_string AT space INTO TABLE rt_result.
    ENDIF.
  ENDMETHOD.

  METHOD concat.
    CONCATENATE LINES OF it_strings INTO rv_result SEPARATED BY space.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/LCS.test.js
**********************************************************************
CLASS ltcl_lcs DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mo_diff3 TYPE REF TO zcl_diff3.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_lcs IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_diff3.

  ENDMETHOD.

  METHOD test.

    DATA lcs TYPE zif_diff3=>ilcsresult_t.
    DATA result TYPE zif_diff3=>ilcsresult.

    " returns the LCS of two arrays
    lcs = mo_diff3->zif_diff3~lcs(
      buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).

    result = lcs[ key = lines( lcs ) - 1 ].

    " '99'
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 9 ).

    " 'M'
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 9 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 6 ).

    " 'ZZ'
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 4 ).

    " 'c'
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 3 ).

    " 'a'
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 1 ).

    " 'AA'
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = 0 ).

    " end
    result = lcs[ key = result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer1index
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = result-buffer2index
      exp = -1 ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffComm.test.js
**********************************************************************
CLASS ltcl_diffcomm DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mo_diff3 TYPE REF TO zcl_diff3.

    METHODS:
      setup,
      test FOR TESTING,
      test_2 FOR TESTING.

ENDCLASS.

CLASS ltcl_diffcomm IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_diff3.

  ENDMETHOD.

  METHOD test.

    DATA result TYPE zif_diff3=>icommresult_t.

    " returns a comm-style diff of two arrays
    result = mo_diff3->zif_diff3~diffcomm(
      buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 1 ]-common )
      exp = 'AA a' ).
    cl_abap_unit_assert=>assert_initial(
      act = result[ 1 ]-diff ).

    cl_abap_unit_assert=>assert_initial(
      act = result[ 2 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 2 ]-diff-buffer1 )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 2 ]-diff-buffer2 )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 3 ]-common )
      exp = 'c ZZ' ).
    cl_abap_unit_assert=>assert_initial(
      act = result[ 3 ]-diff ).

    cl_abap_unit_assert=>assert_initial(
      act = result[ 4 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 4 ]-diff-buffer1 )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 4 ]-diff-buffer2 )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 5 ]-common )
      exp = 'M' ).
    cl_abap_unit_assert=>assert_initial(
      act = result[ 5 ]-diff ).

    cl_abap_unit_assert=>assert_initial(
      act = result[ 6 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 6 ]-diff-buffer1 )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 6 ]-diff-buffer2 )
      exp = 'z z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( result[ 7 ]-common )
      exp = '99' ).
    cl_abap_unit_assert=>assert_initial(
      act = result[ 7 ]-diff ).

  ENDMETHOD.

  METHOD test_2.

    DATA old TYPE string.
    DATA new TYPE string.
    DATA result TYPE zif_diff3=>icommresult_t.

    old = `REPORT z_abapgit_test_prog.\n`
      && `\n`
      && `* next line was added\n`
      && `\n`
      && `* next line was changed\n`
      && `MESSAGE 'changed line' TYPE 'I'.\n`
      && `\n`
      && `* next line was removed\n`
      && `MESSAGE 'removed line' TYPE 'I'.\n`
      && `\n`
      && `* Some comment\n`
      && `" Another comment\n`
      && `DATA variable TYPE string.\n`
      && `\n`
      && `variable = 'some text'. " in-line comment\n`
      && `variable = |some text|.\n`
      && `variable = |some { variable } text|.\n`
      && `\n`
      && `* eof *\n`.

    new = `REPORT z_abapgit_test_prog.\n`
      && `\n`
      && `* next line was added\n`
      && `MESSAGE 'added line' TYPE 'I'.\n`
      && `\n`
      && `* next line was changed\n`
      && `MESSAGE 'changed line' TYPE 'W'.\n`
      && `\n`
      && `* next line was removed\n`
      && `\n`
      && `* Some comment\n`
      && `" Another comment\n`
      && `DATA variable TYPE string.\n`
      && `\n`
      && `variable = 'some text'. " in-line comment\n`
      && `variable = |some text|.\n`
      && `variable = |some { variable } text|.\n`
      && `\n`
      && `* eof **\n`.

    result = mo_diff3->zif_diff3~diffcomm(
      buffer1 = lcl_helper=>split( old )
      buffer2 = lcl_helper=>split( new ) ).

  ENDMETHOD.
ENDCLASS.
