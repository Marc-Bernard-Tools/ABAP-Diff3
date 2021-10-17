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
      mi_diff3 TYPE REF TO zif_differ_diff3.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_lcs IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = NEW zcl_differ_diff3( ).
  ENDMETHOD.

  METHOD test.

    " returns the LCS of two arrays
    DATA(lt_lcs) = mi_diff3->lcs(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).

    DATA(ls_result) = lt_lcs[ key = lines( lt_lcs ) - 1 ].

    " '99'
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 9 ).

    " 'M'
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 9 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 6 ).

    " 'ZZ'
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 4 ).

    " 'c'
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 3 ).

    " 'a'
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 1 ).

    " 'AA'
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 0 ).

    " end
    ls_result = lt_lcs[ key = ls_result-chain ].

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = -1 ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffComm.test.js
**********************************************************************
CLASS ltcl_diff_comm DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_differ_diff3.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_comm IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = NEW zcl_differ_diff3( ).
  ENDMETHOD.

  METHOD test.

    " returns a comm-style diff of two arrays
    DATA(lt_result) = mi_diff3->diff_comm(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-common )
      exp = 'AA a' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 1 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-diff-buffer1 )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-diff-buffer2 )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-common )
      exp = 'c ZZ' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 3 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 4 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-diff-buffer1 )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-diff-buffer2 )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 5 ]-common )
      exp = 'M' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 5 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 6 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 6 ]-diff-buffer1 )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 6 ]-diff-buffer2 )
      exp = 'z z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 7 ]-common )
      exp = '99' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 7 ]-diff ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffIndices.test.js
**********************************************************************
CLASS ltcl_diff_indices DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_differ_diff3.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_indices IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = NEW zcl_differ_diff3( ).
  ENDMETHOD.

  METHOD test.

    " returns array indices for differing regions of two arrays
    DATA(lt_result) = mi_diff3->diff_indices(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-key
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-buffer1content )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-key
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-buffer2content )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-key
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-len
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-buffer1content )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-key
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-buffer2content )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-key
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-len
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-buffer1content )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-key
      exp = 7 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-len
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-buffer2content )
      exp = 'z z' ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Diff ABAP Code
**********************************************************************
CLASS ltcl_abap_code DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mt_old   TYPE string_table,
      mt_new   TYPE string_table,
      mi_diff3 TYPE REF TO zif_differ_diff3.

    METHODS:
      setup,
      diff_comm FOR TESTING,
      diff_indices FOR TESTING.

ENDCLASS.

CLASS ltcl_abap_code IMPLEMENTATION.

  METHOD setup.

    mi_diff3 = NEW zcl_differ_diff3( ).

    DATA(lv_old) = `REPORT z_differ_test_prog.\n`
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

    DATA(lv_new) = `REPORT z_differ_test_prog.\n`
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

    mt_old = lcl_helper=>split( lv_old ).
    mt_new = lcl_helper=>split( lv_new ).

  ENDMETHOD.

  METHOD diff_comm.

    DATA(lt_result) = mi_diff3->diff_comm(
      it_buffer1 = mt_old
      it_buffer2 = mt_new ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 1 ]-common )
      exp = 3 ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 1 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 2 ]-diff-buffer1 )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 2 ]-diff-buffer2 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 3 ]-common )
      exp = 2 ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 3 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 4 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 4 ]-diff-buffer1 )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 4 ]-diff-buffer2 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 5 ]-common )
      exp = 2 ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 5 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 6 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 6 ]-diff-buffer1 )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 6 ]-diff-buffer2 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 7 ]-common )
      exp = 9 ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 7 ]-diff ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 8 ]-common ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 8 ]-diff-buffer1 )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result[ 8 ]-diff-buffer2 )
      exp = 1 ).

  ENDMETHOD.

  METHOD diff_indices.

    DATA(lt_result) = mi_diff3->diff_indices(
      it_buffer1 = mt_old
      it_buffer2 = mt_new ).

  ENDMETHOD.
ENDCLASS.
