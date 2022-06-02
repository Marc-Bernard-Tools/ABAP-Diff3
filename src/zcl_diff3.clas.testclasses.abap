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

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_lcs IMPLEMENTATION.

  METHOD test.

    " returns the LCS of two arrays
    DATA(lt_lcs) = zcl_diff3=>create( )->lcs(
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

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_comm IMPLEMENTATION.

  METHOD test.

    " returns a comm-style diff of two arrays
    DATA(lt_result) = zcl_diff3=>create( )->diff_comm(
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

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_indices IMPLEMENTATION.

  METHOD test.

    " returns array indices for differing regions of two arrays
    DATA(lt_result) = zcl_diff3=>create( )->diff_indices(
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
* https://github.com/bhousel/node-diff3/blob/main/test/diffPatch.test.js
**********************************************************************
CLASS ltcl_diff_patch DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_diff_patch FOR TESTING,
      test_patch FOR TESTING,
      test_strip_patch FOR TESTING,
      test_invert_patch FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_patch IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).

    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).
  ENDMETHOD.

  METHOD test_diff_patch.

    " returns a patch-style diff of two arrays
    DATA(lt_result) = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-offset
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-buffer1-chunk )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-offset
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-buffer2-chunk )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-offset
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-length
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-buffer1-chunk )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-offset
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-buffer2-chunk )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-offset
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-length
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-buffer1-chunk )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-offset
      exp = 7 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-length
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-buffer2-chunk )
      exp = 'z z' ).

  ENDMETHOD.

  METHOD test_patch.

    " applies a patch against buffer1 to get buffer2
    DATA(lt_patch) = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).

    DATA(lt_result) = mi_diff3->patch(
      it_buffer   = mt_a
      it_patchres = lt_patch ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_b ).

  ENDMETHOD.

  METHOD test_strip_patch.

    " removes extra information from the diffPatch result
    DATA(lt_patch) = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).

    DATA(lt_strip) = mi_diff3->strip_patch( lt_patch ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 1 ]-buffer1-offset
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 1 ]-buffer1-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 1 ]-buffer1-chunk ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 1 ]-buffer2-offset ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 1 ]-buffer2-length ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_strip[ 1 ]-buffer2-chunk )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 2 ]-buffer1-offset
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 2 ]-buffer1-length
      exp = 4 ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 2 ]-buffer1-chunk ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 2 ]-buffer2-offset ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 2 ]-buffer2-length ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_strip[ 2 ]-buffer2-chunk )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 3 ]-buffer1-offset
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_strip[ 3 ]-buffer1-length
      exp = 0 ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 3 ]-buffer1-chunk ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 3 ]-buffer2-offset ).
    cl_abap_unit_assert=>assert_initial( lt_strip[ 3 ]-buffer2-length ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_strip[ 3 ]-buffer2-chunk )
      exp = 'z z' ).

    " applies a stripped patch against buffer1 to get buffer2
    DATA(lt_result) = mi_diff3->patch(
      it_buffer   = mt_a
      it_patchres = lt_strip ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_b ).

  ENDMETHOD.

  METHOD test_invert_patch.

    " inverts the diffPatch result
    DATA(lt_patch) = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).

    DATA(lt_invert) = mi_diff3->invert_patch( lt_patch ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 1 ]-buffer2-offset
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 1 ]-buffer2-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 1 ]-buffer2-chunk )
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 1 ]-buffer1-offset
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 1 ]-buffer1-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 1 ]-buffer1-chunk )
      exp = 'd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 2 ]-buffer2-offset
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 2 ]-buffer2-length
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 2 ]-buffer2-chunk )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 2 ]-buffer1-offset
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 2 ]-buffer1-length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 2 ]-buffer1-chunk )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 3 ]-buffer2-offset
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 3 ]-buffer2-length
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 3 ]-buffer2-chunk )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 3 ]-buffer1-offset
      exp = 7 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_invert[ 3 ]-buffer1-length
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_invert[ 3 ]-buffer1-chunk )
      exp = 'z z' ).

    " applies a stripped patch against buffer1 to get buffer2
    DATA(lt_result) = mi_diff3->patch(
      it_buffer   = mt_b
      it_patchres = lt_invert ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_a ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diff3MergeRegions.test.js
**********************************************************************
CLASS ltcl_diff3_merge_regions DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff3_merge_regions IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).
  ENDMETHOD.

  METHOD test.

    " returns results of 3-way diff from o,a,b arrays
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).

    DATA(lt_result) = mi_diff3->diff3_merge_regions(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-stable
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-stable_region-buffer
      exp = 'o' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-stable_region-buffer_start
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-stable_region-buffer_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-stable_region-buffer_content )
      exp = 'AA' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-stable
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-a_start
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-a_length
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-unstable_region-a_content )
      exp = 'a b c' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-o_start
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-o_length
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-unstable_region-o_content )
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-b_start
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-unstable_region-b_length
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-unstable_region-b_content )
      exp = 'a d c' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-stable
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-stable_region-buffer
      exp = 'o' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-stable_region-buffer_start
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-stable_region-buffer_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-stable_region-buffer_content )
      exp = 'ZZ' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-stable
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-a_start
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-a_length
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-unstable_region-a_content )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-o_start
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-o_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-unstable_region-o_content )
      exp = '00' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-b_start
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-unstable_region-b_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-unstable_region-b_content )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 5 ]-stable
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 5 ]-stable_region-buffer
      exp = 'o' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 5 ]-stable_region-buffer_start
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 5 ]-stable_region-buffer_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 5 ]-stable_region-buffer_content )
      exp = 'M' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 6 ]-stable
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 6 ]-stable_region-buffer
      exp = 'b' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 6 ]-stable_region-buffer_start
      exp = 7 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 6 ]-stable_region-buffer_length
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 6 ]-stable_region-buffer_content )
      exp = 'z z' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 7 ]-stable
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 7 ]-stable_region-buffer
      exp = 'o' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 7 ]-stable_region-buffer_start
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 7 ]-stable_region-buffer_length
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 7 ]-stable_region-buffer_content )
      exp = '99' ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diff3Merge.test.js
**********************************************************************
CLASS ltcl_diff3_merge DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test FOR TESTING,
      exclude_false_conflicts FOR TESTING,
      include_false_conflicts FOR TESTING.

ENDCLASS.

CLASS ltcl_diff3_merge IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).
  ENDMETHOD.

  METHOD test.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).

    DATA(lt_result) = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

* AA
* <<<<<<< a
* a
* b
* c
* ||||||| o
* =======
* a
* d
* c
* >>>>>>> b
* ZZ
* <<<<<<< a
* new
* 00
* a
* a
* ||||||| o
* 00
* =======
* 11
* >>>>>>> b
* M
* z
* z
* 99

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-ok )
      exp = 'AA' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 1 ]-conflict ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-ok ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-conflict-o ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-conflict-a )
      exp = 'a b c' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-conflict-b )
      exp = 'a d c' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-ok )
      exp = 'ZZ' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 3 ]-conflict ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 4 ]-ok ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-conflict-o )
      exp = '00' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-conflict-a )
      exp = 'new 00 a a' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 4 ]-conflict-b )
      exp = '11' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 5 ]-ok )
      exp = 'M z z 99' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 5 ]-conflict ).

  ENDMETHOD.

  METHOD exclude_false_conflicts.

    " excludes false conflicts by default
    mt_o = lcl_helper=>split( 'AA ZZ' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ' ).
    mt_b = lcl_helper=>split( 'AA a b c ZZ' ).

    DATA(lt_result) = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-ok )
      exp = 'AA a b c ZZ' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 1 ]-conflict ).

  ENDMETHOD.

  METHOD include_false_conflicts.

    " can include false conflicts with option
    mt_o = lcl_helper=>split( 'AA ZZ' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ' ).
    mt_b = lcl_helper=>split( 'AA a b c ZZ' ).

    DATA(lt_result) = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b
      iv_exclude_false_conflicts = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 1 ]-ok )
      exp = 'AA' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 1 ]-conflict ).

    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-ok ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 2 ]-conflict-o ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-conflict-a )
      exp = 'a b c' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 2 ]-conflict-b )
      exp = 'a b c' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( lt_result[ 3 ]-ok )
      exp = 'ZZ' ).
    cl_abap_unit_assert=>assert_initial( lt_result[ 3 ]-conflict ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/merge.test.js
**********************************************************************
CLASS ltcl_merge DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).

    DATA(ls_result) = mi_diff3->merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).

    DATA(ls_result) = mi_diff3->merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    DATA(lt_exp) = lcl_helper=>split(
      'AA\n<<<<<<<\na\nb\nc\n=======\na\nd\nc\n' &&
      '>>>>>>>\nZZ\n<<<<<<<\nnew\n00\na\na\n' &&
      '=======\n11\n>>>>>>>\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/mergeDiff3.test.js
**********************************************************************
CLASS ltcl_merge_diff3 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge_diff3 IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).

    DATA(ls_result) = mi_diff3->merge_diff3(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).

    DATA(ls_labels) = VALUE zif_diff3=>ty_labels(
      a = 'a'
      b = 'b'
      o = 'o' ).

    DATA(ls_result) = mi_diff3->merge_diff3(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b
      is_labels = ls_labels ).

    DATA(lt_exp) = lcl_helper=>split(
      'AA\n<<<<<<< a\na\nb\nc\n||||||| o\n=======\na\nd\nc\n' &&
      '>>>>>>> b\nZZ\n<<<<<<< a\nnew\n00\na\na\n' &&
      '||||||| o\n00\n=======\n11\n>>>>>>> b\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/mergeDigIn.test.js
**********************************************************************
CLASS ltcl_merge_dig_in DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge_dig_in IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).

    DATA(ls_result) = mi_diff3->merge_dig_in(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).

    DATA(ls_result) = mi_diff3->merge_dig_in(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    DATA(lt_exp) = lcl_helper=>split(
      'AA\na\n<<<<<<<\nb\n=======\nd\n' &&
      '>>>>>>>\nc\nZZ\n<<<<<<<\nnew\n00\na\na\n' &&
      '=======\n11\n>>>>>>>\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

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
      mi_diff3 TYPE REF TO zif_diff3.

    METHODS:
      setup,
      diff_comm FOR TESTING,
      diff_indices FOR TESTING.

ENDCLASS.

CLASS ltcl_abap_code IMPLEMENTATION.

  METHOD setup.

    mi_diff3 = zcl_diff3=>create( ).

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

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-key
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer1-len
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-key
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-buffer2-len
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-key
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer1-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-key
      exp = 6 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 2 ]-buffer2-len
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-key
      exp = 8 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer1-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-key
      exp = 9 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 3 ]-buffer2-len
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-buffer1-key
      exp = 18 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-buffer1-len
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-buffer2-key
      exp = 18 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 4 ]-buffer2-len
      exp = 1 ).

  ENDMETHOD.
ENDCLASS.
