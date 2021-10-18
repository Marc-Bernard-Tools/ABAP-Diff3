CLASS zcl_differ_diff3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* ABAP Differ - Diff3
*
* https://github.com/Marc-Bernard-Tools/ABAP-Differ
*
* This is a port of JavaScript (https://github.com/bhousel/node-diff3, MIT license)
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_differ_diff3.

    CLASS-METHODS convert_to_abap_indices
      CHANGING
        !ct_diff_indices TYPE zif_differ_diff3=>ty_diffindicesresult_t OPTIONAL.

  PROTECTED SECTION.

    METHODS chunk_description
      IMPORTING
        !it_buffer       TYPE string_table
        !iv_offset       TYPE zif_differ_diff3=>ty_number
        !iv_length       TYPE zif_differ_diff3=>ty_number
      RETURNING
        VALUE(rs_result) TYPE zif_differ_diff3=>ty_chunk .
  PRIVATE SECTION.

    METHODS _reverse
      IMPORTING
        !it_data         TYPE string_table
      RETURNING
        VALUE(rt_result) TYPE string_table.
    METHODS _slice
      IMPORTING
        !it_data         TYPE string_table
        !iv_start        TYPE i
        !iv_end          TYPE i
      RETURNING
        VALUE(rt_result) TYPE string_table.

ENDCLASS.



CLASS zcl_differ_diff3 IMPLEMENTATION.


  METHOD chunk_description.

    rs_result-offset = iv_offset.
    rs_result-length = iv_length.

    DO iv_length TIMES.
      APPEND it_buffer[ iv_offset + sy-index ] TO rs_result-chunk.
    ENDDO.

  ENDMETHOD.


  METHOD convert_to_abap_indices.

    LOOP AT ct_diff_indices ASSIGNING FIELD-SYMBOL(<ls_diff_indices>).
      <ls_diff_indices>-buffer1-key = <ls_diff_indices>-buffer1-key + 1.
      <ls_diff_indices>-buffer2-key = <ls_diff_indices>-buffer2-key + 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_differ_diff3~diff_comm.
    " We apply the LCS to build a 'comm'-style picture of the
    " differences between buffer1 and buffer2.

    DATA:
      ls_res       LIKE LINE OF rt_result,
      ls_different TYPE zif_differ_diff3=>ty_commresult-diff,
      lt_common    TYPE zif_differ_diff3=>ty_commresult-common.

    DATA(lt_lcs) = zif_differ_diff3~lcs( it_buffer1 = it_buffer1
                                         it_buffer2 = it_buffer2 ).

    DATA(lv_tail1) = lines( it_buffer1 ).
    DATA(lv_tail2) = lines( it_buffer2 ).

    DATA(ls_candidate) = lt_lcs[ key = lines( lt_lcs ) - 1 ].
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.

      CLEAR ls_different.

      DO.
        lv_tail1 = lv_tail1 - 1.
        IF lv_tail1 <= ls_candidate-buffer1index.
          EXIT.
        ENDIF.
        INSERT it_buffer1[ lv_tail1 + 1 ] INTO TABLE ls_different-buffer1.
      ENDDO.

      DO.
        lv_tail2 = lv_tail2 - 1.
        IF lv_tail2 <= ls_candidate-buffer2index.
          EXIT.
        ENDIF.
        INSERT it_buffer2[ lv_tail2 + 1 ] INTO TABLE ls_different-buffer2.
      ENDDO.

      IF lines( ls_different-buffer1 ) > 0 OR lines( ls_different-buffer2 ) > 0.
        IF lines( lt_common ) > 0.
          CLEAR ls_res.
          ls_res-common = _reverse( lt_common ).
          INSERT ls_res INTO rt_result INDEX 1.
          CLEAR lt_common.
        ENDIF.

        CLEAR ls_res.
        ls_res-diff-buffer1 = _reverse( ls_different-buffer1 ).
        ls_res-diff-buffer2 = _reverse( ls_different-buffer2 ).
        INSERT ls_res INTO rt_result INDEX 1.
      ENDIF.

      IF lv_tail1 >= 0.
        INSERT it_buffer1[ lv_tail1 + 1 ] INTO TABLE lt_common.
      ENDIF.

      ls_candidate = lt_lcs[ key = ls_candidate-chain ].
    ENDDO.

    IF lines( lt_common ) > 0.
      CLEAR ls_res.
      ls_res-common = _reverse( lt_common ).
      INSERT ls_res INTO rt_result INDEX 1.
    ENDIF.

  ENDMETHOD.


  METHOD zif_differ_diff3~diff_indices.
    " We apply the LCS to give a simple representation of the
    " offsets and lengths of mismatched chunks in the input
    " buffers. This is used by diff3MergeRegions.

    DATA ls_result LIKE LINE OF rt_result.

    DATA(lt_lcs) = zif_differ_diff3~lcs( it_buffer1 = it_buffer1
                                         it_buffer2 = it_buffer2 ).

    DATA(lv_tail1) = lines( it_buffer1 ).
    DATA(lv_tail2) = lines( it_buffer2 ).

    DATA(ls_candidate) = lt_lcs[ key = lines( lt_lcs ) - 1 ].
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.

      DATA(lv_mismatchlength1) = lv_tail1 - ls_candidate-buffer1index - 1.
      DATA(lv_mismatchlength2) = lv_tail2 - ls_candidate-buffer2index - 1.
      lv_tail1 = ls_candidate-buffer1index.
      lv_tail2 = ls_candidate-buffer2index.

      IF lv_mismatchlength1 > 0 OR lv_mismatchlength2 > 0.
        CLEAR ls_result.
        ls_result-buffer1-key    = lv_tail1 + 1.
        ls_result-buffer1-len    = lv_mismatchlength1.
        ls_result-buffer1content = _slice( it_data  = it_buffer1
                                           iv_start = lv_tail1 + 1
                                           iv_end   = lv_tail1 + 1 + lv_mismatchlength1 ).
        ls_result-buffer2-key    = lv_tail2 + 1.
        ls_result-buffer2-len    = lv_mismatchlength2.
        ls_result-buffer2content = _slice( it_data  = it_buffer2
                                           iv_start = lv_tail2 + 1
                                           iv_end   = lv_tail2 + 1 + lv_mismatchlength2 ).
        INSERT ls_result INTO rt_result INDEX 1.
      ENDIF.

      ls_candidate = lt_lcs[ key = ls_candidate-chain ].
    ENDDO.

  ENDMETHOD.


  METHOD zif_differ_diff3~diff_patch.
    " We apply the LCS to build a JSON representation of a
    " diff(1)-style patch.

    DATA ls_result LIKE LINE OF rt_result.

    DATA(lt_lcs) = zif_differ_diff3~lcs( it_buffer1 = it_buffer1
                                         it_buffer2 = it_buffer2 ).

    DATA(lv_tail1) = lines( it_buffer1 ).
    DATA(lv_tail2) = lines( it_buffer2 ).

    DATA(ls_candidate) = lt_lcs[ key = lines( lt_lcs ) - 1 ].
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.

      DATA(lv_mismatchlength1) = lv_tail1 - ls_candidate-buffer1index - 1.
      DATA(lv_mismatchlength2) = lv_tail2 - ls_candidate-buffer2index - 1.
      lv_tail1 = ls_candidate-buffer1index.
      lv_tail2 = ls_candidate-buffer2index.

      IF lv_mismatchlength1 > 0 OR lv_mismatchlength2 > 0.
        CLEAR ls_result.
        ls_result-buffer1 = chunk_description( it_buffer = it_buffer1
                                               iv_offset = lv_tail1 + 1
                                               iv_length = lv_mismatchlength1 ).
        ls_result-buffer2 = chunk_description( it_buffer = it_buffer2
                                               iv_offset = lv_tail2 + 1
                                               iv_length = lv_mismatchlength2 ).
        INSERT ls_result INTO rt_result INDEX 1.
      ENDIF.

      ls_candidate = lt_lcs[ key = ls_candidate-chain ].
    ENDDO.

  ENDMETHOD.


  METHOD zif_differ_diff3~lcs.
    " Text diff algorithm following Hunt and McIlroy 1976.
    " J. W. Hunt and M. D. McIlroy, An algorithm for differential buffer
    " comparison, Bell Telephone Laboratories CSTR #41 (1976)
    " http:"www.cs.dartmouth.edu/~doug/
    " https:"en.wikipedia.org/wiki/Longest_common_subsequence_problem
    "
    " Expects two arrays, finds longest common sequence

    TYPES:
      BEGIN OF ty_equivalenceclass,
        key    TYPE string,
        values TYPE zif_differ_diff3=>ty_numbers,
      END OF ty_equivalenceclass.

    DATA:
      ls_equivalenceclass   TYPE ty_equivalenceclass,
      lt_equivalenceclasses TYPE HASHED TABLE OF ty_equivalenceclass WITH UNIQUE KEY key,
      ls_nullresult         TYPE zif_differ_diff3=>ty_lcsresult,
      lt_candidates         TYPE zif_differ_diff3=>ty_lcsresult_t,
      ls_newcandidate       TYPE zif_differ_diff3=>ty_lcsresult.

    DATA(lv_j) = 0.
    LOOP AT it_buffer2 ASSIGNING FIELD-SYMBOL(<lv_buffer2>).
      READ TABLE lt_equivalenceclasses ASSIGNING FIELD-SYMBOL(<ls_equivalentclass>)
        WITH TABLE KEY key = <lv_buffer2>.
      IF sy-subrc <> 0.
        CLEAR ls_equivalenceclass.
        ls_equivalenceclass-key = <lv_buffer2>.
        INSERT ls_equivalenceclass INTO TABLE lt_equivalenceclasses ASSIGNING <ls_equivalentclass>.
      ENDIF.
      INSERT lv_j INTO TABLE <ls_equivalentclass>-values.
      lv_j = lv_j + 1.
    ENDLOOP.

    ls_nullresult-key          = 0.
    ls_nullresult-buffer1index = -1.
    ls_nullresult-buffer2index = -1.
    ls_nullresult-chain        = -1.
    INSERT ls_nullresult INTO TABLE lt_candidates.

    DATA(lv_i) = 0.
    LOOP AT it_buffer1 ASSIGNING FIELD-SYMBOL(<lv_buffer1>).
      IF line_exists( lt_equivalenceclasses[ key = <lv_buffer1> ] ).
        DATA(lt_buffer2indices) = lt_equivalenceclasses[ key = <lv_buffer1> ]-values.
      ELSE.
        CLEAR lt_buffer2indices.
      ENDIF.

      DATA(lv_r) = 0.
      DATA(ls_c) = lt_candidates[ key = 0 ].
      LOOP AT lt_buffer2indices INTO lv_j.

        DATA(lv_s) = lv_r.
        DO.
          IF lv_s < lines( lt_candidates ).
            IF lt_candidates[ key = lv_s ]-buffer2index < lv_j AND
              ( lv_s = lines( lt_candidates ) - 1 OR lt_candidates[ key = lv_s + 1 ]-buffer2index > lv_j ).
              EXIT.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
          lv_s = lv_s + 1.
        ENDDO.

        IF lv_s < lines( lt_candidates ).
          ls_newcandidate-buffer1index = lv_i.
          ls_newcandidate-buffer2index = lv_j.
          ls_newcandidate-chain        = lv_s.

          IF lv_r = lines( lt_candidates ).
            ls_c-key = lines( lt_candidates ) + 1.
            INSERT ls_c INTO TABLE lt_candidates.
          ELSE.
            lt_candidates[ key = lv_r ]-buffer1index = ls_c-buffer1index.
            lt_candidates[ key = lv_r ]-buffer2index = ls_c-buffer2index.
            lt_candidates[ key = lv_r ]-chain        = ls_c-chain.
          ENDIF.

          lv_r = lv_s + 1.
          ls_c = ls_newcandidate.

          IF lv_r = lines( lt_candidates ).
            EXIT. " no point in examining further (j)s
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF line_exists( lt_candidates[ key = lv_r ] ).
        lt_candidates[ key = lv_r ]-buffer1index = ls_c-buffer1index.
        lt_candidates[ key = lv_r ]-buffer2index = ls_c-buffer2index.
        lt_candidates[ key = lv_r ]-chain        = ls_c-chain.
      ELSE.
        ls_c-key = lv_r.
        INSERT ls_c INTO TABLE lt_candidates.
      ENDIF.

      lv_i = lv_i + 1.
    ENDLOOP.

    " At this point, we know the LCS: it's in the reverse of the
    " linked-list through chain of candidates[ lines( candidates ) - 1 ].

    rt_result = lt_candidates.

  ENDMETHOD.


  METHOD zif_differ_diff3~patch.
    " Applies a patch to a buffer.
    " Given buffer1 and buffer2, `patch(buffer1, diffPatch(buffer1, buffer2))` should give buffer2.

    DATA(lv_curroffset) = 0.

    LOOP AT it_patchres ASSIGNING FIELD-SYMBOL(<ls_patch>).
      WHILE lv_curroffset < <ls_patch>-buffer1-offset.
        APPEND it_buffer[ lv_curroffset + 1 ] TO rt_result.
        lv_curroffset = lv_curroffset + 1.
      ENDWHILE.

      DO <ls_patch>-buffer2-length TIMES.
        APPEND <ls_patch>-buffer2-chunk[ sy-index ] TO rt_result.
      ENDDO.

      lv_curroffset = lv_curroffset + <ls_patch>-buffer1-length.
    ENDLOOP.

    WHILE lv_curroffset < lines( it_buffer ).
      APPEND it_buffer[ lv_curroffset + 1 ] TO rt_result.
      lv_curroffset = lv_curroffset + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD _reverse.

    DATA(lv_line) = lines( it_data ).

    DO lines( it_data ) TIMES.
      INSERT it_data[ lv_line ] INTO TABLE rt_result.
      lv_line = lv_line - 1.
    ENDDO.

  ENDMETHOD.


  METHOD _slice.

    " select from start to end (end not included!)
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>) FROM iv_start + 1 TO iv_end.
      APPEND <ls_data> TO rt_result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
