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
* https://github.com/bhousel/node-diff3/blob/main/index.mjs as of 2021-09-24
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_differ_diff3.

    CLASS-METHODS convert_to_abap_indices
      CHANGING
        !ct_diff_indices TYPE zif_differ_diff3=>ty_diff_indices_result_t OPTIONAL.

  PROTECTED SECTION.

    TYPES:
      ty_ab TYPE c LENGTH 1.

    TYPES:
      BEGIN OF ty_hunk,
        ab        TYPE ty_ab,
        o_start   TYPE zif_differ_diff3=>ty_number,
        o_length  TYPE zif_differ_diff3=>ty_number,
        ab_start  TYPE zif_differ_diff3=>ty_number,
        ab_length TYPE zif_differ_diff3=>ty_number,
      END OF ty_hunk.
    TYPES:
      ty_hunks TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY.

    METHODS chunk_description
      IMPORTING
        !it_buffer       TYPE string_table
        !iv_offset       TYPE zif_differ_diff3=>ty_number
        !iv_length       TYPE zif_differ_diff3=>ty_number
      RETURNING
        VALUE(rs_result) TYPE zif_differ_diff3=>ty_chunk.

    METHODS add_hunk
      IMPORTING
        !it_buffer TYPE zif_differ_diff3=>ty_diff_indices_result_t
        !iv_ab     TYPE ty_ab
      CHANGING
        ct_hunks   TYPE ty_hunks.

    METHODS advance_to
      IMPORTING
        !iv_end_offset TYPE zif_differ_diff3=>ty_number
        !it_o          TYPE string_table
      CHANGING
        cv_curr_offset TYPE zif_differ_diff3=>ty_number
        ct_results     TYPE zif_differ_diff3=>ty_region_t.

    METHODS flush_ok
      CHANGING
        !ct_buffer TYPE string_table
        !ct_result TYPE zif_differ_diff3=>ty_merge_region_t.

    METHODS get_labels
      IMPORTING
        !is_labels       TYPE zif_differ_diff3=>ty_labels
      RETURNING
        VALUE(rs_labels) TYPE zif_differ_diff3=>ty_labels.

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


  METHOD add_hunk.
    " hunks are array subsets where `a` or `b` are different from `o`
    " https://www.gnu.org/software/diffutils/manual/html_node/diff3-Hunks.html

    DATA ls_hunk LIKE LINE OF ct_hunks.

    LOOP AT it_buffer ASSIGNING FIELD-SYMBOL(<ls_item>).
      ls_hunk-ab        = iv_ab.
      ls_hunk-o_start   = <ls_item>-buffer1-key.
      ls_hunk-o_length  = <ls_item>-buffer1-len.
      ls_hunk-ab_start  = <ls_item>-buffer2-key.
      ls_hunk-ab_length = <ls_item>-buffer2-len.
      INSERT ls_hunk INTO TABLE ct_hunks.
    ENDLOOP.

  ENDMETHOD.


  METHOD advance_to.

    DATA ls_result LIKE LINE OF ct_results.

    IF iv_end_offset > cv_curr_offset.
      ls_result-stable = abap_true.
      ls_result-stable_region = VALUE #(
        buffer         = 'o'
        buffer_start   = cv_curr_offset
        buffer_length  = iv_end_offset - cv_curr_offset
        buffer_content = _slice(
          it_data  = it_o
          iv_start = cv_curr_offset
          iv_end   = iv_end_offset ) ).
      INSERT ls_result INTO TABLE ct_results.
      cv_curr_offset = iv_end_offset.
    ENDIF.

  ENDMETHOD.


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


  METHOD flush_ok.

    DATA ls_result LIKE LINE OF ct_result.

    IF ct_buffer IS NOT INITIAL.
      INSERT LINES OF ct_buffer INTO TABLE ls_result-ok.
      INSERT ls_result INTO TABLE ct_result.
      CLEAR ct_buffer.
    ENDIF.

  ENDMETHOD.


  METHOD get_labels.

    rs_labels = VALUE #(
      a = `<<<<<<<`
      o = `|||||||`
      x = `=======`
      b = `>>>>>>>` ).

    IF is_labels-a IS NOT INITIAL.
      rs_labels-a = rs_labels-a && | { is_labels-a }|.
    ENDIF.
    IF is_labels-o IS NOT INITIAL.
      rs_labels-o = rs_labels-o && | { is_labels-o }|.
    ENDIF.
    IF is_labels-b IS NOT INITIAL.
      rs_labels-b = rs_labels-b && | { is_labels-b }|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_differ_diff3~diff3_merge.
    " Applies the output of diff3MergeRegions to actually
    " construct the merged buffer; the returned result alternates
    " between 'ok' and 'conflict' blocks.
    " A "false conflict" is where `a` and `b` both change the same from `o`

    DATA ls_result LIKE LINE OF rt_result.
    DATA lt_ok_buffer TYPE string_table.

    DATA(lt_regions) = zif_differ_diff3~diff3_merge_regions(
      it_a = it_a
      it_o = it_o
      it_b = it_b ).

    LOOP AT lt_regions ASSIGNING FIELD-SYMBOL(<ls_region>).
      IF <ls_region>-stable = abap_true.
        INSERT LINES OF <ls_region>-stable_region-buffer_content INTO TABLE lt_ok_buffer.
      ELSE.
        IF iv_exclude_false_conflicts = abap_true AND
          <ls_region>-unstable_region-a_length  = <ls_region>-unstable_region-b_length AND
          <ls_region>-unstable_region-a_content = <ls_region>-unstable_region-b_content.
          INSERT LINES OF <ls_region>-unstable_region-a_content INTO TABLE lt_ok_buffer.
        ELSE.
          flush_ok(
            CHANGING
              ct_buffer = lt_ok_buffer
              ct_result = rt_result ).

          CLEAR ls_result.
          ls_result-conflict = VALUE #(
            a       = <ls_region>-unstable_region-a_content
            a_index = <ls_region>-unstable_region-a_start
            o       = <ls_region>-unstable_region-o_content
            o_index = <ls_region>-unstable_region-o_start
            b       = <ls_region>-unstable_region-b_content
            b_index = <ls_region>-unstable_region-b_start ).
          INSERT ls_result INTO TABLE rt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    flush_ok(
      CHANGING
        ct_buffer = lt_ok_buffer
        ct_result = rt_result ).

  ENDMETHOD.


  METHOD zif_differ_diff3~diff3_merge_regions.
    " Given three buffers, A, O, and B, where both A and B are
    " independently derived from O, returns a fairly complicated
    " internal representation of merge decisions it's taken. The
    " interested reader may wish to consult
    "
    " Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce.
    " 'A Formal Investigation of ' In Arvind and Prasad,
    " editors, Foundations of Software Technology and Theoretical
    " Computer Science (FSTTCS), December 2007.
    "
    " (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)

    TYPES:
      BEGIN OF ty_bound,
        n0 TYPE zif_differ_diff3=>ty_number,
        n1 TYPE zif_differ_diff3=>ty_number,
        n2 TYPE zif_differ_diff3=>ty_number,
        n3 TYPE zif_differ_diff3=>ty_number,
      END OF ty_bound.

    DATA:
      ls_result       LIKE LINE OF rt_result,
      lt_hunks        TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY,
      lt_region_hunks TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY.

    DATA:
      BEGIN OF ls_bounds,
        a TYPE ty_bound,
        b TYPE ty_bound,
      END OF ls_bounds.

    add_hunk(
      EXPORTING
        it_buffer = zif_differ_diff3~diff_indices(
                      it_buffer1 = it_o
                      it_buffer2 = it_a )
        iv_ab     = 'a'
      CHANGING
        ct_hunks  = lt_hunks ).

    add_hunk(
      EXPORTING
        it_buffer = zif_differ_diff3~diff_indices(
                      it_buffer1 = it_o
                      it_buffer2 = it_b )
        iv_ab     = 'b'
      CHANGING
        ct_hunks  = lt_hunks ).

    SORT lt_hunks BY o_start ab.

    DATA(lv_curr_offset) = 0.

    DATA(lv_hunk) = 0.
    WHILE lv_hunk < lines( lt_hunks ).
      DATA(ls_hunk) = lt_hunks[ lv_hunk + 1 ].
      lv_hunk = lv_hunk + 1.

      DATA(lv_region_start) = ls_hunk-o_start.
      DATA(lv_region_end)   = ls_hunk-o_start + ls_hunk-o_length.

      CLEAR lt_region_hunks.
      INSERT ls_hunk INTO TABLE lt_region_hunks.

      advance_to(
        EXPORTING
          iv_end_offset  = lv_region_start
          it_o           = it_o
        CHANGING
          cv_curr_offset = lv_curr_offset
          ct_results     = rt_result ).

      " Try to pull next overlapping hunk into this region
      WHILE lv_hunk < lines( lt_hunks ).
        DATA(ls_next_hunk) = lt_hunks[ lv_hunk + 1 ].
        DATA(lv_next_hunk_start) = ls_next_hunk-o_start.

        IF lv_next_hunk_start > lv_region_end.
          EXIT.          " no overlap
        ENDIF.

        lv_region_end = nmax(
          val1 = lv_region_end
          val2 = lv_next_hunk_start + ls_next_hunk-o_length ).

        INSERT ls_next_hunk INTO TABLE lt_region_hunks.
        lv_hunk = lv_hunk + 1.
      ENDWHILE.

      IF lines( lt_region_hunks ) = 1.
        " Only one hunk touches this region, meaning that there is no conflict here.
        " either `a` or `b` is inserting into a region of `o` unchanged by the other.
        IF ls_hunk-ab_length > 0.
          IF ls_hunk-ab = 'a'.
            ASSIGN it_a TO FIELD-SYMBOL(<lt_buffer>).
            ASSERT sy-subrc = 0.
          ELSE.
            ASSIGN it_b TO <lt_buffer>.
            ASSERT sy-subrc = 0.
          ENDIF.
          CLEAR ls_result.
          ls_result-stable = abap_true.
          ls_result-stable_region = VALUE #(
            buffer        = ls_hunk-ab
            buffer_start  = ls_hunk-ab_start
            buffer_length = ls_hunk-ab_length
            buffer_content = _slice(
              it_data  = <lt_buffer>
              iv_start = ls_hunk-ab_start
              iv_end   = ls_hunk-ab_start + ls_hunk-ab_length ) ).
          INSERT ls_result INTO TABLE rt_result.
        ENDIF.
      ELSE.
        " a true a/b conflict. determine the bounds involved from `a`, `o`, and `b`.
        " effectively merge all the `a` hunks into one giant hunk, then do the
        " same for the `b` hunks. then, correct for skew in the regions of `o`
        " that each side changed, and report appropriate spans for the three sides.
        ls_bounds = VALUE #(
          a = VALUE #(
            n0 = lines( it_a )
            n1 = -1
            n2 = lines( it_o )
            n3 = -1 )
          b = VALUE #(
            n0 = lines( it_b )
            n1 = -1
            n2 = lines( it_o )
            n3 = -1 ) ).

        DATA(lv_region_hunk) = 0.
        WHILE lv_region_hunk < lines( lt_region_hunks ).
          ls_hunk = lt_region_hunks[ lv_region_hunk + 1 ].
          lv_region_hunk = lv_region_hunk + 1.

          DATA(lv_o_start)  = ls_hunk-o_start.
          DATA(lv_o_end)    = lv_o_start + ls_hunk-o_length.
          DATA(lv_ab_start) = ls_hunk-ab_start.
          DATA(lv_ab_end)   = lv_ab_start + ls_hunk-ab_length.

          IF ls_hunk-ab = 'a'.
            ASSIGN ls_bounds-a TO FIELD-SYMBOL(<ls_b>).
            ASSERT sy-subrc = 0.
          ELSE.
            ASSIGN ls_bounds-b TO <ls_b>.
            ASSERT sy-subrc = 0.
          ENDIF.

          DATA(ls_b) = VALUE ty_bound(
            n0 = nmin(
              val1 = lv_ab_start
              val2 = <ls_b>-n0 )
            n1 = nmax(
              val1 = lv_ab_end
              val2 = <ls_b>-n1 )
            n2 = nmin(
              val1 = lv_o_start
              val2 = <ls_b>-n2 )
            n3 = nmax(
              val1 = lv_o_end
              val2 = <ls_b>-n3 ) ).
          <ls_b> = ls_b.
        ENDWHILE.

        DATA(lv_a_start) = ls_bounds-a-n0 + lv_region_start - ls_bounds-a-n2.
        DATA(lv_a_end)   = ls_bounds-a-n1 + lv_region_end - ls_bounds-a-n3.
        DATA(lv_b_start) = ls_bounds-b-n0 + lv_region_start - ls_bounds-b-n2.
        DATA(lv_b_end)   = ls_bounds-b-n1 + lv_region_end - ls_bounds-b-n3.

        CLEAR ls_result.
        ls_result-stable = abap_false.
        ls_result-unstable_region = VALUE #(
          a_start   = lv_a_start
          a_length  = lv_a_end - lv_a_start
          a_content = _slice(
            it_data  = it_a
            iv_start = lv_a_start
            iv_end   = lv_a_end )
          o_start   = lv_region_start
          o_length  = lv_region_end - lv_region_start
          o_content = _slice(
            it_data  = it_o
            iv_start = lv_region_start
            iv_end   = lv_region_end )
          b_start   = lv_b_start
          b_length  = lv_b_end - lv_b_start
          b_content = _slice(
            it_data  = it_b
            iv_start = lv_b_start
            iv_end   = lv_b_end ) ).
        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

      lv_curr_offset = lv_region_end.
    ENDWHILE.

    advance_to(
      EXPORTING
        iv_end_offset  = lines( it_o )
        it_o           = it_o
      CHANGING
        cv_curr_offset = lv_curr_offset
        ct_results     = rt_result ).

  ENDMETHOD.


  METHOD zif_differ_diff3~diff_comm.
    " We apply the LCS to build a 'comm'-style picture of the
    " differences between buffer1 and buffer2.

    DATA:
      ls_res       LIKE LINE OF rt_result,
      ls_different TYPE zif_differ_diff3=>ty_comm_result-diff,
      lt_common    TYPE zif_differ_diff3=>ty_comm_result-common.

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

    DATA(lt_lcs) = zif_differ_diff3~lcs(
      it_buffer1 = it_buffer1
      it_buffer2 = it_buffer2 ).

    DATA(lv_tail1) = lines( it_buffer1 ).
    DATA(lv_tail2) = lines( it_buffer2 ).

    DATA(ls_candidate) = lt_lcs[ key = lines( lt_lcs ) - 1 ].
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.

      DATA(lv_mismatch_length1) = lv_tail1 - ls_candidate-buffer1index - 1.
      DATA(lv_mismatch_length2) = lv_tail2 - ls_candidate-buffer2index - 1.
      lv_tail1 = ls_candidate-buffer1index.
      lv_tail2 = ls_candidate-buffer2index.

      IF lv_mismatch_length1 > 0 OR lv_mismatch_length2 > 0.
        DATA(ls_result) = VALUE zif_differ_diff3=>ty_diff_indices_result(
          buffer1-key    = lv_tail1 + 1
          buffer1-len    = lv_mismatch_length1
          buffer1content = _slice(
            it_data  = it_buffer1
            iv_start = lv_tail1 + 1
            iv_end   = lv_tail1 + 1 + lv_mismatch_length1 )
          buffer2-key    = lv_tail2 + 1
          buffer2-len    = lv_mismatch_length2
          buffer2content = _slice(
            it_data  = it_buffer2
            iv_start = lv_tail2 + 1
            iv_end   = lv_tail2 + 1 + lv_mismatch_length2 ) ).
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


  METHOD zif_differ_diff3~invert_patch.
    " Takes the output of diffPatch(), and inverts the sense of it, so that it
    " can be applied to buffer2 to give buffer1 rather than the other way around.

    DATA ls_result LIKE LINE OF rt_result.

    LOOP AT it_patchres ASSIGNING FIELD-SYMBOL(<ls_patch>).
      CLEAR ls_result.
      ls_result-buffer1 = <ls_patch>-buffer2.
      ls_result-buffer2 = <ls_patch>-buffer1.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

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
      lt_equivalenceclasses TYPE HASHED TABLE OF ty_equivalenceclass WITH UNIQUE KEY key,
      lt_candidates         TYPE zif_differ_diff3=>ty_lcs_result_t,
      ls_newcandidate       TYPE zif_differ_diff3=>ty_lcs_result.

    DATA(lv_j) = 0.
    LOOP AT it_buffer2 ASSIGNING FIELD-SYMBOL(<lv_buffer2>).
      READ TABLE lt_equivalenceclasses ASSIGNING FIELD-SYMBOL(<ls_equivalentclass>)
        WITH TABLE KEY key = <lv_buffer2>.
      IF sy-subrc <> 0.
        DATA(ls_equivalenceclass) = VALUE ty_equivalenceclass( key = <lv_buffer2> ).
        INSERT ls_equivalenceclass INTO TABLE lt_equivalenceclasses ASSIGNING <ls_equivalentclass>.
      ENDIF.
      INSERT lv_j INTO TABLE <ls_equivalentclass>-values.
      lv_j = lv_j + 1.
    ENDLOOP.

    DATA(ls_nullresult) = VALUE zif_differ_diff3=>ty_lcs_result(
      key          = 0
      buffer1index = -1
      buffer2index = -1
      chain        = -1 ).
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


  METHOD zif_differ_diff3~merge.

    DATA(ls_labels) = get_labels( is_labels ).

    DATA(lt_regions) = zif_differ_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).

    LOOP AT lt_regions ASSIGNING FIELD-SYMBOL(<ls_region>).
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSEIF <ls_region>-conflict IS NOT INITIAL.
        rs_result-conflict = abap_true.
        INSERT ls_labels-a INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-a INTO TABLE rs_result-result.
        INSERT ls_labels-x INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-b INTO TABLE rs_result-result.
        INSERT ls_labels-b INTO TABLE rs_result-result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_differ_diff3~merge_diff3.

    DATA(ls_labels) = get_labels( is_labels ).

    DATA(lt_regions) = zif_differ_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).

    LOOP AT lt_regions ASSIGNING FIELD-SYMBOL(<ls_region>).
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSEIF <ls_region>-conflict IS NOT INITIAL.
        rs_result-conflict = abap_true.
        INSERT ls_labels-a INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-a INTO TABLE rs_result-result.
        INSERT ls_labels-o INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-o INTO TABLE rs_result-result.
        INSERT ls_labels-x INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-b INTO TABLE rs_result-result.
        INSERT ls_labels-b INTO TABLE rs_result-result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_differ_diff3~merge_dig_in.

    DATA(ls_labels) = get_labels( is_labels ).

    DATA(lt_regions) = zif_differ_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).

    LOOP AT lt_regions ASSIGNING FIELD-SYMBOL(<ls_region>).
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSE.
        DATA(lt_c) = zif_differ_diff3~diff_comm(
          it_buffer1 = <ls_region>-conflict-a
          it_buffer2 = <ls_region>-conflict-b ).

        LOOP AT lt_c ASSIGNING FIELD-SYMBOL(<ls_c>).
          IF <ls_c>-common IS NOT INITIAL.
            INSERT LINES OF <ls_c>-common INTO TABLE rs_result-result.
          ELSE.
            rs_result-conflict = abap_true.
            INSERT ls_labels-a INTO TABLE rs_result-result.
            INSERT LINES OF <ls_c>-diff-buffer1 INTO TABLE rs_result-result.
            INSERT ls_labels-x INTO TABLE rs_result-result.
            INSERT LINES OF <ls_c>-diff-buffer2 INTO TABLE rs_result-result.
            INSERT ls_labels-b INTO TABLE rs_result-result.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

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

      DO lines( <ls_patch>-buffer2-chunk ) TIMES.
        APPEND <ls_patch>-buffer2-chunk[ sy-index ] TO rt_result.
      ENDDO.

      lv_curroffset = lv_curroffset + <ls_patch>-buffer1-length.
    ENDLOOP.

    WHILE lv_curroffset < lines( it_buffer ).
      APPEND it_buffer[ lv_curroffset + 1 ] TO rt_result.
      lv_curroffset = lv_curroffset + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_differ_diff3~strip_patch.
    " Takes the output of diffPatch(), and removes extra information from it.
    " It can still be used by patch(), below, but can no longer be inverted.

    DATA ls_result LIKE LINE OF rt_result.

    LOOP AT it_patchres ASSIGNING FIELD-SYMBOL(<ls_patch>).
      CLEAR ls_result.
      ls_result-buffer1-offset = <ls_patch>-buffer1-offset.
      ls_result-buffer1-length = <ls_patch>-buffer1-length.
      ls_result-buffer2-chunk  = <ls_patch>-buffer2-chunk.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

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
