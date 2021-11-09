CLASS zcl_differ_htmldiff DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* ABAP Differ - HTML Diff
*
* https://github.com/Marc-Bernard-Tools/ABAP-Differ
*
* This  is a port of JavaScript        (https://github.com/alaorneto/htmldiffer, no license)
* which is a port of CoffeeScript      (https://github.com/tnwinc/htmldiff.js, MIT license)
* which is a port of the original Ruby (https://github.com/myobie/htmldiff, MIT license)
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_differ_htmldiff.

    METHODS constructor
      IMPORTING
        !iv_inserts         TYPE abap_bool DEFAULT abap_true
        !iv_deletes         TYPE abap_bool DEFAULT abap_true
        !iv_css_classes     TYPE abap_bool DEFAULT abap_false
        !iv_support_chinese TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.

    TYPES:
      ty_token  TYPE string,
      ty_tokens TYPE STANDARD TABLE OF ty_token WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_match,
        start_in_before TYPE i,
        start_in_after  TYPE i,
        length          TYPE i,
        end_in_before   TYPE i,
        end_in_after    TYPE i,
      END OF ty_match,
      ty_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.

    TYPES:
      ty_location  TYPE i,
      ty_locations TYPE SORTED TABLE OF ty_location WITH UNIQUE DEFAULT KEY.

    TYPES:
      BEGIN OF ty_index_row,
        token     TYPE ty_token,
        locations TYPE ty_locations,
      END OF ty_index_row,
      ty_index_tab TYPE HASHED TABLE OF ty_index_row WITH UNIQUE KEY token.

    TYPES:
      ty_action TYPE string.

    TYPES:
      BEGIN OF ty_operation,
        action          TYPE ty_action,
        start_in_before TYPE i,
        end_in_before   TYPE i,
        start_in_after  TYPE i,
        end_in_after    TYPE i,
      END OF ty_operation.
    TYPES:
      ty_operations TYPE STANDARD TABLE OF ty_operation WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_action,
        none    TYPE ty_action VALUE 'none',
        equal   TYPE ty_action VALUE 'equal',
        insert  TYPE ty_action VALUE 'insert',
        delete  TYPE ty_action VALUE 'delete',
        insmod  TYPE ty_action VALUE 'insmod',
        delmod  TYPE ty_action VALUE 'delmod',
        replace TYPE ty_action VALUE 'replace',
      END OF c_action.

    METHODS diff
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_operations.

    METHODS is_character
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_chinese
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_quote
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_whitespace
      IMPORTING
        !iv_input        TYPE csequence
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_tag
      IMPORTING
        !iv_token        TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS isnt_tag
      IMPORTING
        !iv_token        TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_tag_begin
      IMPORTING
        !iv_token        TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_tag_end
      IMPORTING
        !iv_token        TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS html_to_tokens
      IMPORTING
        !iv_html         TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS find_match
      IMPORTING
        !it_before_tokens         TYPE ty_tokens
        !it_after_tokens          TYPE ty_tokens ##NEEDED
        !it_index_before_in_after TYPE ty_index_tab
        !iv_start_in_before       TYPE i
        !iv_end_in_before         TYPE i
        !iv_start_in_after        TYPE i
        !iv_end_in_after          TYPE i
      RETURNING
        VALUE(rs_result)          TYPE ty_match.

    METHODS find_matching_blocks
      IMPORTING
        !it_before_tokens TYPE ty_tokens
        !it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rt_result)  TYPE ty_matches.

    METHODS recurs_find_matching_blocks
      IMPORTING
        !it_before_tokens         TYPE ty_tokens
        !it_after_tokens          TYPE ty_tokens
        !it_index_before_in_after TYPE ty_index_tab
        !iv_start_in_before       TYPE i
        !iv_end_in_before         TYPE i
        !iv_start_in_after        TYPE i
        !iv_end_in_after          TYPE i
      CHANGING
        !ct_matching_blocks       TYPE ty_matches
      RETURNING
        VALUE(rt_result)          TYPE ty_matches.

    METHODS create_index
      IMPORTING
        !it_find_these   TYPE ty_tokens
        !it_in_these     TYPE ty_tokens
      RETURNING
        VALUE(rt_result) TYPE ty_index_tab.

    METHODS calculate_operations
      IMPORTING
        !it_before_tokens TYPE ty_tokens
        !it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rt_result)  TYPE ty_operations.

    METHODS calculate_simple
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_operations.

    METHODS is_single_whitespace
      IMPORTING
        !is_op           TYPE ty_operation
        !it_tokens       TYPE ty_tokens
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS consecutive_where
      IMPORTING
        !iv_start        TYPE i
        !it_content      TYPE ty_tokens
        !is_tag          TYPE abap_bool
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS wrap
      IMPORTING
        !iv_tag          TYPE ty_token
        !it_content      TYPE ty_tokens
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS operation
      IMPORTING
        !is_op            TYPE ty_operation
        !it_before_tokens TYPE ty_tokens
        !it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rv_result)  TYPE string.

    METHODS render_operations
      IMPORTING
        !it_before_tokens TYPE ty_tokens
        !it_after_tokens  TYPE ty_tokens
        !it_operations    TYPE ty_operations
      RETURNING
        VALUE(rv_result)  TYPE string.

    METHODS render_simple
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_mode,
        char       TYPE string VALUE 'char',
        whitespace TYPE string VALUE 'whitespace',
        tag        TYPE string VALUE 'tag',
      END OF c_mode.

    CONSTANTS:
      BEGIN OF c_tag,
        begin  TYPE c LENGTH 1 VALUE '<',
        end    TYPE c LENGTH 1 VALUE '>',
        del    TYPE string VALUE 'del',
        ins    TYPE string VALUE 'ins',
        delmod TYPE string VALUE 'delmod',
        insmod TYPE string VALUE 'insmod',
      END OF c_tag.

    CONSTANTS:
      BEGIN OF c_tag_class,
        insert  TYPE string VALUE 'diffins',
        delete  TYPE string VALUE 'diffdel',
        replace TYPE string VALUE 'diffmod',
      END OF c_tag_class.

    DATA:
      mv_css_classes     TYPE abap_bool,
      mv_inserts         TYPE abap_bool,
      mv_deletes         TYPE abap_bool,
      mv_with_img        TYPE abap_bool,
      mv_with_tags       TYPE abap_bool,
      mv_support_chinese TYPE abap_bool.

    METHODS _inject
      IMPORTING
        !iv_with_tags TYPE abap_bool.

    METHODS _slice
      IMPORTING
        !it_tokens       TYPE ty_tokens
        !iv_start        TYPE i
        !iv_end          TYPE i
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS _join
      IMPORTING
        !it_tokens       TYPE ty_tokens
        !iv_separator    TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _new_match
      IMPORTING
        !iv_start_in_before TYPE i
        !iv_start_in_after  TYPE i
        !iv_length          TYPE i
      RETURNING
        VALUE(rs_result)    TYPE ty_match.

    METHODS _new_operation
      IMPORTING
        !iv_action          TYPE string
        !iv_start_in_before TYPE i OPTIONAL
        !iv_start_in_after  TYPE i OPTIONAL
        !iv_end_in_before   TYPE i OPTIONAL
        !iv_end_in_after    TYPE i OPTIONAL
      RETURNING
        VALUE(rs_result)    TYPE ty_operation.

    METHODS _get_class
      IMPORTING
        !iv_tag          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _get_end
      IMPORTING
        !iv_end          TYPE i
      RETURNING
        VALUE(rv_result) TYPE i.

ENDCLASS.



CLASS zcl_differ_htmldiff IMPLEMENTATION.


  METHOD calculate_operations.

    DATA:
      lt_operations     TYPE ty_operations,
      lt_post_processed TYPE ty_operations.

    FIELD-SYMBOLS:
      <ls_last_op> TYPE ty_operation.

    " any before_tokens?
    ASSERT it_before_tokens IS NOT INITIAL.

    " any after_tokens?
    ASSERT it_after_tokens IS NOT INITIAL.

    DATA(lv_position_in_before) = 0.
    DATA(lv_position_in_after)  = 0.

    DATA(lt_matches) = find_matching_blocks( it_before_tokens = it_before_tokens
                                             it_after_tokens  = it_after_tokens ).

    APPEND _new_match(
      iv_start_in_before = lines( it_before_tokens )
      iv_start_in_after  = lines( it_after_tokens )
      iv_length          = 0 )
      TO lt_matches.

    LOOP AT lt_matches INTO DATA(ls_match).

      IF lv_position_in_before = ls_match-start_in_before.
        IF lv_position_in_after = ls_match-start_in_after.
          DATA(lv_action) = c_action-none.
        ELSE.
          lv_action = c_action-insert.
        ENDIF.
      ELSE.
        IF lv_position_in_after = ls_match-start_in_after.
          lv_action = c_action-delete.
        ELSE.
          lv_action = c_action-replace.
        ENDIF.
      ENDIF.

      IF lv_action <> c_action-none.
        IF lv_action <> c_action-insert.
          DATA(lv_end_in_before) = ls_match-start_in_before - 1.
        ELSE.
          lv_end_in_before = -1.
        ENDIF.
        IF lv_action <> c_action-delete.
          DATA(lv_end_in_after) = ls_match-start_in_after - 1.
        ELSE.
          lv_end_in_after = -1.
        ENDIF.

        APPEND _new_operation(
          iv_action          = lv_action
          iv_start_in_before = lv_position_in_before
          iv_end_in_before   = lv_end_in_before
          iv_start_in_after  = lv_position_in_after
          iv_end_in_after    = lv_end_in_after
          ) TO lt_operations.
      ENDIF.

      IF ls_match-length <> 0.
        APPEND _new_operation(
          iv_action          = c_action-equal
          iv_start_in_before = ls_match-start_in_before
          iv_end_in_before   = ls_match-end_in_before
          iv_start_in_after  = ls_match-start_in_after
          iv_end_in_after    = ls_match-end_in_after
          ) TO lt_operations.
      ENDIF.

      lv_position_in_before = ls_match-end_in_before + 1.
      lv_position_in_after  = ls_match-end_in_after + 1.

    ENDLOOP.

    LOOP AT lt_operations INTO DATA(ls_op).
      DATA(lv_whitespace) = is_single_whitespace( is_op     = ls_op
                                                  it_tokens = it_before_tokens ).

      IF ( lv_whitespace = abap_true OR ls_op-action = c_action-replace ) AND
         ( <ls_last_op> IS ASSIGNED AND <ls_last_op>-action = c_action-replace ).
        <ls_last_op>-end_in_before = ls_op-end_in_before.
        <ls_last_op>-end_in_after  = ls_op-end_in_after.
      ELSE.
        APPEND ls_op TO lt_post_processed ASSIGNING <ls_last_op>.
      ENDIF.

    ENDLOOP.

    rt_result = lt_post_processed.

  ENDMETHOD.


  METHOD calculate_simple.

    IF iv_before = iv_after.
      DATA(lv_action) = c_action-none.
    ELSEIF iv_before IS INITIAL AND iv_after IS INITIAL.
      lv_action = ''.
    ELSEIF iv_before IS INITIAL.
      IF mv_inserts = abap_true.
        lv_action = c_action-insert.
      ENDIF.
    ELSEIF iv_after IS INITIAL.
      IF mv_deletes = abap_true.
        lv_action = c_action-delete.
      ENDIF.
    ENDIF.

    IF lv_action IS NOT INITIAL.
      APPEND _new_operation(
        iv_action          = lv_action
        iv_start_in_before = 0
        iv_end_in_before   = -1
        iv_start_in_after  = 0
        iv_end_in_after    = -1
        ) TO rt_result.
    ENDIF.

  ENDMETHOD.


  METHOD consecutive_where.

    LOOP AT it_content INTO DATA(lv_token) FROM iv_start + 1.

      IF is_tag = abap_true.
        DATA(lv_answer) = is_tag( lv_token ).
      ELSE.
        lv_answer = isnt_tag( lv_token ).
      ENDIF.
      IF lv_answer = abap_true.
        APPEND lv_token TO rt_result.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mv_inserts         = iv_inserts.
    mv_deletes         = iv_deletes.
    mv_css_classes     = iv_css_classes.
    mv_support_chinese = iv_support_chinese.

  ENDMETHOD.


  METHOD create_index.

    DATA ls_index TYPE ty_index_row.

    LOOP AT it_find_these ASSIGNING FIELD-SYMBOL(<lv_token>).

      READ TABLE rt_result ASSIGNING FIELD-SYMBOL(<ls_index>) WITH TABLE KEY token = <lv_token>.
      IF sy-subrc <> 0.
        CLEAR ls_index.
        ls_index-token = <lv_token>.
        INSERT ls_index INTO TABLE rt_result ASSIGNING <ls_index>.
      ENDIF.

      LOOP AT it_in_these TRANSPORTING NO FIELDS WHERE table_line = <lv_token>.
        DATA(lv_idx) = sy-tabix - 1.
        COLLECT lv_idx INTO <ls_index>-locations.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD diff.

    mv_with_img  = abap_false.
    mv_with_tags = abap_false.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rt_result = calculate_simple( iv_before = iv_before
                                    iv_after  = iv_after ).
    ELSE.
      DATA(lt_before_tokens) = html_to_tokens( iv_before ).
      DATA(lt_after_tokens)  = html_to_tokens( iv_after ).

      rt_result = calculate_operations( it_before_tokens = lt_before_tokens
                                        it_after_tokens  = lt_after_tokens ).
    ENDIF.

  ENDMETHOD.


  METHOD find_match.

    TYPES:
      BEGIN OF ty_keyval,
        key TYPE i,
        val TYPE i,
      END OF ty_keyval,
      ty_keyvals TYPE HASHED TABLE OF ty_keyval WITH UNIQUE KEY key.

    DATA:
      lt_match_length_at     TYPE ty_keyvals,
      ls_new_match_length_at TYPE ty_keyval,
      lt_new_match_length_at TYPE ty_keyvals.

    DATA(lv_best_match_in_before) = iv_start_in_before.
    DATA(lv_best_match_in_after)  = iv_start_in_after.
    DATA(lv_best_match_length)    = 0.

    CLEAR lt_match_length_at.

    DATA(lv_index_in_before) = iv_start_in_before.
    DO.
      IF iv_start_in_before <= iv_end_in_before.
        IF lv_index_in_before >= iv_end_in_before.
          EXIT.
        ENDIF.
      ELSE.
        IF lv_index_in_before <= iv_end_in_before.
          EXIT.
        ENDIF.
      ENDIF.

      READ TABLE it_before_tokens INTO DATA(lv_looking_for) INDEX lv_index_in_before + 1.
      ASSERT sy-subrc = 0. " something wrong with do-loop.

      CLEAR lt_new_match_length_at.

      READ TABLE it_index_before_in_after INTO DATA(ls_index_row) WITH TABLE KEY token = lv_looking_for.
      ASSERT sy-subrc = 0. " all tokens must be in index
      DATA(lt_locations_in_after) = ls_index_row-locations.

      LOOP AT lt_locations_in_after INTO DATA(lv_index_in_after).

        IF lv_index_in_after < iv_start_in_after.
          CONTINUE.
        ENDIF.
        IF lv_index_in_after >= iv_end_in_after.
          EXIT.
        ENDIF.

        READ TABLE lt_match_length_at INTO DATA(ls_match_length_at) WITH TABLE KEY key = lv_index_in_after - 1.
        IF sy-subrc <> 0.
          ls_match_length_at-key = lv_index_in_after - 1.
          ls_match_length_at-val = 0.
          INSERT ls_match_length_at INTO TABLE lt_match_length_at.
        ENDIF.

        DATA(lv_new_match_length) = ls_match_length_at-val + 1.

        ls_new_match_length_at-key = lv_index_in_after.
        ls_new_match_length_at-val = lv_new_match_length.
        INSERT ls_new_match_length_at INTO TABLE lt_new_match_length_at.

        IF lv_new_match_length > lv_best_match_length.
          lv_best_match_in_before = lv_index_in_before - lv_new_match_length + 1.
          lv_best_match_in_after  = lv_index_in_after  - lv_new_match_length + 1.
          lv_best_match_length    = lv_new_match_length.
        ENDIF.

      ENDLOOP.

      lt_match_length_at = lt_new_match_length_at.

      IF iv_start_in_before <= iv_end_in_before.
        lv_index_in_before = lv_index_in_before + 1.
      ELSE.
        lv_index_in_before = lv_index_in_before - 1.
      ENDIF.

    ENDDO.

    IF lv_best_match_length <> 0.
      DATA(ls_match) = _new_match( iv_start_in_before = lv_best_match_in_before
                                   iv_start_in_after  = lv_best_match_in_after
                                   iv_length          = lv_best_match_length ).
    ENDIF.

    rs_result = ls_match.

  ENDMETHOD.


  METHOD find_matching_blocks.

    DATA(lt_matching_blocks) = VALUE ty_matches( ).

    DATA(lt_index_of_before_in_after) = create_index( it_find_these = it_before_tokens
                                                      it_in_these   = it_after_tokens ).

    rt_result = recurs_find_matching_blocks(
      EXPORTING
        it_before_tokens         = it_before_tokens
        it_after_tokens          = it_after_tokens
        it_index_before_in_after = lt_index_of_before_in_after
        iv_start_in_before       = 0
        iv_end_in_before         = lines( it_before_tokens )
        iv_start_in_after        = 0
        iv_end_in_after          = lines( it_after_tokens )
      CHANGING
        ct_matching_blocks       = lt_matching_blocks ).

  ENDMETHOD.


  METHOD html_to_tokens.

    DATA:
      lv_current_word TYPE string,
      lt_words        TYPE ty_tokens.

    DATA(lv_mode) = c_mode-char.

    DATA(lv_idx) = 0.
    DO strlen( iv_html ) TIMES.
      DATA(lv_char) = iv_html+lv_idx(1).

      CASE lv_mode.
        WHEN c_mode-tag.
          IF is_tag_end( lv_char ) = abap_true.
            lv_current_word = lv_current_word && c_tag-end.
            APPEND lv_current_word TO lt_words.
            lv_current_word = ''.
            lv_mode = c_mode-char.
          ELSE.
            lv_current_word = lv_current_word && lv_char.
          ENDIF.

        WHEN c_mode-char.
          IF is_tag_begin( lv_char ) = abap_true.
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = c_tag-begin.
            lv_mode = c_mode-tag.
          ELSEIF is_whitespace( lv_char ) = abap_true.
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = lv_char.
            lv_mode = c_mode-whitespace.
          ELSEIF is_chinese( lv_char ) = abap_true.
            " Treat Chinese characters as individual words
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            APPEND lv_char TO lt_words.
            lv_current_word = ''.
          ELSEIF is_character( lv_char ) = abap_true.
            lv_current_word = lv_current_word && lv_char.
            IF is_quote( lv_current_word ) = abap_true.
              APPEND lv_current_word TO lt_words.
              lv_current_word = ''.
            ENDIF.
          ELSE.
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            APPEND lv_char TO lt_words.
            lv_current_word = ''.
          ENDIF.

        WHEN c_mode-whitespace.
          IF is_tag_begin( lv_char ) = abap_true.
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = c_tag-begin.
            lv_mode = c_mode-tag.
          ELSEIF is_whitespace( lv_char ) = abap_true.
            lv_current_word = lv_current_word && lv_char.
          ELSE.
            IF lv_current_word IS NOT INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = lv_char.
            lv_mode = c_mode-char.
          ENDIF.

        WHEN OTHERS.
          ASSERT 0 = 1. " Unknown mode
      ENDCASE.

      lv_idx = lv_idx + 1.
    ENDDO.

    IF lv_current_word IS NOT INITIAL.
      APPEND lv_current_word TO lt_words.
    ENDIF.

    rt_result = lt_words.

  ENDMETHOD.


  METHOD isnt_tag.

    rv_result = xsdbool( is_tag( iv_token ) = abap_false ).

  ENDMETHOD.


  METHOD is_character.

    FIND REGEX '[\w\#@;]' IN iv_input.

    rv_result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_chinese.

    " This is the common range for CJK Unified Ideographs
    " There can be more, see http://www.unicode.org/faq/han_cjk.html
    CONSTANTS:
      lc_from TYPE x LENGTH 2 VALUE '4E00',
      lc_to   TYPE x LENGTH 2 VALUE '9FFF'.

    IF mv_support_chinese = abap_true.
      DATA(lv_x) = cl_abap_conv_out_ce=>uccp( iv_input ).
      IF lv_x BETWEEN lc_from AND lc_to.
        rv_result = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_quote.

    " https://www.w3schools.com/charsets/ref_utf_basic_latin.asp
    rv_result = xsdbool(
      iv_input = '&quot;' OR iv_input = '&#34;' OR " QUOTATION MARK
      iv_input = '&#39;'  OR iv_input = '&#96;' OR " APOSTROPHE & GRAVE ACCENT
      iv_input = '&lt;'   OR iv_input = '&#60;' OR " LESS-THAN SIGN
      iv_input = '&gt;'   OR iv_input = '&#62;' OR " GREATER-THAN SIGN
      iv_input = '&#124;' ). " VERTICAL LINE

  ENDMETHOD.


  METHOD is_single_whitespace.

    IF is_op-action <> c_action-equal.
      rv_result = abap_false.
      RETURN.
    ENDIF.
    IF is_op-end_in_before - is_op-start_in_before <> 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_string) = _join( _slice( it_tokens = it_tokens
                                     iv_start  = is_op-start_in_before
                                     iv_end    = _get_end( is_op-end_in_before ) ) ).

    rv_result = xsdbool( lv_string = ` ` ). " single space

  ENDMETHOD.


  METHOD is_tag.

    IF mv_with_img IS INITIAL.
      FIND REGEX '<[^>]+>' IN iv_token.
    ELSE.
      FIND REGEX '<(?!img)[^>]+>' IN iv_token.
    ENDIF.

    rv_result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_tag_begin.

    rv_result = xsdbool( iv_token(1) = c_tag-begin AND mv_with_tags = abap_true ).

  ENDMETHOD.


  METHOD is_tag_end.

    rv_result = xsdbool( iv_token(1) = c_tag-end AND mv_with_tags = abap_true ).

  ENDMETHOD.


  METHOD is_whitespace.

    "/^\s+$/
    DATA(lv_whitespace) = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    rv_result = xsdbool( iv_input CO lv_whitespace ).

  ENDMETHOD.


  METHOD operation.

    CASE is_op-action.
      WHEN c_action-equal.
        DATA(lt_val) = _slice( it_tokens = it_before_tokens
                               iv_start  = is_op-start_in_before
                               iv_end    = _get_end( is_op-end_in_before ) ).
        rv_result = _join( lt_val ).

      WHEN c_action-insert OR c_action-insmod.
        IF mv_inserts = abap_true.
          lt_val = _slice( it_tokens = it_after_tokens
                           iv_start  = is_op-start_in_after
                           iv_end    = _get_end( is_op-end_in_after ) ).
          IF is_op-action = c_action-insert.
            rv_result = wrap( iv_tag     = c_tag-ins
                              it_content = lt_val ).
          ELSE.
            rv_result = wrap( iv_tag     = c_tag-insmod
                              it_content = lt_val ).
          ENDIF.
        ENDIF.

      WHEN c_action-delete OR c_action-delmod.
        IF mv_deletes = abap_true.
          lt_val = _slice( it_tokens = it_before_tokens
                           iv_start  = is_op-start_in_before
                           iv_end    = _get_end( is_op-end_in_before ) ).
          IF is_op-action = c_action-delete.
            rv_result = wrap( iv_tag     = c_tag-del
                              it_content = lt_val ).
          ELSE.
            rv_result = wrap( iv_tag     = c_tag-delmod
                              it_content = lt_val ).
          ENDIF.
        ENDIF.

      WHEN c_action-replace.
        DATA(ls_op_del) = is_op.
        DATA(ls_op_ins) = is_op.
        IF mv_css_classes = abap_true.
          ls_op_del-action = c_action-delmod.
          ls_op_ins-action = c_action-insmod.
        ELSE.
          ls_op_del-action = c_action-delete.
          ls_op_ins-action = c_action-insert.
        ENDIF.
        rv_result = operation( is_op            = ls_op_del
                               it_before_tokens = it_before_tokens
                               it_after_tokens  = it_after_tokens )
                 && operation( is_op            = ls_op_ins
                               it_before_tokens = it_before_tokens
                               it_after_tokens  = it_after_tokens ).

    ENDCASE.

  ENDMETHOD.


  METHOD recurs_find_matching_blocks.

    DATA(ls_match) = find_match( it_before_tokens         = it_before_tokens
                                 it_after_tokens          = it_after_tokens
                                 it_index_before_in_after = it_index_before_in_after
                                 iv_start_in_before       = iv_start_in_before
                                 iv_end_in_before         = iv_end_in_before
                                 iv_start_in_after        = iv_start_in_after
                                 iv_end_in_after          = iv_end_in_after ).

    IF ls_match IS NOT INITIAL.
      IF iv_start_in_before < ls_match-start_in_before AND iv_start_in_after < ls_match-start_in_after.
        recurs_find_matching_blocks(
          EXPORTING
            it_before_tokens         = it_before_tokens
            it_after_tokens          = it_after_tokens
            it_index_before_in_after = it_index_before_in_after
            iv_start_in_before       = iv_start_in_before
            iv_end_in_before         = ls_match-start_in_before
            iv_start_in_after        = iv_start_in_after
            iv_end_in_after          = ls_match-start_in_after
          CHANGING
            ct_matching_blocks       = ct_matching_blocks ).
      ENDIF.

      APPEND ls_match TO ct_matching_blocks.

      IF ls_match-end_in_before <= iv_end_in_before AND ls_match-end_in_after <= iv_end_in_after.
        recurs_find_matching_blocks(
          EXPORTING
            it_before_tokens         = it_before_tokens
            it_after_tokens          = it_after_tokens
            it_index_before_in_after = it_index_before_in_after
            iv_start_in_before       = ls_match-end_in_before + 1
            iv_end_in_before         = iv_end_in_before
            iv_start_in_after        = ls_match-end_in_after + 1
            iv_end_in_after          = iv_end_in_after
          CHANGING
            ct_matching_blocks       = ct_matching_blocks ).
      ENDIF.
    ENDIF.

    rt_result = ct_matching_blocks.

  ENDMETHOD.


  METHOD render_operations.

    LOOP AT it_operations INTO DATA(ls_op).

      rv_result = rv_result && operation( is_op            = ls_op
                                          it_before_tokens = it_before_tokens
                                          it_after_tokens  = it_after_tokens ).

    ENDLOOP.

  ENDMETHOD.


  METHOD render_simple.

    IF iv_before = iv_after.
      rv_result = iv_before.
    ELSEIF iv_before IS INITIAL AND iv_after IS INITIAL.
      rv_result = ''.
    ELSEIF iv_before IS INITIAL.
      IF mv_inserts = abap_true.
        rv_result = |<{ c_tag-ins }>{ iv_after }</{ c_tag-ins }>|.
      ELSE.
        rv_result = ''.
      ENDIF.
    ELSEIF iv_after IS INITIAL.
      IF mv_deletes = abap_true.
        rv_result = |<{ c_tag-del }>{ iv_before }</{ c_tag-del }>|.
      ELSE.
        rv_result = ''.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD wrap.

    DATA(lv_length)   = lines( it_content ).
    DATA(lv_position) = 0.

    DO.
      IF lv_position >= lv_length.
        EXIT.
      ENDIF.

      DATA(lt_non_tags) = consecutive_where( iv_start   = lv_position
                                             it_content = it_content
                                             is_tag     = abap_false ).
      lv_position = lv_position + lines( lt_non_tags ).

      IF lines( lt_non_tags ) <> 0.
        rv_result = rv_result &&
          |<{ iv_tag(3) }{ _get_class( iv_tag ) }>| && _join( lt_non_tags ) && |</{ iv_tag(3) }>|.
      ENDIF.

      IF lv_position >= lv_length.
        EXIT.
      ENDIF.

      DATA(lt_tags) = consecutive_where( iv_start   = lv_position
                                         it_content = it_content
                                         is_tag     = abap_true ).
      lv_position = lv_position + lines( lt_tags ).

      IF lines( lt_tags ) <> 0.
        rv_result = rv_result && _join( lt_tags ).
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD zif_differ_htmldiff~htmldiff.

    mv_with_img  = iv_with_img.
    mv_with_tags = abap_true.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rv_result = render_simple( iv_before  = iv_before
                                 iv_after   = iv_after ).
    ELSE.
      DATA(lt_before_tokens) = html_to_tokens( iv_before ).
      DATA(lt_after_tokens)  = html_to_tokens( iv_after ).

      DATA(lt_ops) = calculate_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens ).

      rv_result = render_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens
                                     it_operations    = lt_ops ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_differ_htmldiff~textdiff.

    mv_with_img  = abap_false.
    mv_with_tags = abap_false.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rv_result = render_simple( iv_before  = iv_before
                                 iv_after   = iv_after ).
    ELSE.
      DATA(lt_before_tokens) = html_to_tokens( iv_before ).
      DATA(lt_after_tokens)  = html_to_tokens( iv_after ).

      DATA(lt_ops) = calculate_operations( it_before_tokens = lt_before_tokens
                                           it_after_tokens  = lt_after_tokens ).

      rv_result = render_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens
                                     it_operations    = lt_ops ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_class.

    IF mv_css_classes = abap_true.
      CASE iv_tag.
        WHEN c_tag-ins.
          DATA(lv_class) = c_tag_class-insert.
        WHEN c_tag-del.
          lv_class = c_tag_class-delete.
        WHEN c_tag-insmod OR c_tag-delmod.
          lv_class = c_tag_class-replace.
        WHEN OTHERS.
          ASSERT 0 = 1. " Unknown tag
      ENDCASE.

      rv_result = | class="{ lv_class }"|.
    ENDIF.

  ENDMETHOD.


  METHOD _get_end.
    IF iv_end = -1.
      rv_result = 2 ** 31.
    ELSE.
      rv_result = iv_end + 1.
    ENDIF.
  ENDMETHOD.


  METHOD _inject.
    mv_with_tags = iv_with_tags.
  ENDMETHOD.


  METHOD _join.

    LOOP AT it_tokens INTO DATA(lv_token).

      IF sy-tabix > 1.
        rv_result = rv_result && iv_separator.
      ENDIF.

      rv_result = rv_result && lv_token.

    ENDLOOP.

  ENDMETHOD.


  METHOD _new_match.

    rs_result-start_in_before = iv_start_in_before.
    rs_result-start_in_after  = iv_start_in_after.
    rs_result-length          = iv_length.
    rs_result-end_in_before   = iv_start_in_before + iv_length - 1.
    rs_result-end_in_after    = iv_start_in_after  + iv_length - 1.

  ENDMETHOD.


  METHOD _new_operation.

    rs_result-action          = iv_action.
    rs_result-start_in_before = iv_start_in_before.
    rs_result-start_in_after  = iv_start_in_after.
    rs_result-end_in_before   = iv_end_in_before.
    rs_result-end_in_after    = iv_end_in_after.

  ENDMETHOD.


  METHOD _slice.

    " select from start to end (end not included!)
    LOOP AT it_tokens INTO DATA(lv_token) FROM iv_start + 1 TO iv_end.

      APPEND lv_token TO rt_result.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
