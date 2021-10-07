CLASS zcl_abap_differ DEFINITION
  PUBLIC
  CREATE PUBLIC.

* ABAP HTML Diff (c) 2021 by Marc Bernard Tools
* This  is a port of JavaScript        (https://github.com/alaorneto/htmldiffer, no license)
* which is a port of CoffeeScript      (https://github.com/tnwinc/htmldiff.js, MIT license)
* which is a port of the original Ruby (https://github.com/myobie/htmldiff, MIT license)
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_operation,
        action          TYPE string,
        start_in_before TYPE i,
        end_in_before   TYPE i,
        start_in_after  TYPE i,
        end_in_after    TYPE i,
      END OF ty_operation,
      ty_operations TYPE STANDARD TABLE OF ty_operation WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_action,
        none    TYPE string VALUE 'none',
        equal   TYPE string VALUE 'equal',
        insert  TYPE string VALUE 'insert',
        delete  TYPE string VALUE 'delete',
        insmod  TYPE string VALUE 'insmod',
        delmod  TYPE string VALUE 'delmod',
        replace TYPE string VALUE 'replace',
      END OF c_action.

    METHODS constructor
      IMPORTING
        !iv_inserts      TYPE abap_bool DEFAULT abap_true
        !iv_deletes      TYPE abap_bool DEFAULT abap_true
        !iv_with_classes TYPE abap_bool DEFAULT abap_false.

    METHODS htmldiff
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
        !iv_with_img     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS textdiff
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS diff
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_operations.

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
      ty_locations TYPE STANDARD TABLE OF ty_location WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_index_row,
        token     TYPE ty_token,
        locations TYPE ty_locations,
      END OF ty_index_row,
      ty_index_tab TYPE STANDARD TABLE OF ty_index_row WITH DEFAULT KEY.

    METHODS is_character
      IMPORTING
        iv_char          TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_quote
      IMPORTING
        iv_string        TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_whitespace
      IMPORTING
        iv_string        TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_tag
      IMPORTING
        iv_token         TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS isnt_tag
      IMPORTING
        iv_token         TYPE ty_token
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS html_to_tokens
      IMPORTING
        iv_html          TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS find_match
      IMPORTING
        it_before_tokens         TYPE ty_tokens
        it_after_tokens          TYPE ty_tokens
        it_index_before_in_after TYPE ty_index_tab
        iv_start_in_before       TYPE i
        iv_end_in_before         TYPE i
        iv_start_in_after        TYPE i
        iv_end_in_after          TYPE i
      RETURNING
        VALUE(rs_result)         TYPE ty_match.

    METHODS find_matching_blocks
      IMPORTING
        it_before_tokens TYPE ty_tokens
        it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rt_result) TYPE ty_matches.

    METHODS recurs_find_matching_blocks
      IMPORTING
        it_before_tokens         TYPE ty_tokens
        it_after_tokens          TYPE ty_tokens
        it_index_before_in_after TYPE ty_index_tab
        iv_start_in_before       TYPE i
        iv_end_in_before         TYPE i
        iv_start_in_after        TYPE i
        iv_end_in_after          TYPE i
      CHANGING
        ct_matching_blocks       TYPE ty_matches
      RETURNING
        VALUE(rt_result)         TYPE ty_matches.

    METHODS create_index
      IMPORTING
        it_find_these    TYPE ty_tokens
        it_in_these      TYPE ty_tokens
      RETURNING
        VALUE(rt_result) TYPE ty_index_tab.

    METHODS calculate_operations
      IMPORTING
        it_before_tokens TYPE ty_tokens
        it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rt_result) TYPE ty_operations.

    METHODS calculate_simple
      IMPORTING
        !iv_before       TYPE string
        !iv_after        TYPE string
      RETURNING
        VALUE(rt_result) TYPE ty_operations.

    METHODS is_single_whitespace
      IMPORTING
        is_op            TYPE ty_operation
        it_tokens        TYPE ty_tokens
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS consecutive_where
      IMPORTING
        iv_start         TYPE i
        it_content       TYPE ty_tokens
        is_tag           TYPE abap_bool
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS wrap
      IMPORTING
        iv_tag           TYPE ty_token
        it_content       TYPE ty_tokens
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS operation
      IMPORTING
        is_op            TYPE ty_operation
        it_before_tokens TYPE ty_tokens
        it_after_tokens  TYPE ty_tokens
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS render_operations
      IMPORTING
        it_before_tokens TYPE ty_tokens
        it_after_tokens  TYPE ty_tokens
        it_operations    TYPE ty_operations
      RETURNING
        VALUE(rv_result) TYPE string.

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
      BEGIN OF c_class,
        insert  TYPE string VALUE 'diffins',
        delete  TYPE string VALUE 'diffdel',
        replace TYPE string VALUE 'diffmod',
      END OF c_class.

    DATA mv_with_classes TYPE abap_bool.
    DATA mv_inserts TYPE abap_bool.
    DATA mv_deletes TYPE abap_bool.
    DATA mv_with_img TYPE abap_bool.
    DATA mv_with_tags TYPE abap_bool.

    METHODS slice
      IMPORTING
        !it_tokens       TYPE ty_tokens
        !iv_start        TYPE i
        !iv_end          TYPE i
      RETURNING
        VALUE(rt_result) TYPE ty_tokens.

    METHODS join
      IMPORTING
        !it_tokens       TYPE ty_tokens
        !iv_separator    TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS new_match
      IMPORTING
        !iv_start_in_before TYPE i
        !iv_start_in_after  TYPE i
        !iv_length          TYPE i
      RETURNING
        VALUE(rs_result)    TYPE ty_match.

    METHODS new_operation
      IMPORTING
        !iv_action          TYPE string
        !iv_start_in_before TYPE i OPTIONAL
        !iv_start_in_after  TYPE i OPTIONAL
        !iv_end_in_before   TYPE i OPTIONAL
        !iv_end_in_after    TYPE i OPTIONAL
      RETURNING
        VALUE(rs_result)    TYPE ty_operation.

    METHODS get_class
      IMPORTING
        !iv_tag          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.



CLASS zcl_abap_differ IMPLEMENTATION.


  METHOD calculate_operations.

    DATA:
      lv_action             TYPE string,
      lv_end_in_before      TYPE i,
      lv_end_in_after       TYPE i,
      ls_last_op            TYPE ty_operation,
      ls_match              TYPE ty_match,
      lt_matches            TYPE ty_matches,
      ls_op                 TYPE ty_operation,
      lt_operations         TYPE ty_operations,
      lv_position_in_after  TYPE i,
      lv_position_in_before TYPE i,
      lt_post_processed     TYPE ty_operations,
      lv_whitespace         TYPE abap_bool.

    IF it_before_tokens IS INITIAL.
      ASSERT 0 = 1. " no before_tokens?
    ENDIF.

    IF it_after_tokens IS INITIAL.
      ASSERT 0 = 1. " no after_tokens?
    ENDIF.

    lv_position_in_before = lv_position_in_after = 0.

    lt_matches = find_matching_blocks( it_before_tokens = it_before_tokens
                                       it_after_tokens  = it_after_tokens ).

    APPEND new_match(
      iv_start_in_before = lines( it_before_tokens )
      iv_start_in_after  = lines( it_after_tokens )
      iv_length          = 0 )
      TO lt_matches.

    LOOP AT lt_matches INTO ls_match.

      IF lv_position_in_before = ls_match-start_in_before.
        IF lv_position_in_after = ls_match-start_in_after.
          lv_action = c_action-none.
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
          lv_end_in_before = ls_match-start_in_before - 1.
        ELSE.
          lv_end_in_before = 0.
        ENDIF.
        IF lv_action <> c_action-delete.
          lv_end_in_after = ls_match-start_in_after - 1.
        ELSE.
          lv_end_in_after = 0.
        ENDIF.

        APPEND new_operation(
          iv_action          = lv_action
          iv_start_in_before = lv_position_in_before
          iv_end_in_before   = lv_end_in_before
          iv_start_in_after  = lv_position_in_after
          iv_end_in_after    = lv_end_in_after
          ) TO lt_operations.
      ENDIF.

      IF ls_match-length <> 0.
        APPEND new_operation(
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

    ls_last_op = new_operation( iv_action = c_action-none ).

    LOOP AT lt_operations INTO ls_op.

      lv_whitespace = is_single_whitespace( is_op     = ls_op
                                            it_tokens = it_before_tokens ).

      IF ( lv_whitespace = abap_true OR ls_op-action = c_action-replace ) AND ls_last_op-action = c_action-replace.
        ls_last_op-end_in_before = ls_op-end_in_before.
        ls_last_op-end_in_after  = ls_op-end_in_after.
      ELSE.
        APPEND ls_op TO lt_post_processed.
        ls_last_op = ls_op.
      ENDIF.

    ENDLOOP.

    rt_result = lt_post_processed.

  ENDMETHOD.


  METHOD calculate_simple.

    DATA lv_action TYPE string.

    IF iv_before = iv_after.
      lv_action = c_action-none.
    ELSEIF iv_after IS INITIAL.
      IF mv_deletes = abap_true.
        lv_action = c_action-delete.
      ENDIF.
    ELSEIF iv_before IS INITIAL.
      IF mv_inserts = abap_true.
        lv_action = c_action-insert.
      ENDIF.
    ENDIF.

    IF lv_action IS NOT INITIAL.
      APPEND new_operation(
        iv_action          = lv_action
        iv_start_in_before = 1
        iv_end_in_before   = strlen( iv_before )
        iv_start_in_after  = 1
        iv_end_in_after    = strlen( iv_after )
        ) TO rt_result.
    ENDIF.

  ENDMETHOD.


  METHOD consecutive_where.

    DATA:
      lv_answer TYPE abap_bool,
      lv_index  TYPE i,
      lv_token  TYPE ty_token.

    LOOP AT it_content INTO lv_token FROM iv_start + 1.

      IF is_tag = abap_true.
        lv_answer = is_tag( lv_token ).
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
    mv_inserts = iv_inserts.
    mv_deletes = iv_deletes.
    mv_with_classes = iv_with_classes.
  ENDMETHOD.


  METHOD create_index.

    DATA:
      lv_idx   TYPE i,
      lt_index TYPE ty_index_tab,
      lv_token TYPE ty_token.

    FIELD-SYMBOLS:
      <ls_index> TYPE ty_index_row,
      <lv_token> TYPE ty_token.

    LOOP AT it_find_these ASSIGNING <lv_token>.

      READ TABLE lt_index ASSIGNING <ls_index> WITH TABLE KEY token = <lv_token>.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_index ASSIGNING <ls_index>.
        <ls_index>-token = <lv_token>.
      ENDIF.

      lv_idx = 1.
      DO.
        LOOP AT it_in_these INTO lv_token FROM lv_idx WHERE table_line = <lv_token>.
          lv_idx = sy-tabix.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_idx = lv_idx - 1.
        COLLECT lv_idx INTO <ls_index>-locations.
        lv_idx = lv_idx + 2.
        IF lv_idx > lines( it_in_these ).
          EXIT.
        ENDIF.
      ENDDO.

    ENDLOOP.

    rt_result = lt_index.

  ENDMETHOD.


  METHOD diff.

    DATA:
      lt_before_tokens TYPE ty_tokens,
      lt_after_tokens  TYPE ty_tokens.

    mv_with_img = abap_false.
    mv_with_tags = abap_false.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rt_result = calculate_simple( iv_before = iv_before
                                    iv_after  = iv_after ).
    ELSE.
      lt_before_tokens = html_to_tokens( iv_before ).
      lt_after_tokens  = html_to_tokens( iv_after ).

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
      lv_best_match_in_after  TYPE i,
      lv_best_match_in_before TYPE i,
      lv_best_match_length    TYPE i,
      lv_index_in_after       TYPE i,
      lv_index_in_before      TYPE i,
      lv_idx                  TYPE i,
      ls_index_row            TYPE ty_index_row,
      lt_locations_in_after   TYPE ty_locations,
      lv_looking_for          TYPE ty_token,
      ls_match                TYPE ty_match,
      lv_key                  TYPE i,
      ls_keyval               TYPE ty_keyval,
      lt_match_length_at      TYPE ty_keyvals,
      lv_new_match_length     TYPE i,
      lt_new_match_length_at  TYPE ty_keyvals.

    lv_best_match_in_before = iv_start_in_before.
    lv_best_match_in_after  = iv_start_in_after.
    lv_best_match_length    = 0.

    CLEAR lt_match_length_at.

    LOOP AT it_before_tokens INTO lv_looking_for FROM iv_start_in_before + 1 TO iv_end_in_before + 1.
      lv_index_in_before = sy-tabix - 1.

      CLEAR lt_new_match_length_at.

      READ TABLE it_index_before_in_after INTO ls_index_row WITH KEY token = lv_looking_for.
      IF sy-subrc = 0.
        lt_locations_in_after = ls_index_row-locations.
      ELSE.
        BREAK-POINT.
      ENDIF.

      LOOP AT lt_locations_in_after INTO lv_index_in_after.

        IF lv_index_in_after < iv_start_in_after.
          CONTINUE.
        ENDIF.
        IF lv_index_in_after >= iv_end_in_after.
          EXIT.
        ENDIF.

        lv_key = lv_index_in_after - 1.
        READ TABLE lt_match_length_at INTO ls_keyval WITH TABLE KEY key = lv_key.
        IF sy-subrc <> 0.
          ls_keyval-key = lv_key.
          ls_keyval-val = 0.
          INSERT ls_keyval INTO TABLE lt_match_length_at.
        ENDIF.

        lv_new_match_length = ls_keyval-val + 1.

        ls_keyval-key = lv_index_in_after.
        ls_keyval-val = lv_new_match_length.
        INSERT ls_keyval INTO TABLE lt_new_match_length_at.

        IF lv_new_match_length > lv_best_match_length.
          lv_best_match_in_before = lv_index_in_before - lv_new_match_length + 1.
          lv_best_match_in_after  = lv_index_in_after  - lv_new_match_length + 1.
          lv_best_match_length    = lv_new_match_length.
        ENDIF.

      ENDLOOP.

      lt_match_length_at = lt_new_match_length_at.

    ENDLOOP.

    IF lv_best_match_length <> 0.
      ls_match = new_match( iv_start_in_before = lv_best_match_in_before
                            iv_start_in_after  = lv_best_match_in_after
                            iv_length          = lv_best_match_length ).
    ENDIF.

    rs_result = ls_match.

  ENDMETHOD.


  METHOD find_matching_blocks.

    DATA:
      lt_index_of_before_in_after TYPE ty_index_tab,
      lt_matching_blocks          TYPE ty_matches.

    lt_index_of_before_in_after = create_index( it_find_these = it_before_tokens
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


  METHOD get_class.

    DATA lv_class TYPE string.

    IF mv_with_classes = abap_true.
      CASE iv_tag.
        WHEN c_tag-ins.
          lv_class = c_class-insert.
        WHEN c_tag-del.
          lv_class = c_class-delete.
        WHEN c_tag-insmod OR c_tag-delmod.
          lv_class = c_class-replace.
        WHEN OTHERS.
          ASSERT 0 = 1. " Unknown tag
      ENDCASE.

      rv_result = | class="{ lv_class }"|.
    ENDIF.

  ENDMETHOD.


  METHOD htmldiff.

    DATA:
      lt_before_tokens TYPE ty_tokens,
      lt_after_tokens  TYPE ty_tokens,
      lt_ops           TYPE ty_operations.

    mv_with_img = iv_with_img.
    mv_with_tags = abap_true.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rv_result = render_simple( iv_before  = iv_before
                                 iv_after   = iv_after ).
    ELSE.
      lt_before_tokens = html_to_tokens( iv_before ).
      lt_after_tokens  = html_to_tokens( iv_after ).

      lt_ops = calculate_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens ).

      rv_result = render_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens
                                     it_operations    = lt_ops ).
    ENDIF.

  ENDMETHOD.


  METHOD html_to_tokens.

    DATA:
      lv_char         TYPE string,
      lv_current_word TYPE string,
      lv_idx          TYPE i,
      lv_mode         TYPE string,
      lt_words        TYPE ty_tokens.

    lv_mode = c_mode-char.

    DO strlen( iv_html ) TIMES.
      lv_char = iv_html+lv_idx(1).

      CASE lv_mode.
        WHEN c_mode-tag.
          IF lv_char = c_tag-end AND mv_with_tags = abap_true.
            lv_current_word = lv_current_word && c_tag-end.
            APPEND lv_current_word TO lt_words.
            lv_current_word = ''.
            lv_mode = c_mode-char.
          ELSE.
            lv_current_word = lv_current_word && lv_char.
          ENDIF.

        WHEN c_mode-char.
          IF lv_char = c_tag-begin AND mv_with_tags = abap_true.
            IF NOT lv_current_word IS INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = c_tag-begin.
            lv_mode = c_mode-tag.
          ELSEIF is_whitespace( lv_char ) = abap_true.
            IF NOT lv_current_word IS INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = lv_char.
            lv_mode = c_mode-whitespace.
          ELSEIF is_character( lv_char ) = abap_true.
            lv_current_word = lv_current_word && lv_char.
            IF is_quote( lv_current_word ) = abap_true.
              APPEND lv_current_word TO lt_words.
              lv_current_word = ''.
            ENDIF.
          ELSE.
            IF NOT lv_current_word IS INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = lv_char.
          ENDIF.

        WHEN c_mode-whitespace.
          IF lv_char = c_tag-begin.
            IF NOT lv_current_word IS INITIAL.
              APPEND lv_current_word TO lt_words.
            ENDIF.
            lv_current_word = c_tag-begin.
            lv_mode = c_mode-tag.
          ELSEIF is_whitespace( lv_char ) = abap_true.
            lv_current_word = lv_current_word && lv_char.
          ELSE.
            IF NOT lv_current_word IS INITIAL.
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

    IF NOT lv_current_word IS INITIAL.
      APPEND lv_current_word TO lt_words.
    ENDIF.

    rt_result = lt_words.

  ENDMETHOD.


  METHOD isnt_tag.
    IF is_tag( iv_token ) = abap_true.
      rv_result = abap_false.
    ELSE.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_character.

    FIND REGEX '[\w\#@;]' IN iv_char.
    IF sy-subrc = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_quote.

    " https://www.w3schools.com/charsets/ref_utf_basic_latin.asp
    IF iv_string = '&quot;' OR iv_string = '&#34;' OR " QUOTATION MARK
       iv_string = '&#39;'  OR iv_string = '&#96;' OR " APOSTROPHE & GRAVE ACCENT
       iv_string = '&lt;'   OR iv_string = '&#60;' OR " LESS-THAN SIGN
       iv_string = '&gt;'   OR iv_string = '&#62;' OR " GREATER-THAN SIGN
       iv_string = '&#124;'. " VERTICAL LINE
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_single_whitespace.

    DATA:
      lv_str TYPE string.

    IF is_op-action <> c_action-equal.
      rv_result = abap_false.
      RETURN.
    ENDIF.
    IF is_op-end_in_before - is_op-start_in_before <> 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    lv_str = join( slice( it_tokens = it_tokens
                          iv_start  = is_op-start_in_before
                          iv_end    = is_op-end_in_before + 1 ) ).
    IF is_whitespace( lv_str ) = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_tag.

    IF mv_with_img IS INITIAL.
      FIND REGEX '<[^>]+>' IN iv_token.
    ELSE.
      FIND REGEX '<(?!img)[^>]+>' IN iv_token.
    ENDIF.
    IF sy-subrc = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_whitespace.

    DATA: lv_whitespace TYPE string.

    "/^\s+$/
    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    IF iv_string CO lv_whitespace.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD join.

    DATA: lv_token TYPE ty_token.

    LOOP AT it_tokens INTO lv_token.

      IF sy-tabix > 1.
        rv_result = rv_result && iv_separator.
      ENDIF.

      rv_result = rv_result && lv_token.

    ENDLOOP.

  ENDMETHOD.


  METHOD new_match.

    rs_result-start_in_before = iv_start_in_before.
    rs_result-start_in_after  = iv_start_in_after.
    rs_result-length          = iv_length.
    rs_result-end_in_before   = iv_start_in_before + iv_length - 1.
    rs_result-end_in_after    = iv_start_in_after  + iv_length - 1.

  ENDMETHOD.


  METHOD new_operation.

    rs_result-action          = iv_action.
    rs_result-start_in_before = iv_start_in_before.
    rs_result-start_in_after  = iv_start_in_after.
    rs_result-end_in_before   = iv_end_in_before.
    rs_result-end_in_after    = iv_end_in_after.

  ENDMETHOD.


  METHOD operation.

    DATA:
      lt_val    TYPE ty_tokens,
      ls_op_del TYPE ty_operation,
      ls_op_ins TYPE ty_operation.

    CASE is_op-action.
      WHEN c_action-equal.
        lt_val = slice( it_tokens = it_before_tokens
                        iv_start  = is_op-start_in_before
                        iv_end    = is_op-end_in_before ).
        rv_result = join( lt_val ).

      WHEN c_action-insert OR c_action-insmod.
        IF mv_inserts = abap_true.
          lt_val = slice( it_tokens = it_after_tokens
                          iv_start  = is_op-start_in_after
                          iv_end    = is_op-end_in_after ).
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
          lt_val = slice( it_tokens = it_before_tokens
                          iv_start  = is_op-start_in_before
                          iv_end    = is_op-end_in_before ).
          IF is_op-action = c_action-delete.
            rv_result = wrap( iv_tag     = c_tag-del
                              it_content = lt_val ).
          ELSE.
            rv_result = wrap( iv_tag     = c_tag-delmod
                              it_content = lt_val ).
          ENDIF.
        ENDIF.

      WHEN c_action-replace.
        ls_op_del = ls_op_ins = is_op.
        IF mv_with_classes = abap_true.
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

    DATA: ls_match TYPE ty_match.

    ls_match = find_match( it_before_tokens         = it_before_tokens
                           it_after_tokens          = it_after_tokens
                           it_index_before_in_after = it_index_before_in_after
                           iv_start_in_before       = iv_start_in_before
                           iv_end_in_before         = iv_end_in_before
                           iv_start_in_after        = iv_start_in_after
                           iv_end_in_after          = iv_end_in_after ).

    IF NOT ls_match IS INITIAL.
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

    DATA:
      ls_op        TYPE ty_operation,
      lv_rendering TYPE string.

    LOOP AT it_operations INTO ls_op.

      lv_rendering = lv_rendering && operation( is_op            = ls_op
                                                it_before_tokens = it_before_tokens
                                                it_after_tokens  = it_after_tokens ).

    ENDLOOP.

    rv_result = lv_rendering.

  ENDMETHOD.


  METHOD render_simple.

    IF iv_before = iv_after.
      rv_result = iv_before.
    ELSEIF iv_after IS INITIAL.
      IF mv_deletes = abap_true.
        rv_result = |<{ c_tag-del }>{ iv_before }</{ c_tag-del }>|.
      ELSE.
        rv_result = ''.
      ENDIF.
    ELSEIF iv_before IS INITIAL.
      IF mv_inserts = abap_true.
        rv_result = |<{ c_tag-ins }>{ iv_after }</{ c_tag-ins }>|.
      ELSE.
        rv_result = ''.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD slice.

    DATA: lv_token TYPE ty_token.

    LOOP AT it_tokens INTO lv_token FROM iv_start + 1 TO iv_end + 1.

      APPEND lv_token TO rt_result.

    ENDLOOP.

  ENDMETHOD.


  METHOD textdiff.

    DATA:
      lt_before_tokens TYPE ty_tokens,
      lt_after_tokens  TYPE ty_tokens,
      lt_ops           TYPE ty_operations.

    mv_with_img = abap_false.
    mv_with_tags = abap_false.

    IF iv_before = iv_after OR iv_after IS INITIAL OR iv_before IS INITIAL.
      rv_result = render_simple( iv_before  = iv_before
                                 iv_after   = iv_after ).
    ELSE.
      lt_before_tokens = html_to_tokens( iv_before ).
      lt_after_tokens  = html_to_tokens( iv_after ).

      lt_ops = calculate_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens ).

      rv_result = render_operations( it_before_tokens = lt_before_tokens
                                     it_after_tokens  = lt_after_tokens
                                     it_operations    = lt_ops ).
    ENDIF.

  ENDMETHOD.


  METHOD wrap.

    DATA:
      lv_length    TYPE i,
      lv_position  TYPE i,
      lt_non_tags  TYPE ty_tokens,
      lt_tags      TYPE ty_tokens,
      lv_rendering TYPE string.

    lv_length = lines( it_content ).

    DO.
      IF lv_position >= lv_length.
        EXIT.
      ENDIF.

      lt_non_tags = consecutive_where( iv_start   = lv_position
                                       it_content = it_content
                                       is_tag     = abap_false ).
      lv_position = lv_position + lines( lt_non_tags ).

      IF lines( lt_non_tags ) <> 0.
        lv_rendering = |{ lv_rendering }<{ iv_tag(3) }{ get_class( iv_tag ) }>{ join( lt_non_tags ) }</{ iv_tag(3) }>|.
      ENDIF.

      IF lv_position >= lv_length.
        EXIT.
      ENDIF.

      lt_tags = consecutive_where( iv_start   = lv_position
                                   it_content = it_content
                                   is_tag     = abap_true ).
      lv_position = lv_position + lines( lt_tags ).

      IF lines( lt_tags ) <> 0.
        lv_rendering = lv_rendering && join( lt_tags ).
      ENDIF.
    ENDDO.

    rv_result = lv_rendering.

  ENDMETHOD.
ENDCLASS.
