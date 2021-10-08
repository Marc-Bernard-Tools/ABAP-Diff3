************************************************************************
* Helper Class
************************************************************************

CLASS ltcl_helper DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      format
        IMPORTING
          iv_string        TYPE string
        RETURNING
          VALUE(rv_result) TYPE string,

      diff
        IMPORTING
          iv_before        TYPE string
          iv_after         TYPE string
          iv_with_img      TYPE abap_bool DEFAULT abap_false
          iv_chinese       TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS ltcl_helper IMPLEMENTATION.

  METHOD format.
    rv_result = iv_string.
    REPLACE ALL OCCURRENCES OF '\n' IN rv_result WITH cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD diff.
    DATA lo_differ TYPE REF TO zcl_abap_differ.

    CREATE OBJECT lo_differ
      EXPORTING
        iv_with_classes    = abap_true
        iv_support_chinese = iv_chinese.

    rv_result = lo_differ->htmldiff(
      iv_before   = iv_before
      iv_after    = iv_after
      iv_with_img = iv_with_img ).
  ENDMETHOD.

ENDCLASS.

************************************************************************
* Tests from https://github.com/alaorneto/htmldiffer
************************************************************************

CLASS ltcl_differ_test_1 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mv_original TYPE string,
      mv_modified TYPE string,
      mo_differ   TYPE REF TO zcl_abap_differ.

    METHODS:
      setup,
      test_ignore_image FOR TESTING,
      test_with_image FOR TESTING.

ENDCLASS.

CLASS ltcl_differ_test_1 IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_differ.

    mv_original = ltcl_helper=>format( '\n'
      && '    <p>First paragraph.</p>\n'
      && '    <ul>\n'
      && '        <li>Item A</li>\n'
      && '        <li>Item B</li>\n'
      && '        <li>Item C</li>\n'
      && '    </ul>\n'
      && '    <img src="previous.jpg">\n'
      && '    <span>This is some interesting text.</span>\n' ).

    mv_modified = ltcl_helper=>format( '\n'
      && '    <p>First paragraph.</p>\n'
      && '    <ul>\n'
      && '        <li>Item A</li>\n'
      && '        <li>Item B</li>\n'
      && '        <li>Item D</li>\n'
      && '    </ul>\n'
      && '    <img src="next.jpg">\n'
      && '    <span>This is some new text.</span>\n' ).

  ENDMETHOD.

  METHOD test_ignore_image.

    DATA:
      lv_exp TYPE string,
      lv_act TYPE string.

    lv_exp = ltcl_helper=>format( '\n'
      && '    <p>First paragraph.</p>\n'
      && '    <ul>\n'
      && '        <li>Item A</li>\n'
      && '        <li>Item B</li>\n'
      && '        <li>Item <del>C</del><ins>D</ins></li>\n'
      && '    </ul>\n'
      && '    <img src="previous.jpg"><img src="next.jpg">\n'
      && '    <span>This is some <del>interesting</del><ins>new</ins> text.</span>\n' ).

    lv_act = mo_differ->htmldiff(
      iv_before   = mv_original
      iv_after    = mv_modified
      iv_with_img = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD test_with_image.

    DATA:
      lv_exp TYPE string,
      lv_act TYPE string.

    lv_exp = ltcl_helper=>format( '\n'
      && '    <p>First paragraph.</p>\n'
      && '    <ul>\n'
      && '        <li>Item A</li>\n'
      && '        <li>Item B</li>\n'
      && '        <li>Item <del>C</del><ins>D</ins></li>\n'
      && '    </ul>\n'
      && '    <del><img src="previous.jpg"></del><ins><img src="next.jpg"></ins>\n'
      && '    <span>This is some <del>interesting</del><ins>new</ins> text.</span>\n' ).

    lv_act = mo_differ->htmldiff(
      iv_before   = mv_original
      iv_after    = mv_modified
      iv_with_img = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.

************************************************************************
* Tests from https://github.com/myobie/htmldiff
************************************************************************

CLASS ltcl_differ_test_2 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      diff_text FOR TESTING,
      insert_a_letter_and_a_space FOR TESTING,
      remove_a_letter_and_a_space FOR TESTING,
      change_a_letter FOR TESTING,
      support_chinese FOR TESTING,
      support_img_tags_insertion FOR TESTING,
      support_img_tags_deletion FOR TESTING.

ENDCLASS.

CLASS ltcl_differ_test_2 IMPLEMENTATION.

  METHOD diff_text.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a word is here'
      iv_after  = 'a nother word is there' ).

    lv_exp = 'a<ins class="diffins"> nother</ins> word is <del class="diffmod">'
      && 'here</del><ins class="diffmod">there</ins>'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD insert_a_letter_and_a_space.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a c'
      iv_after  = 'a b c' ).

    lv_exp = 'a <ins class="diffins">b </ins>c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD remove_a_letter_and_a_space.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a b c'
      iv_after  = 'a c' ).

    lv_exp = 'a <del class="diffdel">b </del>c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD change_a_letter.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a b c'
      iv_after  = 'a d c' ).

    lv_exp = 'a <del class="diffmod">b</del><ins class="diffmod">d</ins> c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD support_chinese.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before  = '这个是中文内容, Ruby is the bast'
      iv_after   = '这是中国语内容,Ruby is the best language.'
      iv_chinese = abap_true ).

    lv_exp = '这<del class="diffdel">个</del>是中<del class="diffmod">文</del><ins class="diffmod">'
          && '国语</ins>内容<del class="diffmod">, Ruby</del><ins class="diffmod">,Ruby'
          && '</ins> is the <del class="diffmod">bast</del><ins class="diffmod">best language.</ins>'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD support_img_tags_insertion.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a b c'
      iv_after  = 'a b <img src="some_url" /> c'
      iv_with_img = abap_true ).

    lv_exp = 'a b <ins class="diffins"><img src="some_url" /> </ins>c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD support_img_tags_deletion.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = ltcl_helper=>diff(
      iv_before = 'a b <img src="some_url" /> c'
      iv_after  = 'a b c'
      iv_with_img = abap_true ).

    lv_exp = 'a b <del class="diffdel"><img src="some_url" /> </del>c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.

************************************************************************
* Tests from https://github.com/myobie/htmldiff
* https://github.com/tnwinc/htmldiff.js/tree/master/test
************************************************************************

CLASS ltcl_calculate_operations DEFINITION DEFERRED.
CLASS zcl_abap_differ DEFINITION LOCAL FRIENDS ltcl_calculate_operations.

CLASS ltcl_calculate_operations DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_differ TYPE REF TO zcl_abap_differ.

    METHODS:
      setup,

      calculate_operations
        IMPORTING
          !iv_before TYPE string
          !iv_after  TYPE string
          !iv_count  TYPE i
          !iv_index  TYPE i
          !is_exp    TYPE zcl_abap_differ=>ty_operation,

      action_middle FOR TESTING,
      action_beginning FOR TESTING,
      action_end FOR TESTING,
      action_combo FOR TESTING.


ENDCLASS.

CLASS ltcl_calculate_operations IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_differ.
  ENDMETHOD.

  METHOD calculate_operations.

    DATA:
      lt_before TYPE zcl_abap_differ=>ty_tokens,
      lt_after  TYPE zcl_abap_differ=>ty_tokens,
      ls_op     TYPE zcl_abap_differ=>ty_operation,
      lt_ops    TYPE zcl_abap_differ=>ty_operations.

    SPLIT iv_before AT space INTO TABLE lt_before.
    SPLIT iv_after  AT space INTO TABLE lt_after.

    REPLACE ALL OCCURRENCES OF '_' IN TABLE lt_before WITH ` `.
    REPLACE ALL OCCURRENCES OF '_' IN TABLE lt_after  WITH ` `.

    lt_ops = mo_differ->calculate_operations( it_before_tokens = lt_before
                                              it_after_tokens  = lt_after ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_ops )
      exp = iv_count ).

    READ TABLE lt_ops INTO ls_op INDEX iv_index.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_op
      exp = is_exp ).

  ENDMETHOD.

  METHOD action_middle.

    DATA ls_exp TYPE zcl_abap_differ=>ty_operation.

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 1.
    ls_exp-end_in_before   = 1.
    ls_exp-start_in_after  = 1.
    ls_exp-end_in_after    = 1.

    calculate_operations( iv_before = 'working on it'
                          iv_after  = 'working in it'
                          iv_count  = 3
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'insert'.
    ls_exp-start_in_before = 1.
    ls_exp-end_in_before   = -1.
    ls_exp-start_in_after  = 1.
    ls_exp-end_in_after    = 1.

    calculate_operations( iv_before = 'working it'
                          iv_after  = 'working on it'
                          iv_count  = 3
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'insert'.
    ls_exp-start_in_before = 1.
    ls_exp-end_in_before   = -1.
    ls_exp-start_in_after  = 1.
    ls_exp-end_in_after    = 3.

    calculate_operations( iv_before = 'working it'
                          iv_after  = 'working all up on it'
                          iv_count  = 3
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'delete'.
    ls_exp-start_in_before = 2.
    ls_exp-end_in_before   = 4.
    ls_exp-start_in_after  = 2.
    ls_exp-end_in_after    = -1.

    calculate_operations( iv_before = 'this is a lot of text'
                          iv_after  = 'this is text'
                          iv_count  = 3
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'equal'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = 5.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = 5.

    calculate_operations( iv_before = 'this is what it sounds like'
                          iv_after  = 'this is what it sounds like'
                          iv_count  = 1
                          iv_index  = 1
                          is_exp    = ls_exp ).

  ENDMETHOD.

  METHOD action_beginning.

    DATA ls_exp TYPE zcl_abap_differ=>ty_operation.

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = 2.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = 1.

    calculate_operations( iv_before = 'I dont like veggies'
                          iv_after  = 'Joe loves veggies'
                          iv_count  = 2
                          iv_index  = 1
                          is_exp    = ls_exp ).

    ls_exp-action          = 'insert'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = -1.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = 1.

    calculate_operations( iv_before = 'dog'
                          iv_after  = 'the shaggy dog'
                          iv_count  = 2
                          iv_index  = 1
                          is_exp    = ls_exp ).

    ls_exp-action          = 'delete'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = 0.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = -1.

    calculate_operations( iv_before = 'awesome dog barks'
                          iv_after  = 'dog barks'
                          iv_count  = 2
                          iv_index  = 1
                          is_exp    = ls_exp ).

  ENDMETHOD.

  METHOD action_end.

    DATA ls_exp TYPE zcl_abap_differ=>ty_operation.

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 3.
    ls_exp-end_in_before   = 4.
    ls_exp-start_in_after  = 3.
    ls_exp-end_in_after    = 4.

    calculate_operations( iv_before = 'the dog bit the cat'
                          iv_after  = 'the dog bit a bird'
                          iv_count  = 2
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'insert'.
    ls_exp-start_in_before = 4.
    ls_exp-end_in_before   = -1.
    ls_exp-start_in_after  = 4.
    ls_exp-end_in_after    = 5.

    calculate_operations( iv_before = 'this is a dog'
                          iv_after  = 'this is a dog that barks'
                          iv_count  = 2
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'delete'.
    ls_exp-start_in_before = 4.
    ls_exp-end_in_before   = 5.
    ls_exp-start_in_after  = 4.
    ls_exp-end_in_after    = -1.

    calculate_operations( iv_before = 'this is a dog that barks'
                          iv_after  = 'this is a dog'
                          iv_count  = 2
                          iv_index  = 2
                          is_exp    = ls_exp ).

  ENDMETHOD.

  METHOD action_combo.

    DATA ls_exp TYPE zcl_abap_differ=>ty_operation.

    " There are a bunch of replaces, but, because whitespace is
    " tokenized, they are broken up with equals. We want to combine
    " them into a contiguous replace operation.

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = 4.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = 4.

    calculate_operations( iv_before = 'I _ am _ awesome'
                          iv_after  = 'You _ are _ great'
                          iv_count  = 1
                          iv_index  = 1
                          is_exp    = ls_exp ).

    " Don't absorb non-single-whitespace tokens

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 0.
    ls_exp-end_in_before   = 0.
    ls_exp-start_in_after  = 0.
    ls_exp-end_in_after    = 0.

    calculate_operations( iv_before = 'I __ am _ awesome'
                          iv_after  = 'You __ are _ great'
                          iv_count  = 3
                          iv_index  = 1
                          is_exp    = ls_exp ).

    ls_exp-action          = 'equal'.
    ls_exp-start_in_before = 1.
    ls_exp-end_in_before   = 1.
    ls_exp-start_in_after  = 1.
    ls_exp-end_in_after    = 1.

    calculate_operations( iv_before = 'I __ am _ awesome'
                          iv_after  = 'You __ are _ great'
                          iv_count  = 3
                          iv_index  = 2
                          is_exp    = ls_exp ).

    ls_exp-action          = 'replace'.
    ls_exp-start_in_before = 2.
    ls_exp-end_in_before   = 4.
    ls_exp-start_in_after  = 2.
    ls_exp-end_in_after    = 4.

    calculate_operations( iv_before = 'I __ am _ awesome'
                          iv_after  = 'You __ are _ great'
                          iv_count  = 3
                          iv_index  = 3
                          is_exp    = ls_exp ).

  ENDMETHOD.

ENDCLASS.

******

CLASS ltcl_diff DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_differ TYPE REF TO zcl_abap_differ.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_differ.
  ENDMETHOD.

  METHOD test.

    DATA lv_act TYPE string.

    " When both inputs are the same, should return the text
    cl_abap_unit_assert=>assert_equals(
      act = mo_differ->htmldiff( iv_before = 'input text'
                                 iv_after  = 'input text' )
      exp = 'input text' ).

    " When a letter is added, should mark the new letter
    cl_abap_unit_assert=>assert_equals(
      act = mo_differ->htmldiff( iv_before = 'input'
                                 iv_after  = 'input 2' )
      exp = 'input<ins> 2</ins>' ).

    " When an entire sentence is replaced, should replace the whole chunk
    lv_act = mo_differ->htmldiff( iv_before = 'this is what I had'
                                  iv_after  = 'and now we have a new one' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = '<del>this is what I had</del><ins>and now we have a new one</ins>' ).

  ENDMETHOD.

ENDCLASS.

******

CLASS ltcl_html_to_token DEFINITION DEFERRED.
CLASS zcl_abap_differ DEFINITION LOCAL FRIENDS ltcl_html_to_token.

CLASS ltcl_html_to_token DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_differ TYPE REF TO zcl_abap_differ.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_html_to_token IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_differ.
    mo_differ->_inject( abap_true ).
  ENDMETHOD.

  METHOD test.

    DATA:
      lv_token  TYPE zcl_abap_differ=>ty_token,
      lt_tokens TYPE zcl_abap_differ=>ty_tokens,
      lt_exp    TYPE zcl_abap_differ=>ty_tokens.

    " when called with text, should return 7
    lt_tokens = mo_differ->html_to_tokens( 'this is a test' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_tokens )
      exp = 7 ).

    " when called with html, should return 11
    lt_tokens = mo_differ->html_to_tokens( '<p>this is a <strong>test</strong></p>' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_tokens )
      exp = 11 ).

    " should identify contiguous whitespace as a single token
    lt_tokens = mo_differ->html_to_tokens( `a   b` ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_tokens )
      exp = 3 ).

    READ TABLE lt_tokens INTO lv_token INDEX 2.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_token
      exp = `   ` ).

    " should identify a single space as a single token
    lt_tokens = mo_differ->html_to_tokens( ` a b ` ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_tokens )
      exp = 5 ).

    READ TABLE lt_tokens INTO lv_token INDEX 5.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_token
      exp = ` ` ).

    " should identify self closing tags as tokens
    lt_tokens = mo_differ->html_to_tokens( '<p>hello</br>goodbye</p>' ).

    APPEND '<p>' TO lt_exp.
    APPEND 'hello' TO lt_exp.
    APPEND '</br>' TO lt_exp.
    APPEND 'goodbye' TO lt_exp.
    APPEND '</p>' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      act = lt_tokens
      exp = lt_exp ).

  ENDMETHOD.

ENDCLASS.

******

CLASS ltcl_find_matching_blocks DEFINITION DEFERRED.
CLASS zcl_abap_differ DEFINITION LOCAL FRIENDS ltcl_find_matching_blocks.

CLASS ltcl_find_matching_blocks DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA mo_differ TYPE REF TO zcl_abap_differ.

    METHODS:
      setup,

      index_tokens FOR TESTING,

      find_match
        IMPORTING
          !iv_before       TYPE string
          !iv_after        TYPE string
        RETURNING
          VALUE(rs_result) TYPE zcl_abap_differ=>ty_match,

      find_match_1 FOR TESTING,
      find_match_2 FOR TESTING,
      find_match_3 FOR TESTING,

      find_matching_blocks
        IMPORTING
          !iv_before       TYPE string
          !iv_after        TYPE string
        RETURNING
          VALUE(rt_result) TYPE zcl_abap_differ=>ty_matches,

      find_matching_blocks_1 FOR TESTING,
      find_matching_blocks_2 FOR TESTING.

ENDCLASS.

CLASS ltcl_find_matching_blocks IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_differ.
  ENDMETHOD.

  METHOD index_tokens.

    DATA:
      lt_find_these TYPE zcl_abap_differ=>ty_tokens,
      lt_in_these   TYPE zcl_abap_differ=>ty_tokens,
      lv_loc        TYPE zcl_abap_differ=>ty_location,
      ls_index      TYPE zcl_abap_differ=>ty_index_row,
      lt_index      TYPE zcl_abap_differ=>ty_index_tab.

    " When the items exist in the search target
    SPLIT 'a has' AT space INTO TABLE lt_find_these.
    SPLIT 'a apple has a worm' AT space INTO TABLE lt_in_these.

    lt_index = mo_differ->create_index( it_find_these = lt_find_these
                                        it_in_these   = lt_in_these ).

    " should find "a" twice
    READ TABLE lt_index INTO ls_index WITH TABLE KEY token = 'a'.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_index-locations )
      exp = 2 ).

    " should find "a" at 0
    READ TABLE ls_index-locations INTO lv_loc INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_loc
      exp = 0 ).

    " should find "a" at 3
    READ TABLE ls_index-locations INTO lv_loc INDEX 2.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_loc
      exp = 3 ).

    " should find "has" at 2
    READ TABLE lt_index INTO ls_index WITH TABLE KEY token = 'has'.
    cl_abap_unit_assert=>assert_subrc( ).

    READ TABLE ls_index-locations INTO lv_loc INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_loc
      exp = 2 ).

  ENDMETHOD.

  METHOD find_match.

    DATA:
      lt_before TYPE zcl_abap_differ=>ty_tokens,
      lt_after  TYPE zcl_abap_differ=>ty_tokens,
      ls_index  TYPE zcl_abap_differ=>ty_index_row,
      lt_index  TYPE zcl_abap_differ=>ty_index_tab.

    SPLIT iv_before AT space INTO TABLE lt_before.
    SPLIT iv_after  AT space INTO TABLE lt_after.

    lt_index = mo_differ->create_index( it_find_these = lt_before
                                        it_in_these   = lt_after ).

    rs_result = mo_differ->find_match( it_before_tokens         = lt_before
                                       it_after_tokens          = lt_after
                                       it_index_before_in_after = lt_index
                                       iv_start_in_before       = 0
                                       iv_end_in_before         = lines( lt_before )
                                       iv_start_in_after        = 0
                                       iv_end_in_after          = lines( lt_after ) ).

  ENDMETHOD.

  METHOD find_match_1.

    DATA:
      ls_exp   TYPE zcl_abap_differ=>ty_match,
      ls_match TYPE zcl_abap_differ=>ty_match.

    " When there is a match, should match the match
    ls_match = find_match( iv_before = 'a dog bites'
                           iv_after =  'a dog bites a man' ).

    ls_exp-start_in_before = 0.
    ls_exp-start_in_after  = 0.
    ls_exp-length          = 3.
    ls_exp-end_in_before   = 2.
    ls_exp-end_in_after    = 2.

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

  ENDMETHOD.

  METHOD find_match_2.

    DATA:
      ls_exp   TYPE zcl_abap_differ=>ty_match,
      ls_match TYPE zcl_abap_differ=>ty_match.

    " When the match is surrounded, should match with appropriate indexing
    ls_match = find_match( iv_before = 'dog bites'
                           iv_after =  'the dog bites a man' ).

    ls_exp-start_in_before = 0.
    ls_exp-start_in_after  = 1.
    ls_exp-length          = 2.
    ls_exp-end_in_before   = 1.
    ls_exp-end_in_after    = 2.

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

  ENDMETHOD.

  METHOD find_match_3.

    DATA:
      ls_exp   TYPE zcl_abap_differ=>ty_match,
      ls_match TYPE zcl_abap_differ=>ty_match.

    " When these is no match, should return nothing
    ls_match = find_match( iv_before = 'the rat squeaks'
                           iv_after =  'a dog bites a man' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

  ENDMETHOD.

  METHOD find_matching_blocks.

    DATA:
      lt_before TYPE zcl_abap_differ=>ty_tokens,
      lt_after  TYPE zcl_abap_differ=>ty_tokens,
      lt_index  TYPE zcl_abap_differ=>ty_index_tab.

    SPLIT iv_before AT space INTO TABLE lt_before.
    SPLIT iv_after  AT space INTO TABLE lt_after.

    lt_index = mo_differ->create_index( it_find_these = lt_before
                                        it_in_these   = lt_after ).

    rt_result = mo_differ->find_matching_blocks( it_before_tokens = lt_before
                                                 it_after_tokens  = lt_after ).

  ENDMETHOD.

  METHOD find_matching_blocks_1.

    DATA lt_matches TYPE zcl_abap_differ=>ty_matches.

    " When called with a single match, should return a match
    lt_matches = find_matching_blocks( iv_before = 'a dog bites'
                                       iv_after =  'when a dog bites it hurts' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_matches )
      exp = 1 ).

  ENDMETHOD.

  METHOD find_matching_blocks_2.

    DATA:
      ls_exp     TYPE zcl_abap_differ=>ty_match,
      ls_match   TYPE zcl_abap_differ=>ty_match,
      lt_matches TYPE zcl_abap_differ=>ty_matches.

    " When called with multiple matches, should return 3 matches
    lt_matches = find_matching_blocks( iv_before = 'the dog bit a man'
                                       iv_after =  'the large brown dog bit a tall man' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_matches )
      exp = 3 ).

    " should match "the"
    READ TABLE lt_matches INTO ls_match INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    ls_exp-start_in_before = 0.
    ls_exp-start_in_after  = 0.
    ls_exp-length          = 1.
    ls_exp-end_in_before   = 0.
    ls_exp-end_in_after    = 0.

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

    " should match "dog bit a"
    READ TABLE lt_matches INTO ls_match INDEX 2.
    cl_abap_unit_assert=>assert_subrc( ).

    ls_exp-start_in_before = 1.
    ls_exp-start_in_after  = 3.
    ls_exp-length          = 3.
    ls_exp-end_in_before   = 3.
    ls_exp-end_in_after    = 5.

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

    " should match "man"
    READ TABLE lt_matches INTO ls_match INDEX 3.
    cl_abap_unit_assert=>assert_subrc( ).

    ls_exp-start_in_before = 4.
    ls_exp-start_in_after  = 7.
    ls_exp-length          = 1.
    ls_exp-end_in_before   = 4.
    ls_exp-end_in_after    = 7.

    cl_abap_unit_assert=>assert_equals(
      act = ls_match
      exp = ls_exp ).

  ENDMETHOD.

ENDCLASS.
