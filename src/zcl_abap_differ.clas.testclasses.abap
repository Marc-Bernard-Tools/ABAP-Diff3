CLASS lct_helper DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS format
      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS diff
      IMPORTING iv_before        TYPE string
                iv_after         TYPE string
                iv_with_img      TYPE abap_bool DEFAULT abap_false
                iv_chinese       TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lct_helper IMPLEMENTATION.

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

CLASS lct_differ_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  " Tests from https://github.com/alaorneto/htmldiffer
  PRIVATE SECTION.

    METHODS setup.
    METHODS test_ignore_image FOR TESTING.
    METHODS test_with_image FOR TESTING.

    DATA:
      mv_original TYPE string,
      mv_modified TYPE string,
      mo_differ   TYPE REF TO zcl_abap_differ.

ENDCLASS.

CLASS lct_differ_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_differ.

    mv_original = lct_helper=>format( '\n'
      && '    <p>First paragraph.</p>\n'
      && '    <ul>\n'
      && '        <li>Item A</li>\n'
      && '        <li>Item B</li>\n'
      && '        <li>Item C</li>\n'
      && '    </ul>\n'
      && '    <img src="previous.jpg">\n'
      && '    <span>This is some interesting text.</span>\n' ).

    mv_modified = lct_helper=>format( '\n'
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

    lv_exp = lct_helper=>format( '\n'
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

    lv_exp = lct_helper=>format( '\n'
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

CLASS lct_differ_test_2 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  " Tests from https://github.com/myobie/htmldiff
  PRIVATE SECTION.

    METHODS diff_text FOR TESTING.
    METHODS insert_a_letter_and_a_space FOR TESTING.
    METHODS remove_a_letter_and_a_space FOR TESTING.
    METHODS change_a_letter FOR TESTING.
    METHODS support_chinese FOR TESTING.
    METHODS support_img_tags_insertion FOR TESTING.
    METHODS support_img_tags_deletion FOR TESTING.

ENDCLASS.

CLASS lct_differ_test_2 IMPLEMENTATION.

  METHOD diff_text.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = lct_helper=>diff(
      iv_before = 'a word is here'
      iv_after  = 'a nother word is there' ).

    lv_exp = 'a<ins class="diffins"> nother</ins> word is <del class="diffmod">here</del><ins class="diffmod">there</ins>'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD insert_a_letter_and_a_space.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = lct_helper=>diff(
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

    lv_act = lct_helper=>diff(
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

    lv_act = lct_helper=>diff(
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

    lv_act = lct_helper=>diff(
      iv_before  = '这个是中文内容, Ruby is the bast'
      iv_after   = '这是中国语内容,Ruby is the best language.'
      iv_chinese = abap_true ).

    lv_exp = '这<del class=\"diffdel\">个</del>是中<del class=\"diffmod\">文</del><ins class=\"diffmod\">'
          && '国语</ins>内<del class=\"diffmod\">容, Ruby</del><ins class=\"diffmod\">容，Ruby'
          && '</ins> is the <del class=\"diffmod\">bast</del><ins class=\"diffmod\">best language.</ins>'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).
  ENDMETHOD.

  METHOD support_img_tags_insertion.

    DATA: lv_act TYPE string,
          lv_exp TYPE string.

    lv_act = lct_helper=>diff(
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

    lv_act = lct_helper=>diff(
      iv_before = 'a b <img src="some_url" /> c'
      iv_after  = 'a b c'
      iv_with_img = abap_true ).

    lv_exp = 'a b <del class="diffdel"><img src="some_url" /> </del>c'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).
  ENDMETHOD.

ENDCLASS.
