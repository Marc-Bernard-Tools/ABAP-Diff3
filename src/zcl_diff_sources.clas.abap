CLASS zcl_diff_sources DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_delta_diff3
      IMPORTING
        !it_new         TYPE abaptxt255_tab
        !it_old         TYPE abaptxt255_tab
      RETURNING
        VALUE(rt_delta) TYPE vxabapt255_tab .
    CLASS-METHODS get_delta_sap
      IMPORTING
        !it_new         TYPE abaptxt255_tab
        !it_old         TYPE abaptxt255_tab
      RETURNING
        VALUE(rt_delta) TYPE vxabapt255_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_diff_sources IMPLEMENTATION.


  METHOD get_delta_diff3.

    DATA:
      lt_buffer1 TYPE string_table,
      lt_buffer2 TYPE string_table,
      lo_diff3   TYPE REF TO zcl_diff3,
      ls_delta   LIKE LINE OF rt_delta.

    LOOP AT it_old ASSIGNING FIELD-SYMBOL(<ls_code>).
      INSERT |{ <ls_code>-line }| INTO TABLE lt_buffer1.
    ENDLOOP.

    LOOP AT it_new ASSIGNING <ls_code>.
      INSERT |{ <ls_code>-line }| INTO TABLE lt_buffer2.
    ENDLOOP.

    CREATE OBJECT lo_diff3.

    DATA(lt_diffs) = lo_diff3->zif_diff3~diffcomm(
      buffer1 = lt_buffer1
      buffer2 = lt_buffer2 ).

    LOOP AT lt_diffs ASSIGNING FIELD-SYMBOL(<ls_diff>) WHERE common IS INITIAL.
      CLEAR ls_delta.
      IF <ls_diff>-diff-buffer1 IS NOT INITIAL AND <ls_diff>-diff-buffer2 IS NOT INITIAL.
        ls_delta-vrsflag = zif_abapgit_definitions=>c_diff-update.
      ELSEIF <ls_diff>-diff-buffer1 IS NOT INITIAL.
        ls_delta-vrsflag = zif_abapgit_definitions=>c_diff-delete.
      ELSEIF <ls_diff>-diff-buffer2 IS NOT INITIAL.
        ls_delta-vrsflag = zif_abapgit_definitions=>c_diff-insert.
      ELSE.
        ASSERT 0 = 1.
      ENDIF.
      INSERT ls_delta INTO TABLE rt_delta.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_delta_sap.

    DATA:
      lt_trdirtab_old TYPE TABLE OF trdir,
      lt_trdirtab_new TYPE TABLE OF trdir,
      lt_trdir_delta  TYPE TABLE OF xtrdir.

    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = it_old
        texttab_new  = it_new
        trdirtab_old = lt_trdirtab_old
        trdirtab_new = lt_trdirtab_new
        trdir_delta  = lt_trdir_delta
        text_delta   = rt_delta.

  ENDMETHOD.
ENDCLASS.
