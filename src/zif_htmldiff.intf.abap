INTERFACE zif_htmldiff PUBLIC.

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

ENDINTERFACE.
