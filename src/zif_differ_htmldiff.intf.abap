INTERFACE zif_differ_htmldiff
  PUBLIC.

************************************************************************
* ABAP Differ - HTML Diff Interface
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
