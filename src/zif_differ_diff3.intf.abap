INTERFACE zif_differ_diff3
  PUBLIC.

************************************************************************
* ABAP Differ - Diff3 Interface
*
* https://github.com/Marc-Bernard-Tools/ABAP-Differ
*
* This is a port of JavaScript (https://github.com/bhousel/node-diff3, MIT license)
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************

  TYPES ty_number TYPE i.

  TYPES:
    ty_numbers TYPE STANDARD TABLE OF ty_number WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_number_number,
      key TYPE ty_number,
      len TYPE ty_number,
    END OF ty_number_number.

  TYPES:
    BEGIN OF ty_ilcsresult,
      key          TYPE i,
      buffer1index TYPE ty_number,
      buffer2index TYPE ty_number,
      chain        TYPE i, " ref to ilcsresult-key
    END OF ty_ilcsresult.
  TYPES:
    ty_ilcsresult_t TYPE SORTED TABLE OF ty_ilcsresult WITH UNIQUE KEY key.

  TYPES:
    BEGIN OF ty_icommresult,
      common TYPE string_table,
      BEGIN OF diff,
        buffer1 TYPE string_table,
        buffer2 TYPE string_table,
      END OF diff,
    END OF ty_icommresult.
  TYPES:
    ty_icommresult_t TYPE STANDARD TABLE OF ty_icommresult WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_idiffindicesresult,
      buffer1        TYPE ty_number_number,
      buffer1content TYPE string_table,
      buffer2        TYPE ty_number_number,
      buffer2content TYPE string_table,
    END OF ty_idiffindicesresult.
  TYPES:
    ty_idiffindicesresult_t TYPE STANDARD TABLE OF ty_idiffindicesresult WITH DEFAULT KEY.

  "! Expects two arrays, finds longest common sequence
  METHODS lcs
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_ilcsresult_t.

  "! We apply the LCS to build a 'comm'-style picture of the
  "! differences between buffer1 and buffer2.
  METHODS diff_comm
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_icommresult_t.

  "! We apply the LCS to give a simple representation of the
  "! offsets and lengths of mismatched chunks in the input
  "! buffers. This is used by diff3MergeRegions.
  METHODS diff_indices
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_idiffindicesresult_t.

ENDINTERFACE.
