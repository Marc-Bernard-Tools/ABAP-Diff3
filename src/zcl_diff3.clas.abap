CLASS zcl_diff3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_diff3.

    ALIASES diff
      FOR zif_diff3~diffcomm.
    ALIASES lcs
      FOR zif_diff3~lcs.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _reverse
      IMPORTING
        !it_data      TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.
    METHODS _reverse_diffcomm
      IMPORTING
        !it_data      TYPE zif_diff3=>icommresult_t
      RETURNING
        VALUE(result) TYPE zif_diff3=>icommresult_t.
ENDCLASS.



CLASS zcl_diff3 IMPLEMENTATION.


  METHOD zif_diff3~diffcomm.
    " We apply the LCS to build a 'comm'-style picture of the
    " differences between buffer1 and buffer2.

    DATA:
      res       LIKE LINE OF result,
      different TYPE zif_diff3=>icommresult-diff,
      common    TYPE zif_diff3=>icommresult-common.

    DATA(lcs) = zif_diff3~lcs( buffer1 = buffer1
                               buffer2 = buffer2 ).

    DATA(tail1) = lines( buffer1 ).
    DATA(tail2) = lines( buffer2 ).

    DATA(candidate) = lcs[ key = lines( lcs ) - 1 ].
    DO.
      IF candidate-chain = -1.
        EXIT.
      ENDIF.

      CLEAR different.

      tail1 = tail1 - 1.
      WHILE tail1 > candidate-buffer1index.
        INSERT buffer1[ tail1 + 1 ] INTO TABLE different-buffer1.
        tail1 = tail1 - 1.
      ENDWHILE.

      tail2 = tail2 - 1.
      WHILE tail2 > candidate-buffer2index.
        INSERT buffer2[ tail2 + 1 ] INTO TABLE different-buffer2.
        tail2 = tail2 - 1.
      ENDWHILE.

      IF lines( different-buffer1 ) > 0 OR lines( different-buffer2 ) > 0.
        IF lines( common ) > 0.
          CLEAR res.
          res-common = _reverse( common ).
          INSERT res INTO TABLE result.
          CLEAR common.
        ENDIF.

        CLEAR res.
        res-diff-buffer1 = _reverse( different-buffer1 ).
        res-diff-buffer2 = _reverse( different-buffer2 ).
        INSERT res INTO TABLE result.
      ENDIF.

      IF tail1 >= 0.
        INSERT buffer1[ tail1 + 1 ] INTO TABLE common.
      ENDIF.

      candidate = lcs[ key = candidate-chain ].
    ENDDO.

    IF lines( common ) > 0.
      CLEAR res.
      res-common = _reverse( common ).
      INSERT res INTO TABLE result.
    ENDIF.

    result = _reverse_diffcomm( result ).

  ENDMETHOD.


  METHOD zif_diff3~lcs.
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
        values TYPE zif_diff3=>numbers,
      END OF ty_equivalenceclass.

    DATA:
      equivalenceclass   TYPE ty_equivalenceclass,
      equivalenceclasses TYPE HASHED TABLE OF ty_equivalenceclass WITH UNIQUE KEY key,
      nullresult         TYPE zif_diff3=>ilcsresult,
      candidates         TYPE zif_diff3=>ilcsresult_t,
      newcandidate       TYPE zif_diff3=>ilcsresult.

    DATA(j) = 0.
    LOOP AT buffer2 ASSIGNING FIELD-SYMBOL(<buffer2>).
      IF NOT line_exists( equivalenceclasses[ key = <buffer2> ] ).
        equivalenceclass-key = <buffer2>.
        INSERT equivalenceclass INTO TABLE equivalenceclasses ASSIGNING FIELD-SYMBOL(<equivalentclass>).
      ENDIF.
      INSERT j INTO TABLE <equivalentclass>-values.
      j = j + 1.
    ENDLOOP.

    nullresult-key          = 0.
    nullresult-buffer1index = -1.
    nullresult-buffer2index = -1.
    nullresult-chain        = -1.
    INSERT nullresult INTO TABLE candidates.

    DATA(i) = 0.
    LOOP AT buffer1 ASSIGNING FIELD-SYMBOL(<buffer1>).
      IF line_exists( equivalenceclasses[ key = <buffer1> ] ).
        DATA(buffer2indices) = equivalenceclasses[ key = <buffer1> ]-values.
      ELSE.
        CLEAR buffer2indices.
      ENDIF.

      DATA(r) = 0.
      DATA(c) = candidates[ key = 0 ].
      LOOP AT buffer2indices INTO j.

        DATA(s) = r.
        DO.
          IF s < lines( candidates ).
            IF candidates[ key = s ]-buffer2index < j AND
              ( s = lines( candidates ) - 1 OR candidates[ key = s + 1 ]-buffer2index > j ).
              EXIT.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
          s = s + 1.
        ENDDO.

        IF s < lines( candidates ).
          newcandidate-buffer1index = i.
          newcandidate-buffer2index = j.
          newcandidate-chain        = s.

          IF r = lines( candidates ).
            c-key = lines( candidates ) + 1.
            INSERT c INTO TABLE candidates.
          ELSE.
            candidates[ key = r ]-buffer1index = c-buffer1index.
            candidates[ key = r ]-buffer2index = c-buffer2index.
            candidates[ key = r ]-chain        = c-chain.
          ENDIF.

          r = s + 1.
          c = newcandidate.

          IF r = lines( candidates ).
            EXIT. " no point in examining further (j)s
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF line_exists( candidates[ key = r ] ).
        candidates[ key = r ]-buffer1index = c-buffer1index.
        candidates[ key = r ]-buffer2index = c-buffer2index.
        candidates[ key = r ]-chain        = c-chain.
      ELSE.
        c-key = r.
        INSERT c INTO TABLE candidates.
      ENDIF.

      i = i + 1.
    ENDLOOP.

    " At this point, we know the LCS: it's in the reverse of the
    " linked-list through chain of candidates[ lines( candidates ) - 1 ].

    result = candidates.

  ENDMETHOD.


  METHOD _reverse.

    DATA(lv_line) = lines( it_data ).

    DO lines( it_data ) TIMES.
      INSERT it_data[ lv_line ] INTO TABLE result.
      lv_line = lv_line - 1.
    ENDDO.

  ENDMETHOD.


  METHOD _reverse_diffcomm.

    DATA(lv_line) = lines( it_data ).

    DO lines( it_data ) TIMES.
      INSERT it_data[ lv_line ] INTO TABLE result.
      lv_line = lv_line - 1.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
