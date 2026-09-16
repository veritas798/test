CLASS LCL_HANDLER_MANA DEFINITION
    PUBLIC SECTION.
        METHODS ADDED_FUNCTION FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
            IMPORTING E_SALV_FUNCTION.
ENDCLASS.

CLASS LCL_HANDLER_MANA IMPLEMENTATION.
    METHOD ADDED_FUNCTION.

        DATA : LRF_SELECTIONS TYPE REF TO CL_SLAV_SELECTIONS,
               LT_SELECTED_ROWS TYPE SALV_T_ROW,
               LT_SCR_MANAGE TYPE TABLE OF TKESS_YS_SCR_MANAGE,
               LS_MANA TYPE TKESS_YS_SCR_MANAGE,
               LV_ANSWER.

        CASE E_SALV_FUNCTION.
            WHEN 'CANC'.
                GRF_MANAGE_ALV->CLOSE_SCREEN(  ).
                FREE GRF_MANAGE_ALV.
            WHEN 'DELETE'.
                CLEAR : LT_SELECTED_ROWS, LT_SCR_MANAGER.

                LRF_SELECTIONS = GRF_MANAGE_ALV->GET_SELECTIONS(  ).
                LT_SELECTED_ROWS = LRF_SELECTIONS->GET_SELECTED_ROWS( ).            

                LOOP AT LT_SELECTED_ROWS INTO DATA(LS_SELECTED_ROWS).
                    READ TABLE GT_SCR_MANAGE INTO DATA(LS_SCR_MANAGE) INDEX LS_SELECTED_ROWS.
                    IF SY-SUBRC = 0.
                        LS_SCR_MANAGE-SCRDELE = 'X'.
                        APPEND LS_SCR_MANAGE TO LT_SCR_MANAGE.
                    ENDIF.
                ENDLOOP.

                IF LT_SCR_MANAGE IS NOT INITIAL.
                    CLEAR LV_ANSWER.

                    CALL FUNCTION 'POPUP_TO_CONFIRM'
                        EXPORTING
                            DEFAULT_BUTTON = '2'
                            DISPLAY_CANCEL_BUTTON = SPACE
                            TITLEBAR = 'DELETION'
                            TEXT_QUESTION = 'DO YOU WANT TO DELETE SELECTE SCREEN VARIANTS?'
                            POPUP_TYPE = 'ICON_MESSAGE_WARNING'
                        IMPORTING
                            ANSWER = LV_ANSWER.
                    
                        IF LV_ANSWER = '1'.
                            READ TABLE LT_SCR_MANAGE INTO LS_SCR_MANAGE INDEX 1.

                            SORT GT_SCR_MANAGE BY SCRNAME.
                            READ TABLE GT_SCR_MANAGE INTO DATA(LS_TEMP) WITH KEY SCRNAME = LS_SCR_MANAGE-SCRNAME BINARY SEARCH.
                            IF SY-SUBRC EQ 0.
                                DATA(LV_TABIX) = SY-TABIX.

                                IF GV_SCRNAME = LS_TEMP-SCRNAME.
                                    CLEAR GV_SCRNAME.
                                ENDIF.

                                PERFORM SECRRN_DELETE USING LS_TEMP-SCRNAME 'X'.

                                LS_TEMP-SCRDELE = 'X'.
                                MODIFY GT_SCR_MANAGE FROM FROM LS_TEMP INDEX LV_TABIX TRANSPORTING SCRDELE.
                            ENDIF.

                            DELETE GT_SCR_MANAGE FROM 1 WHERE SCRDELE EX 'X'.
                            IF SY-SUBRC EQ 0.
                                GV_LIST_UPDATE = 'X'.
                            ENDIF.

                            GRF_MANAGE_ALV->REFRESH( ).
                        ENDIF.
                ENDIF.
            ENDCASE.
    ENDMETHOD.
ENDCLASS.


