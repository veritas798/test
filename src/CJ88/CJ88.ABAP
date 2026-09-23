FORM POSTING_PROCESS USING PV_IDX CHANGING PV_ERR.
    DATA LV_ERR.

    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX PV_IDX.
    IF SY-SUBRC EQ 0.
        CLEAR LV_ERR.
        PERFORM POSTING_FUNC USING ABAP_FALSE CHANGING <LS_DISP> LV_ERR.
        IF LV_ERR IS INITIAL.
            <LS_DISP>-EXEC = 'P' "POSTING
            PERFORM GET_BELNR CHANGING <LS_DISP>.
            APPEND <LS_DISP> TO GT_SAVE.
        ENDIF.
    ELSE.
        PV_ERR = LV_ERR.
    ENDIF.
ENDFORM.

FORM POSTING_FUNC USING PV_REV CHANGING PS_DISP TYPE TS_DISP
                                        PV_ERR.
    DATA : LS_AUAK TYPE AUAK,
           LS_CONTROL TYPE KABR_CONTROL,
           LT_OBJNR LIKE TABLE OF JSTO_PRE,
           LT_TRACT TYPE KABRT_OBJTAB_TABLE,
           LT_SERYPTAB TYPE KABRT_SETYP_TABLE,
           LS_LST TYPE KABR_LST,
           LT_OBJTAB TYPE KABRT_OBJTAB_TABLE,
           LT_MESG TYPE TSMESG,
           LT_MESGL TYPE TSMESGL,

           LT_SET TYPE KABRT_AUFSEL_TABLE,
           LT_LATE_OBJ TYPE KABRT_OBJTAB_TABLE,
           LT_ADD_SDR TYPE KABRT_OBJTAB_TABLE,
           LT_BEL_ALL TYPE KABRT_BEL_ALL_TABLE,
           LT_MATNR TYPE KABTR_COSE_SEL_TABLE,
           LT_WSUM TYPE KABRT_WSUM_TABLE,
           LT_GSUM TYPE KABRT_GSUM_TABLE.
    IF PV_REV IS INITIAL.
        PERFORM SET_AUAK CHANGING LS_AUAK.
        PERFORM SET_CONTROL USING PV_REV CHANGING LS_CONTROL.
    ELSE.
        PERFORM SET_AUAK_REV CHANGING LS_AUAK.
        PERFORM SET_CONTROL_REV USING PV_REV CHANGING LS_CONTROL.
    ENDIF.

    LT_SETYPTAB = VALUE #( ( SETYP = 'PR' ) ).
    LT_OBJTAB = VALUE #( ( OBJNR = PS_DISP-OBJNR ) ).
    
    CALL FUNCTION 'MESSAGE_INITIALIZE'
        EXPORTING
            I_IDENTIFICATION = LS_CONTROL-MESS_IDENTIF
            CHECK_ON_COMMIT = ' '
        IMPORTING
            E_IDENTIFICATION = LS_CONTROL-MESS_IDENTIF.

    LS_CONTFOL-ZEILE = '1'.

    TRY.
        CALL FUNCTION 'K_SETTLEMENT_GROUP_PROCESS'
            EXPORTING
                I_AUAK = LS_AUAK
                I_CONTROL = LS_CONTROL
                IT_REM_OBJ = LT_OBJNR
                IT_TRACE_OBJ = LT_TRACE
                IT_SETYPTAB = LT_SETYPTAB
            IMPORTING
                E_LST = LS_LST
            TABLES
                IT_OBJTAB = LT_OBJTAB
            CHANGING
                CT_SET = LT_SET
                CT_LATE_OBJ = LT_LATE_OBJ
                CT_ADD_SDR = LT_ADD_SDR
                CT_BEL_ALL = LT_BEL_ALL
                CT_MATNR = LT_MATNR
                CT_WSUM = LT_WSUM
                CT_GSUM = LT_GSUM
            EXCEPTIONS
                OTHERS = 9.
    CATCH CX_ROOT INTO DATA(LR_ROOT).
        PV_ERR = ABAP_TRUE.
        EXIT.
    ENDTRY.

    CLEAR LS_LST-ZEILE.

    CALL FUNCTION 'MESSAGE_GIVE'
        EXPORTING
            I_ZEILE = LS_LST-ZEILE
            I_INCL_TITLE = ABAP_TRUE
        TABLES
            T_MESG = LT_MESG
            T_MESGL = LT_MESGL.
    

    LOOP AT LT_MESGL ASSIGNING FIELD-SYMBOL(<LS_MESGL>).
        IF <LS_MESGL>-MSGTY EQ 'E'.
            
            MESSAGE <LS_MESGL>-TEXT TYPE 'S' DISPLAY LIKE 'E'.
            PV_ERR = ABAP_TRUE.
        ENDIF.
    ENDLOOP.
ENDFORM.

FORM SET_AUAK CHANGING PS_AUAK TYPE AUAK.
    PS_AUAK = VALUE #( KOKRS = PA_KOKRS
                       BUDAT = GV_EDATE
                       BZDAT = '00000000'
                       CPUDT = SY-DATLO
                       WSDAT = GV_EDATE
                       KURST = 'M'
                       GJAHR = SO_SPMON-LOW+0(4)
                       PERIO = |0{ SO_SPMON-LOW+4(2) }| 
                       SAPRL = SY-SAPRL
                       AWSYS = GS_T000-LOGSYS
                       CO_VAART = '1'
                       BUPERIO = |0{ SO_SPMON-LOW+4(2) }| 
                       BUDJAHR = SO_SPMON-LOW+0(4)
                       VERSN = PA_VERSN
                       WRTTP = '04'  
                       ).
ENDFORM.

FORM SET_CONTROL USING PV_REV CHANGING PS_CONTROL LIKE KABR_CONTROL.
    




        


