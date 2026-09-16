* 타 SAP 서버에서 SAP 서버로 COPA 데이터를 마이그레이션 한다. 
* 인덱스 gjahr, paobjnr을 사용하며 병렬로 처리하고 실패 관리를 해서 재 실행시에 전체 데이터를 가져오지 않아도 된다.

selection-screen BEGIN of block with title frame title text-001.
    parameters : pa_rfcdt type rfcdest obligatory,
                 pa_stab type tabname obligatory,
                 pa_ttba type tabname obligatory.
    select-options : so_perio for gv_jahrper obligatory.
selection-screen end of block b1.

selection-screen BEGIN of block with title frame title text-002.
    parameters : pa_pomax type numc10 obligatory default '',
                 pa_p_max type i obligatory default 10,
                 pr_rows type i obligatory.
selection-screen end of block b2.
parameters pa_clear as checkbox.
parameters pa_trcat as checkbox.

constants : lc_max_retry type i value '3',
            lc_batch_size type i value 100000,
            lc_paobj_min type numc10 value '0000000001'.

types : BEGIN of ty_job_queue,
            perio type jahrper,
            paobj_low type numc10,
            paobj_high type numc10,
            skips type i,
            status type char1,
            taskname type char20,
            retry_cnt type i,
        end of ty_job_queue.

data : gt_queue type table of ty_job_queue with non-unique sorted key sk1 components status,
       gv_act_tasks type i value 0,
       gv_task_idx type i value 0.

data : gv_gahrper type jahrper,
       gv_error,
       gv_retry,
       gv_exit.

data : go_timer type ref to if_abap_runtime,
       gv_start_time type int8.

*-------------------------------------------------------------------------------    

START-OF-SELECTION.
    PERFORM INIT.
    PERFORM LOAD_AND BUILD_QUEUE.
    PERFORM EXECUTE_PARALLEL_PROCESSING.

*-------------------------------------------------------------------------------    
FORM INIT.
    CHECK pa_clear IS NOT INITIAL.

    SELECT SINGLE pa_stab
    FROM YTAB_COPA_LOG
    WHERE STAB EQ @pa_stab 
      AND TTAB EQ @pa_TTAB 
      AND PERIO IN @so_perio
      INTO @DATA(LV_STAB).
    IF SY-SUBRC EQ 0.
        DELETE FROM YTAB_COPA_LOG
        WHERE STAB EQ @pa_stab
          AND TTAB EQ @pa_TTAB
          AND PERIO IN @SO_PERIO.
        IF PA_TRCAT EQ ABAP_TRUE.
            PERFORM TRUNCATE_TABLE.
        ELSE.
            DELETE FROM (PA_TTAB) WHERE PERIO IN @SO_PERIO.
            COMMIT WORK AND WAIT.
        ENDIF.
    ENDIF.

ENDFORM.    

FORM TRUNCATE_TABLE.
    DATA : LO_SQL TYPE REF TO CL_sQL_STATEMENT,
           LV_SQL TYPE STRING.
    TRY.
        CREATE OBJECT LO_SQL.
        LV_SQL = |TRUNCATE TABLE { PA_TTAB }|.
        LO_SQL->EXECUTE_DDL( LV_SQL ).

        MESSAGE 'SUCCESSFULLY TRUNCATED' TYPE 'S'.
    CATCH CX_SQL_EXCEPTION INTO DATA(LR_ERR).
        DATA(LV_MSG) = LR_ERR->GET_TEXT( ).
        MESSAGE LV_MSG TYPE 'E'.
        GV_ERROR = ABAP_TRUE.
        STOP.
    ENDTRY.
ENDFORM.

FORM LOAD_AND_BUILD_QUEUE.
    DATA : LT_TARGET_PERIOS TYPE TABLE OF JAHRPER,
           LT_DB_LOG TYPE TABLE OF YTAB_COPA_LOG,
           LT_DEL_DB TYPE TABLE OF YTAB_COPA_LOG,
           LT_RE_DB  TYPE TABLE OF YTAB_COPA_LOG.
    DATA LV_CURR_HGH TYPE NUMC10.
    DATA LV_MSG TYPE CHAR50.

    SELECT * FROM YTAB_COPA_LOG
    INTO TABLE @LT_DB_LOG
    WHERE STAB = @pa_stab
      AND TTAB = @PA_TTAB
      AND PERIO IN @SO_PERIO
      ORDER BY STAB, TTAB, PERIO, PAOBJ_LOW, PAOBJ_HIGH.

    IF SY-SUBRC EQ 0 AND LT_DB_LOG IS NOT INITIAL.
        " RESUME MODE
        LOOP AT LT_DB_LOG ASSIGNING FIELD-SYMBOF(<LS_DB>).
            IF <LS_DB>-STATUS = 'C'.
                CONTINUE.
            ENDIF.

            DATA(LV_CURR_LOW) = <LS_DB>-PAOBJ_LOW.
            DATA(LV_CURR_HIGH) = <LS_DB>-PAOBJ_HIGH.
            " 재작업 대상 상태 = 'E' 기존 DB 데이터만 집계
            " 재작업이 실행되면 기존 상태 = 'E' 데이터는 삭제하고 재작업 로그를 새로 생성.
            " 재작업 PA_ROWS를 처음 작업보다 크게 할 수는 없음.
            APPEND <LS_DB> TO LT_DEL_DB.

            WHILE LV_CURR_LOW <= LV_MAX_HIGH.
                LV_CURR_HGH = LV_CURR_LOW + PA_ROWS - 1.
                IF LV_CURR_HGH > LV_MAX_HIGH.
                    LV_CURR_HGH = LV_MAX_HIGH.
                ENDIF.
                " 실행중(R) 이거나 에러(E) 대기( ) 상태였던 중단점 구간들만 타켓 큐에 재등록
                APPEND VALUE #( PAOBJ_LOW = LV_CURR_LOW
                                PA0BJ_HIGH = LV_CURR_HGH
                                PERIO = <LS_DB>-PERIO
                                SKIPS = <LS_DB>-SKIPS
                                STATUS = ' ' " 상태 초기화하여 다시 대기 세션으로 재배정
                                RETRY_CNT = <LS_DB>-RETRY_CNT ) TO GT_QUEUE.
                " 재작업 로그 데이터 신규로 생성.
                APPEND VALUE #( STAB = pa_stab
                                TTAB = PA_TTAB
                                PERIO = <LS_DB>-PERIO
                                PAOBJ_LOW = LV_CURR_LOW
                                PAOBJ_HIGH = LV_CURR_HGH
                                SKIPS = <LS_DB>-SKIPS
                                STATUS = ' '
                                RETRY_CNT = <LS_DB>-RETRY_CNT + 1
                                
                                ERNAM = <LS_DB>-ERNAM
                                ERDAT = <LS_DB>-ERDAT
                                ERZET = <LS_DB>-ERZET
                                
                                AENAM = SY-UNAME
                                AEDAT = SY-DATUM
                                AEZET = SY_UZEIT
                                ) TO LT_RE_DB.

                LV_CURR_LOW = LV_CURR_HGH + 1.
            ENDWHILE.
                
        ENDLOOP.

        IF GT_QUEUE IS INTIAL.
            MESSAGE '[작업완료] 모든 테스크가 완료되어 잔여 테스크가 존재하지 않아 종료 합니다.' TYPE 'S'.
            STOP.
        ELSE.
            GV_RETRY = ABAP_TRUE.

            IF LT_DEL_DB IS NOT INITIAL.
                DELETE YTAB_COPA_LOG FROM TABLE @LT_DEL_DB.
            ENDIF.

            IF LT_RE_DB IS NOT INITIAL.
                INSERT YTAB_COPA_LOG FROM TABLE @LT_RE_DB.
                COMMIT WORK.
            ENDIF.

            MESSAGE '> [복구 완료] 성공 구간 건넘띔 완ㄹ료. 복구된 잔여 테스트 수 : { LINES( GT_QUEUE ) }개' TYPE 'S'
            RETURN.
        ENDIF.
    ENDIF.

    "[ 최초 실행모드 ] DB 기록이 없으므로 타겟 큐를 신규 빌드
    MESSAGE '>[ NEW MODE ] 최초 이관을 감지 했습니다. 타겟 큐 빌드 시작' TYPE 'S'.

    DATA(LV_LOW_YEAR) = SO_PERIO-LOW(4).
    DATA(LV_HIGH_YEAR) = COND #( WHEN SO_PERIO-HIGH IS INITIAL THEN SO_PERIO-LOW(4)
                                 ELSE SO_PERIO-HIGH(4) ).
    DATA(LV_Y) = LV_LOW_YEAR.
    WHILE LV_Y <= LV_HIGH_YEAR.
        DO 12 TIMES.
            DATA(LV_M) = |{ SY-INDEX WIDTH = 3 ALIGH = RIGHT PAD = '0' }|.
            DATA(LV_EVAL_JAHRPER) = CONV JAHRPER( |{ LV_Y }{ LV_M }| ).

            IF LV_EVAL_JAHRPER IN SO_PERIO.
                APPEND LV_EVAL_JAHRPER TO LT_TARGET_PERIOS.
            ENDIF.
        ENDDO.
        LV_Y = LV_Y + 1.
    ENDWHILE.

    SORT LT_TARGET_PERIOS.
    DELETE ADJACENT DUPLICATES FROM LT_TARGET_PERIOS.

    IF LT_TARGET_PERIOS IS INITIAL.
        MESSAGE '이관 대상 기간이 존재하지 않습니다.'  TYPE 'S'.
        STOP.
    ENDIF.

    " 초기 큐 생성.
    DATA LT_INIT_DB TYPE TABLE OF YTAB_COPA_LOG.

    LOOP AT LT_TARGET_PERIOS ASSIGNING FIELD-SYMBOL(<LV_P>).
        LV_CURR_LOW = lc_paobj_min.
        LV_MAX_HIGH = pa_pomax.

        WHILE LV_CURR_LOW <= LV_MAX_HIGH.
            LV_CURR_HGH = LV_CURR_LOW _ PA_ROWS - 1.
            IF LV_CURR_HGH > LV_MAX_HIGH.
                LV_CURR_HGH = LV_MAX_HIGH.
            ENDIF.

            " 1 내부 제어용 메모리 큐
            APPEND VALUE #( PAOBJ_LOW = LV_CURR_LOW
                            PAOBJ_HIGH = LV_CURR_HGH
                            PERIO = <LV_P>
                            SKIPS = 0
                            STATUS = ' '
                            RETRY_CNT = 0 ) TO GT_QUEUE.
            " 2 보존용 초기 로그
            APPEND VALUE #( STAB = pa_stab
                            TTAB = PA_TTAB
                                PERIO = <LV_P>
                                PAOBJ_LOW = LV_CURR_LOW
                                PAOBJ_HIGH = LV_CURR_HGH
                                SKIPS = 0
                                STATUS = ' '
                                RETRY_CNT = 0
                                
                                ERNAM = SY-UNAME
                                ERDAT = SY-DATUM
                                ERZET = SY_UZEIT
                                
                                AENAM = SY-UNAME
                                AEDAT = SY-DATUM
                                AEZET = SY_UZEIT
                                ) TO LT_INIT_DB.
            LV_CURR_LOW = LV_CURR_HGH + 1.
        ENDWHILE.
    ENDLOOP.

    "로그 CBO 테이블에 반영
    IF LT_INIT_DB IS NOT INITIAL.
        INSERT YTAB_COPA_LOG FROM TABLE @LT_INIT_DB.
        COMMIT WORK.
    ENDIF.

    MESSAGE 'STEP 2: 초기 타겟 큐 빌드 및 CBO 로그 연동 완료. 총 테스트 개수 : { LINES( GT_QUEUE ) } 개' TYPES 'S'.
ENDFORM.

FORM EXECUTE_PARALLEL_PROCESSING.
    MESSAGE 'STEP3 : 병렬 프로세스 스케줄러 가동' TYPE 'S'.

    WHILE GV_ACT_TASKS > 0 OR LINE_EXISTS( GT_QUEUE[ KEY SK1 components STATUS = ' ' ] ).
        IF GV_ACT_TASKS < pa_p_max.
            READ TABLE GT_QUEUE TRANSPORTING NO FIELDS WITH KEY SK1 components STATUS = ' '.
            IF SY-SUBRC EQ 0.
                DATA(LV_TABIX) = SY-TABIX.

                GV_TASK_IDX = GV_TASK_IDX + 1.
                DATA(LV_TASK_NAME) = CONV CHAR20( |TASK_{ GV_TASK_IDX }| ).

                READ GT_QUEUE ASSIGNING FIELD-SYMBOL(<FS_QUEUE>) INDEX LV_TABIX USING KEY SK1.
                <FS_QUEUE>-STATUS = 'R'.
                <FS_QUEUE>-TASKNAME = LV_TASK_NAME.
                GV_ACT_TASKS = GV_ACT_TASKS + 1.

                " 프로세스가 실행을 시작하는 순간 상태를 'R'로 업데이트
                UPDATE YTAB_COPA_LOG
                   SET STATUS = 'R'
                       TASKNAME = @LV_TASK_NAME,
                       AENAM = @SY-UNAME,
                       AEDAT = @SY-DATUM,
                       AEZET = @SY-UZEIT
                WHERE STAB = @pa_stab
                  AND TTAB = @PA_TTAB
                  AND PERIO = @<FS_QUEUE>-PERIO
                  AND PAOBJ_LOW = @<FS_QUEUE>-PAOBJ_LOW
                  AND PAOBJ_HIGH = @<FS_QUEUE>-PAOBJ_HIGH
                  AND SKIPS = @<FS_QUEUE>-SKIPS.

                PERFORM CALL_BODS_RFC_ASYNC USING LV_TASK_NAME
                                                  <FS_QUEUE>-PAOBJ_LOW
                                                  <FS_QUEUE>-PA0BJ_HIGH
                                                  <FS_QUEUE>-PERIO
                                                  <FS_QUEUE>-SKIPS
                                                  LV_TABIX.
            ENDIF.
        ENDIF.

        IF GV_ACT_TASKS >= pa_p_max OR NOT LINE_EXISTS( GT_QUEUE[ KEY SK1 components STATUS = ' ' ] ).
            COMMIT WORK.
            WAIT UP TO 1 SECONDS.
        ENDIF.
    ENDWHILE.

    PERFORM PRINT_SUMMARY_REPORT.
ENDFORM.

FORM CALL_BODS_RFC_ASYNC USING PV_TASK TYPE CHAR20  
                               PV_PLOW TYPE numc10
                               PV_PHIGH TYPE NUMC10
                               PV_PERIO TYPE JAHRPER
                               PV_SKIPS TYPE i
                               PV_Q_IDX TABIX I.

DATA : LT_OPTIONS TYPE TABLE OF RFC_DB_OPT,
       LT_FILEDS TYPE TABLE OF RFC_DB_FLD,
       LV_MSG TYPE CHAR255,
       LV_SYS_MSG TYPE STRING.

PERFORM SET_FIELDS TABLES LT_FIELDS.

APPEND VALUE #( TEXT = |PAOBJNR BETWEEN '{ PV_PLOW }' AND '{ OV/0PHIGH }'| ) TO LT_OPTIONS.
APPEND VALUE #( TEXT = |AND PERIO = '{ PV_PERIO }'| ) TO LT_OPTIONS.

CALL FUNCTION '/BODS/RFC_READ_TABLE2'
    DESTINATION pa_rfcdt
    STARTING NEW TASK PV_TASK
    PERFORMING CALLBACK_RFC_RESULT ON END OF TASK
    EXPORTING
        QUERY_TABLE = pa_stab
        DELIMITER = '|'
        NO_DATA = ' '
        ROWSKIPS = 0
        WORCOUNT = 0
    TABLES
        OPTIONS = LT_OPRIONS
        FIELDS = LT_FIELDS
    EXCEPTIONS
        ....
        OTHERS = 9.
    
    IF SY-SUBRC <> 0.
        IF SY-MSGID IS NOT INITIAL.
            MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
        ENDIF.

        READ TABLE GT_QUEUE ASSIGNING FIELD-SYMBOL(<FS_Q>) INDEX PV_Q_IDX.
        <FS_Q>-STATUS = ' '.
        GV_ACT_TASKS = GV_ACT_TASKS - 1.

        " 호출 단계 실패 시에도 DB 자원 원복 마킹
        UPDATE YTAB_COPA_LOG
        SET STATUS = ' '
            AENAM = @SY-UNAME,
            AEDAT = @SY-DATUM,
            AEZET = @SY-UZEIT
        WHERE STAB = @pa_stab
          AND TTAB = @PA_TTAB
          AND PERIO = @<FS_Q>-PERIO
          AND PAOBJ_LOW = @<FS_Q>-PAOBJ_LOW
          AND PAOBJ_HIGH = @<FS_Q>-PAOBJ_HIGH
          AND SKIPS = @<FS_Q>-SKIPS.            
    ENDIF.
ENDFORM.

FORM CALLBACK_RFC_RESULT USING PV_TASK TYPE CLIKE.
    DATA : LT_OUT128 TYPE TABLE OF /BODS/TAB128,
           LT_OUT512 TYPE TABLE OF /BODS/TAB512,
           LT_OUT2048 TYPE TABLE OF /BODS/TAB2048,
           LT_OUT8192 TYPE TABLE OF /BODS/TAB8192,
           LT_OUT30000 TYPE TABLE OF /BODS/TAB30000,
           LV_OUT_TAB TYPE DD021-TABNAME,
           LV_MSG TYPE CHAR255,
           LV_LMSG TYPE CHAR50,
           LV_SYS_MSG TYPE STRING.
    FIELD-SYMBOLS : <LT_ACTIVE_BUFFER> TYPE TABLE.

    REFRESH : LT_OUT128, LT_OUT512, LT_OUT2048, LT_OUT30000.
    CLEAR : LV_OUT_TAB, LV_SYS_MSG.

    RECEIVE RESULTS FROM FUNCTION '/BODS/RFC_READ_TABLE2'
    IMPORTING
        OUT_TABLE = LV_OUT_TAB
    TABLES
        TBLOUT128 = LT_OUT128
        TBLOUT512 = LT_OUT512
        TBLOUT2048 = LT_OUT2048
        TBLOUT8192 = LT_OUT8192
        TBLOUT30000 = LT_OUT30000
    EXCEPTIONS
        ...
        OTHERS = 9.

    DATA(LV_RFC_SUBRC) = SY-SUBRC.
    READ TABLE GT_QUEUE WITH KEY TASKNAME = PV_TASK ASSIGNING FIELD-SYMBOF(<FS_1>).
    IF SY-SUBRC EQ 0.
        IF LV_RFC_SUBRC = 0 AND LV_OUT_TAB IS NOT INITIAL.
            DATA(LV_BIND_TARGET) = |LT_{ LV_OUT_TAB+3 }|.
            TRANSLATE LV_BIND_TARGET TO UPPER CASE.
            ASSIGN (LV_BIND_TARGET) TO <LT_ACTIVE_BUFFER>.
            IF SY-SUBRC = 0 AND <LT_ACTIVE_BUFFER> IS ASSIGNED.
                DATA(LV_RECEIVED_LINES) = LINES( <LT_ACTIVE_BUFFER> ).
                " 데이터가 존재 할 때만 저장 및 로그 출력
                IF LV_RECEIVED_LINES > 0.
                    MESSAGE |> [{ PV_TASK }] 데이터 패킷 { LV_RECEIVED_LINES } 건 수신 완료. 객체: { <FS_Q>-PAOBJ_LOW } ~ { <FS_Q>-PAOBJ_HIGH }| TYPE 'S'.
                    PERFORM SAVE_TO_HANA_DB TABLES <LT_ACTIVE_BUFFER> USING PA_TTAB PV_TASK.
                ENDIF.

                "PA_ROWS 건 미만으로 깔끔하게 완료된 경우 최종 마감 동기화
                <FS_Q>-STATUS = 'C'.
                UPDATE YTAB_COPA_LOG
                SET STATUS = 'C'
                    AENAM = @SY-UNAME,
                    AEDAT = @SY-DATUM,
                    AEZET = @SY-UZEIT
                WHERE STAB = @pa_stab
                  AND TTAB = @PA_TTAB
                  AND PERIO = @<FS_Q>-PERIO
                  AND PAOBJ_LOW = @<FS_Q>-PAOBJ_LOW
                  AND PAOBJ_HIGH = @<FS_Q>-PAOBJ_HIGH
                  AND SKIPS = @<FS_Q>-SKIPS.            
            ELSE.
                MESSAGE |X [{ PV_TASK }] 결과 테이블 동적 매핑 실패! ({ LV_BIND_TARGET })|.
                <FS_Q>-STATUS = 'E'.
                UPDATE YTAB_COPA_LOG
                SET STATUS = 'E'
                    AENAM = @SY-UNAME,
                    AEDAT = @SY-DATUM,
                    AEZET = @SY-UZEIT
                WHERE STAB = @pa_stab
                  AND TTAB = @PA_TTAB
                  AND PERIO = @<FS_Q>-PERIO
                  AND PAOBJ_LOW = @<FS_Q>-PAOBJ_LOW
                  AND PAOBJ_HIGH = @<FS_Q>-PAOBJ_HIGH
                  AND SKIPS = @<FS_Q>-SKIPS.            
            ENDIF.
        ELSE.
            <FS_Q>-RETRY_CNT = <FS_Q>-RETRY_CNT + 1.
            IF SY-MSGID IS NOT INITIAL.
                MESSAGE ID SY-MSGID TYPE '' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO LV_SYS_MSG.
            ENDIF.

            MESSAGE |X [{ PV_TASK }] RFC 에러 감지 (SUBRC: { LV_RFC_SUBRC }, 사유: { LV_SYS_MSG })| TYPE 'E'.

            

ENDFORM.


        


        
                    


