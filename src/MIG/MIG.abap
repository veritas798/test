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
    
