class ZCL_COMMON definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_ref_stru ,
              field        TYPE fieldname,
              refrollname  TYPE rollname,
              reftabname   TYPE tabname,
              reffieldname TYPE fieldname,
            END OF ty_ref_stru .
  types:
    ty_ref_stru_t TYPE TABLE OF ty_ref_stru .

  class-methods GET_HIERARCHY_GROUP_LIST .
  class-methods MAKE_DYNAMIC_ITAB
    importing
      !IT_ITAB type STANDARD TABLE optional
      !IT_FIELD_INFO type TY_REF_STRU_T
    returning
      value(RO_ITAB) type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_COMMON IMPLEMENTATION.


  METHOD get_hierarchy_group_list.

*    DATA: lv_setid     TYPE sethier-setid,
*          lv_info      TYPE grphinfo,
*          lv_overwrite TYPE sy-datar,
*          lt_nodes     TYPE gseth_node_tab,
*          lt_values    TYPE gseth_val_tab,
*          ls_range     TYPE rsdsselopt,
*          lt_d_values  TYPE TABLE OF setvalues.
*
*    DATA: lo_dref TYPE REF TO data,
*          lo_sref TYPE REF TO data.
*
*    FIELD-SYMBOLS <lt_range> TYPE STANDARD TABLE.
*
*    CONSTANTS lc_u TYPE c VALUE 'u'.
*
**-- Dereference dynamic export parameter.
*    CREATE DATA lo_dref LIKE et_range.
*    ASSIGN lo_dref->* TO <lt_range>.
*    CREATE DATA lo_sref LIKE LINE OF <lt_range>.
*    ASSIGN lo_sref->* TO FIELD-SYMBOL(<ls_range>).
*    DESCRIBE FIELD <ls_range> TYPE DATA(lv_type).
*
*    IF iv_setid IS INITIAL AND iv_group_code IS NOT INITIAL.
*      "Set 'SETID' code
*      CALL FUNCTION 'G_SET_ENCRYPT_SETID'
*        EXPORTING
*          setclass             = iv_setclass
*          shortname            = CONV setid( iv_group_code )
*          kokrs                = iv_kokrs
*          fikrs                = iv_fikrs
*        IMPORTING
*          setid                = lv_setid
*        EXCEPTIONS
*          no_co_area_specified = 1
*          illegal_setclass     = 2
*          OTHERS               = 3.
*    ELSE.
*      lv_setid = iv_setid.
*    ENDIF.
*
*    IF lv_setid IS INITIAL.
*      EXIT.
*    ENDIF.
  ENDMETHOD.


  METHOD make_dynamic_itab.

    DATA ls_stru_comp TYPE abap_componentdescr.
    DATA lt_stru_comp TYPE abap_component_tab.
    DATA lo_table TYPE REF TO data.

    IF it_itab IS SUPPLIED.
      CREATE DATA lo_table LIKE LINE OF it_itab.
      DATA(lr_tabdescr) = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_data_ref( lo_table ) ).
    ENDIF.

    LOOP AT cl_salv_data_descr=>read_structdescr( lr_tabdescr ) INTO DATA(ls_ddfields).
      ls_stru_comp-name = ls_ddfields-fieldname.

      IF ls_ddfields-rollname IS NOT INITIAL.
        DATA(lv_rollname) = ls_ddfields-rollname.
      ELSE.
        IF ls_ddfields-reftable IS NOT INITIAL AND
           ls_ddfields-reffield IS NOT INITIAL.

          SELECT SINGLE rollname INTO @lv_rollname
          FROM dd03vv
          WHERE tabname EQ @ls_ddfields-reftable
          AND fieldname EQ @ls_ddfields-reffield.
        ENDIF.
      ENDIF.

      ls_stru_comp-type ?= cl_abap_typedescr=>describe_by_name( lv_rollname ).

      APPEND ls_stru_comp TO lt_stru_comp.
    ENDLOOP.
    LOOP AT it_field_info INTO DATA(ls_field_info).
      ls_stru_comp-name = ls_field_info-field.

      IF ls_field_info-refrollname IS INITIAL AND
         ls_field_info-reftabname IS NOT INITIAL AND
         ls_field_info-reffieldname IS NOT INITIAL.

        SELECT SINGLE rollname INTO @ls_field_info-refrollname
        FROM dd03vv
        WHERE tabname EQ @ls_field_info-reftabname
        AND fieldname EQ @ls_field_info-reffieldname.
      ENDIF.

      ls_stru_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_field_info-refrollname ).
      APPEND ls_stru_comp TO lt_stru_comp.
    ENDLOOP.

    DATA(lo_struct) = cl_abap_structdescr=>create( lt_stru_comp ).
    DATA(lo_tab) = cl_abap_tabledescr=>create( lo_struct ).

    CREATE DATA ro_itab TYPE HANDLE lo_tab.


  ENDMETHOD.
ENDCLASS.
