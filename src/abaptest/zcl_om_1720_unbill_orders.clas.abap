class ZCL_OM_1720_UNBILL_ORDERS definition
  public
  final
  create public .

public section.

  methods GET_FAILED_AUTH_ORDERS
    importing
      value(S_DATE) type ZTTOM_1720_DATE
    exporting
      value(LT_FINAL) type ZTTOM_1720_FINAL_TABLE .
  methods GET_NO_PAYMENT_ORDERS
    importing
      value(S_DATE) type ZTTOM_1720_DATE
    exporting
      !LT_FINAL type ZTTOM_1720_FINAL_TABLE .
  methods GET_DPC_ORDERS
    importing
      value(S_DATE) type ZTTOM_1720_DATE
    exporting
      !LT_FINAL type ZTTOM_1720_FINAL_TABLE .
  methods GET_INVALID_ORDERS
    importing
      value(S_SALES) type ZTTOM_1720_SALESORDER
    exporting
      !LT_TABLE1 type ZTTOM_1720_PAYMENT_TABLE1 .
  methods SEND_EMAIL
    importing
      !LT_TABLE type ZTTOM_1720_FINAL_TABLE optional
      !P_EMAIL type ADR6-SMTP_ADDR
      !LT_TABLE1 type ZTTOM_1720_PAYMENT_TABLE1 optional
      !LS_BODY type CHAR1 .
  methods CREATE_JOB
    importing
      !I_JOBNAME type TBTCJOB-JOBNAME
      !S_DATE type ZTTOM_1720_DATE
      !I_UNBILL type CHAR1
      !I_INVA type CHAR1
      !I_AUTH type CHAR1
      !I_PAY type CHAR1
      !I_CAP type CHAR1
      !S_SALES type ZTTOM_1720_SALESORDER
    exporting
      !E_JOBCOUNT type TBTCJOB-JOBCOUNT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OM_1720_UNBILL_ORDERS IMPLEMENTATION.


  METHOD create_job.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname                = i_jobname
     IMPORTING
       jobcount               = e_jobcount
     EXCEPTIONS
       cant_create_job        = 1
       invalid_job_data       = 2
       jobname_missing        = 3
       OTHERS                 = 4
              .
    IF sy-subrc EQ 0.
    SUBMIT zom_1720_cust_unbill_orders
        WITH      s_date IN s_date
        WITH      p_unbill = i_unbill
        WITH      p_inva = i_inva
        WITH      p_auth = i_auth
        WITH      p_pay = i_pay
        WITH      p_cap = i_cap
        WITH      s_sales IN s_sales
        VIA JOB i_jobname NUMBER e_jobcount
        AND RETURN.
     ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount                          = e_jobcount
        jobname                           = i_jobname
        strtimmed                         = 'X'
      EXCEPTIONS
         cant_start_immediate              = 1
         invalid_startdate                 = 2
         jobname_missing                   = 3
         job_close_failed                  = 4
         job_nosteps                       = 5
         job_notex                         = 6
         lock_failed                       = 7
         invalid_target                    = 8
         invalid_time_zone                 = 9
         OTHERS                            = 10
          .
IF sy-subrc EQ 0.
MESSAGE : 'Job scheduled' TYPE 'S'.
ENDIF.

  ENDMETHOD.


  METHOD get_dpc_orders.

* --------> Table declaration
DATA: lt_text TYPE TABLE OF tline,
          lt_lines TYPE  soli_tab,
          lt_table1 TYPE zttom_1720_payment_table,

* --------> Workarea declaration
          ls_lines LIKE LINE OF lt_lines,
          ls_final2 LIKE LINE OF lt_final.

* --------> Constants declaration
CONSTANTS lc_dpc TYPE char2 VALUE '06'.

* --------> Processing logic
SELECT zz_sales_order zz_pay_status zz_processor zz_amount zz_srvc_count zz_re_count
  FROM zz1_fe8520560b9e INTO TABLE lt_table1 WHERE zz_pay_status EQ lc_dpc.                  "DPC orders
      CHECK sy-subrc EQ '0' AND lt_table1[] IS NOT INITIAL.
      SORT lt_table1.
      DELETE ADJACENT DUPLICATES FROM lt_table1[] COMPARING zz_sales_order zz_pay_status.
      SELECT * FROM vbak INTO TABLE @DATA(lt_vbak) FOR ALL ENTRIES IN @lt_table1 WHERE vbeln = @lt_table1-zz_sales_order.
        CHECK sy-subrc EQ '0' AND lt_vbak IS NOT INITIAL.
         SELECT * FROM vbkd INTO TABLE @DATA(lt_vbkd) FOR ALL ENTRIES IN @lt_vbak
          WHERE vbeln EQ @lt_vbak-vbeln.
           CHECK sy-subrc EQ '0' AND lt_vbkd[] IS NOT INITIAL.
          SORT lt_vbkd.
          DELETE ADJACENT DUPLICATES FROM lt_vbkd[] COMPARING vbeln fkdat.

LOOP AT lt_table1[] INTO DATA(ls_table1).
  READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = ls_table1-zz_sales_order.
  IF ls_vbak IS NOT INITIAL.
    IF NOT  ls_vbak-erdat IN s_date.
    EXIT.
    ENDIF.
    READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_vbak-vbeln.
    CHECK ls_vbkd IS NOT INITIAL.
      IF ls_vbak-vbeln NE ls_table1-zz_sales_order.
      EXIT.
      ENDIF.
      ls_final2-zz1_uni_ordid_sdh = ls_vbak-zz1_uni_ordid_sdh.
      ls_final2-zz1_sysorgin_sdh = ls_vbak-zz1_sysorgin_sdh.
      ls_final2-vbeln = ls_vbak-vbeln.
      ls_final2-auart = ls_vbak-auart.
      ls_final2-zz_amount = ls_table1-zz_amount.
      ls_final2-zz_processor = ls_table1-zz_amount.
      ls_final2-zz_pay_status = ls_table1-zz_pay_status.
      ls_final2-fkdat = ls_vbkd-fkdat.
      ls_final2-erdat = ls_vbak-erdat.
      ls_final2-zz_re_count = ls_table1-zz_pay_status.
      ls_final2-zz_srvc_count = ls_table1-zz_srvc_count.
      APPEND ls_final2 TO lt_final.
      CLEAR ls_final2.
    ENDIF.
  ENDLOOP.
  ENDMETHOD.


  METHOD get_failed_auth_orders.


* -------> Table declaration
DATA: lt_text TYPE TABLE OF tline,                     " Table to store standard text
          lt_lines TYPE  soli_tab,
          lt_table1 TYPE zttom_1720_payment_table,

* -------> Workarea declaration
           ls_lines LIKE LINE OF lt_lines,
           ls_final TYPE zsom_1720_final_table.

* -------> Constant declaration
CONSTANTS lc_failed_auth TYPE char2 VALUE '03'.

* -------> Processing logic
  SELECT zz_sales_order zz_pay_status zz_processor zz_amount zz_srvc_count zz_re_count
    FROM zz1_fe8520560b9e INTO TABLE lt_table1 WHERE zz_pay_status EQ lc_failed_auth.       "Re-authorization failed status
      CHECK sy-subrc EQ '0' AND lt_table1[] IS NOT INITIAL.
      SORT lt_table1.
      DELETE ADJACENT DUPLICATES FROM lt_table1[] COMPARING zz_sales_order zz_pay_status.
      SELECT * FROM vbak INTO TABLE @DATA(lt_vbak) FOR ALL ENTRIES IN @lt_table1 WHERE vbeln = @lt_table1-zz_sales_order.
        CHECK sy-subrc EQ '0' AND lt_vbak IS NOT INITIAL.
        SELECT * FROM vbkd INTO TABLE @DATA(lt_vbkd) FOR ALL ENTRIES IN @lt_vbak
          WHERE vbeln EQ @lt_vbak-vbeln.
          CHECK sy-subrc EQ '0' AND lt_vbkd[] IS NOT INITIAL.
          SORT lt_vbkd.
          DELETE ADJACENT DUPLICATES FROM lt_vbkd[] COMPARING vbeln fkdat.

LOOP AT lt_table1[] INTO DATA(ls_table1).
  READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = ls_table1-zz_sales_order.
  IF ls_vbak IS NOT INITIAL.
    IF NOT  ls_vbak-erdat IN s_date.
    EXIT.
    ENDIF.
    READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_vbak-vbeln.
    CHECK ls_vbkd IS NOT INITIAL.
      IF ls_vbak-vbeln NE ls_table1-zz_sales_order.
      EXIT.
      ENDIF.
      ls_final-zz1_uni_ordid_sdh = ls_vbak-zz1_uni_ordid_sdh.
      ls_final-zz1_sysorgin_sdh = ls_vbak-zz1_sysorgin_sdh.
      ls_final-vbeln = ls_vbak-vbeln.
      ls_final-auart = ls_vbak-auart.
      ls_final-zz_amount = ls_table1-zz_amount.
      ls_final-zz_processor = ls_table1-zz_amount.
      ls_final-zz_pay_status = ls_table1-zz_pay_status.
      ls_final-fkdat = ls_vbkd-fkdat.
      ls_final-erdat = ls_vbak-erdat.
      ls_final-zz_re_count = ls_table1-zz_pay_status.
      ls_final-zz_srvc_count = ls_table1-zz_srvc_count.
      APPEND ls_final TO lt_final.
      CLEAR ls_final.
    ENDIF.
  ENDLOOP.

  ENDMETHOD.


  METHOD get_invalid_orders.

* --------> Constants declaration
CONSTANTS: lc_captured TYPE char2 VALUE '05',
               lc_refunded TYPE char2 VALUE '07',
               lc_cancelled TYPE char2  VALUE '09',
               lc_failed_auth TYPE char2 VALUE '03',
               lc_no_pay TYPE char2 VALUE '10',
               lc_dpc TYPE char2 VALUE '06'.

* ---------> Class object declaration
  DATA : lo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr,
         lo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager,
         lo_bo_conf  TYPE REF TO /bobf/if_frw_configuration,
         lo_change   TYPE REF TO /bobf/if_tra_change,
         lo_message  TYPE REF TO /bobf/if_frw_message,
         lr_s_root     TYPE REF TO zz1_s_zz1_paymnt_info,

* ---------> Workarea declarations
         ls_pmt      TYPE  zz1_fe8520560b9e,

* ---------> Table declaration
         lt_mod      TYPE /bobf/t_frw_modification,

* ---------> Variable declaration
         lv_rejected TYPE boole_d.

* ---------> Field-symbol declaration
FIELD-SYMBOLS: <lfs_mod> LIKE LINE OF lt_mod.

* --------> Processing logic
SELECT SAP_UUID zz_sales_order zz_pay_status zz_processor zz_amount zz_srvc_count zz_re_count
  FROM zz1_fe8520560b9e INTO TABLE lt_table1
  WHERE zz_sales_order IN s_sales.
  CHECK sy-subrc EQ '0' AND lt_table1[] IS NOT INITIAL.
  SORT lt_table1.
  DELETE ADJACENT DUPLICATES FROM lt_table1 COMPARING zz_sales_order zz_pay_status.
  LOOP AT lt_table1 INTO DATA(ls_table1).
*    IF ls_table1-zz_pay_status NE lc_captured OR ls_table1-zz_pay_status = lc_refunded.
      IF ls_table1-zz_pay_status EQ lc_failed_auth OR ls_table1-zz_pay_status EQ lc_no_pay OR ls_table1-zz_pay_status EQ lc_dpc.
      ls_table1-zz_pay_status = lc_cancelled.
*      MODIFY TABLE lt_table1 FROM ls_table1 TRANSPORTING zz_pay_status.
*      SELECT SINGLE * FROM zz1_fe8520560b9e INTO @DATA(ls_update)
*        WHERE zz_sales_order = @ls_table1-zz_sales_order.
*      IF sy-subrc = 0.
*        ls_update-zz_pay_status = ls_table1-zz_pay_status.
*        UPDATE zz1_fe8520560b9e SET zz_pay_status = ls_update-zz_pay_status
*          WHERE zz_sales_order EQ ls_update-zz_sales_order.
*        COMMIT WORK.
*      ENDIF.

*       MOVE-CORRESPONDING p_ls_zz1_pay  TO ls_pmt.
       MOVE-CORRESPONDING ls_table1  TO ls_pmt.
     TRY.
        "Obtain a reference to the BOPF transaction manager:
        lo_txn_mngr =
          /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        "Obtain a reference to the BOPF service manager:
        lo_svc_mngr =
          /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
            zz1_if_zz1_paymnt_info_c=>sc_bo_key ).

        "Access the metadata for the ZZ1_IF_ZZ1_PAYMNT_INFO_C BO:
        lo_bo_conf =
          /bobf/cl_frw_factory=>get_configuration(
            zz1_if_zz1_paymnt_info_c=>sc_bo_key ).

        "Build the ROOT node:
        CREATE DATA lr_s_root.
        lr_s_root->key = /bobf/cl_frw_factory=>get_new_key( ).
        lr_s_root->zz_sales_order   = ls_pmt-zz_sales_order.
        lr_s_root->zz_count         = ls_pmt-zz_count.
        lr_s_root->zz_card_type     = ls_pmt-zz_card_type.
        lr_s_root->zz_card_no       = ls_pmt-zz_card_no.
        lr_s_root->zz_exp_date      = ls_pmt-zz_exp_date.
        lr_s_root->zz_auth_id       = ls_pmt-zz_auth_id.
        lr_s_root->zz_cap_id        = ls_pmt-zz_cap_id.
        lr_s_root->zz_pay_status    = ls_pmt-zz_pay_status.
        lr_s_root->zz_processor     = ls_pmt-zz_processor.
        lr_s_root->zz_amount        = ls_pmt-zz_amount.
        lr_s_root->zz_pay_type      = ls_pmt-zz_pay_type.
        lr_s_root->zz_card_cat      = ls_pmt-zz_card_cat.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <lfs_mod>.
        <lfs_mod>-node        = zz1_if_zz1_paymnt_info_c=>sc_node-zz1_paymnt_info.
        <lfs_mod>-change_mode = /bobf/if_frw_c=>sc_modify_update.
        <lfs_mod>-key         = lr_s_root->key.
        <lfs_mod>-data        = lr_s_root.

        "Create the customer record:
        CALL METHOD lo_svc_mngr->modify
          EXPORTING
            it_modification = lt_mod
          IMPORTING
            eo_change       = lo_change
            eo_message      = lo_message.

        "Apply the transactional changes:
        CALL METHOD lo_txn_mngr->save
          IMPORTING
            eo_message  = lo_message
            ev_rejected = lv_rejected.
      CATCH /bobf/cx_frw INTO DATA(lx_frw).

    ENDTRY.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_no_payment_orders.

* --------> Table declaration
DATA: lt_text TYPE TABLE OF tline,
          lt_lines TYPE  soli_tab,
          lt_table1 TYPE zttom_1720_payment_table,

* ---------> Workarea declaration
          ls_lines LIKE LINE OF lt_lines,
          ls_final1 TYPE zsom_1720_final_table.

* ---------> Constants decalration
CONSTANTS lc_no_pay TYPE char2 VALUE '10'.

* ---------> Processing logic
SELECT zz_sales_order ZZ_PAY_STATUs zz_processor zz_amount zz_srvc_count zz_re_count FROM zz1_fe8520560b9e
  INTO TABLE lt_table1 WHERE zz_pay_status EQ lc_no_pay.                                                      "No Payment orders
      CHECK sy-subrc EQ '0' AND lt_table1[] IS NOT INITIAL.
      SORT lt_table1.
      DELETE ADJACENT DUPLICATES FROM lt_table1[] COMPARING zz_sales_order zz_pay_status.
      SELECT * FROM vbak INTO TABLE @DATA(lt_vbak) FOR ALL ENTRIES IN @lt_table1 WHERE vbeln = @lt_table1-zz_sales_order.
        CHECK sy-subrc EQ '0' AND lt_vbak IS NOT INITIAL.
        SELECT * FROM vbkd INTO TABLE @DATA(lt_vbkd) FOR ALL ENTRIES IN @lt_vbak
          WHERE vbeln = @lt_vbak-vbeln.
          SORT lt_vbkd.
          DELETE ADJACENT DUPLICATES FROM lt_vbkd[] COMPARING vbeln fkdat.

LOOP AT lt_table1[] INTO DATA(ls_table1).
  READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = ls_table1-zz_sales_order.
  IF ls_vbak IS NOT INITIAL.
    IF NOT  ls_vbak-erdat IN s_date.
    EXIT.
    ENDIF.
    READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_vbak-vbeln.
    CHECK ls_vbkd IS NOT INITIAL.
      IF ls_vbak-vbeln NE ls_table1-zz_sales_order.
      EXIT.
      ENDIF.
      ls_final1-zz1_uni_ordid_sdh = ls_vbak-zz1_uni_ordid_sdh.
      ls_final1-zz1_sysorgin_sdh = ls_vbak-zz1_sysorgin_sdh.
      ls_final1-vbeln = ls_vbak-vbeln.
      ls_final1-auart = ls_vbak-auart.
      ls_final1-zz_amount = ls_table1-zz_amount.
      ls_final1-zz_processor = ls_table1-zz_amount.
      ls_final1-zz_pay_status = ls_table1-zz_pay_status.
      ls_final1-fkdat = ls_vbkd-fkdat.
      ls_final1-erdat = ls_vbak-erdat.
      ls_final1-zz_re_count = ls_table1-zz_pay_status.
      ls_final1-zz_srvc_count = ls_table1-zz_srvc_count.
      APPEND ls_final1 TO lt_final.
      CLEAR ls_final1.
    ENDIF.
  ENDLOOP.
  ENDMETHOD.


  METHOD send_email.

* -------> Data declarations
 DATA: lt_text TYPE TABLE OF tline,
          lt_lines TYPE  soli_tab,
          lt_solixtab TYPE solix_tab,
            lt_emailbody TYPE soli_tab.

* -------> Workarea declarations
DATA : ls_lines LIKE LINE OF lt_lines,
        ls_emailbody         TYPE soli.

* -------> Variable declarations
DATA : lv_zz_amount(14) TYPE c,
       lv_string TYPE string,
       lv_length            TYPE i,
       lv_sent_to_all       TYPE abap_bool,
       lv_content_bin       TYPE xstring,
       lv_fname             TYPE so_obj_des,
       lv_subject           TYPE so_obj_des,
       lv_filecontent       TYPE string,
       lv_quote_guid        TYPE crmt_object_guid,
       lv_attachment_string TYPE string,
       lv_mailto            TYPE string,
       lv_index             TYPE sy-tabix,
       lv_type              TYPE char3.

    DATA binary_content TYPE solix_tab.
    DATA size           TYPE so_obj_len.

* --------> Constants declaration
CONSTANTS: gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
           gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf,
           lc_id TYPE thead-tdid VALUE 'ST',
           lc_raw TYPE so_obj_tp VALUE 'RAW',
           lc_xls TYPE so_obj_tp VALUE 'XLS',
           lc_lang TYPE spras VALUE 'E',
           lc_sttxt TYPE tdobname VALUE 'ZST_OM_UNBILL_ORDERS',
           lc_sttxt1 TYPE tdobname VALUE 'ZST_OM_INVALID_ORDERS',
           lc_object TYPE tdobject VALUE 'TEXT',
           lc_subject(50) TYPE c VALUE 'Unbilled Orders',
           lc_subject1(50) type c VALUE 'Cancelled Orders',
           lc_title1(50) TYPE c VALUE 'Unbilled Sales Order Report',
           lc_title2(50) TYPE c VALUE 'Cancelled Sales Order Report',
           lc_noorder(20) TYPE c VALUE 'No Orders found'.

* ---------> Class objects
DATA: lr_document     TYPE REF TO cl_document_bcs,
      lr_recipient    TYPE REF TO cl_cam_address_bcs,
      lr_send_request TYPE REF TO cl_bcs.

* ---------> Processing logic

*EMAILBODY-----
IF ls_body IS NOT INITIAL.
      CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                            = lc_id
      language                      = lc_lang
      name                          = lc_sttxt
      object                        = lc_object
    TABLES
      lines                         = lt_text
   EXCEPTIONS
     id                            = 1
     language                      = 2
     name                          = 3
     not_found                     = 4
     object                        = 5
     reference_check               = 6
     wrong_access_to_archive       = 7
     OTHERS                        = 8
            .
      lv_subject = lc_subject.
  ELSEIF
        ls_body IS INITIAL.
        CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                            = lc_id
      language                      = lc_lang
      name                          = lc_sttxt1
      object                        = lc_object
    TABLES
      lines                         = lt_text
   EXCEPTIONS
     id                            = 1
     language                      = 2
     name                          = 3
     not_found                     = 4
     object                        = 5
     reference_check               = 6
     wrong_access_to_archive       = 7
     OTHERS                        = 8
            .
        lv_subject = lc_subject1.
  ENDIF.
  CHECK sy-subrc EQ '0'.
  LOOP AT lt_text INTO DATA(ls_text).
    APPEND ls_text-tdline TO lt_lines.
  ENDLOOP.

*Create persistent send request
  TRY.
      lr_send_request = cl_bcs=>create_persistent( ).
    CATCH cx_send_req_bcs.
  ENDTRY.
  TRY.
      lr_document = cl_document_bcs=>create_document(
            i_type    = lc_raw
            i_text    = lt_lines
            i_subject = lv_subject ).
    CATCH cx_document_bcs.
  ENDTRY.

* convert internal table to excel.
IF ls_body IS NOT INITIAL.
  If lt_table IS NOT INITIAL.
* excel header
  CONCATENATE lc_title1                  "#EC NOTEXT
              gc_crlf gc_crlf
              INTO lv_string.
  CONCATENATE 'Universal Order ID'    gc_tab
              'System of Origin'      gc_tab
              'Order No'              gc_tab
              'Order Type'            gc_tab
              'Order Amount'          gc_tab
              'Processor'             gc_tab
              'Payment Status'        gc_tab
              'Billing Date'          gc_tab
              'Order Date'            gc_tab
              'Reauth-Counter'        gc_tab
              'Service-order counter' gc_crlf
              INTO lv_string.
  LOOP AT lt_table INTO DATA(wa_table).
    WRITE wa_table-zz_amount TO lv_zz_amount.
    CONCATENATE lv_string
                wa_table-zz1_uni_ordid_sdh  gc_tab
                wa_table-zz1_sysorgin_sdh   gc_tab
                wa_table-vbeln              gc_tab
                wa_table-auart              gc_tab
                lv_zz_amount                gc_tab
                wa_table-zz_processor       gc_tab
                wa_table-zz_pay_status      gc_tab
                wa_table-fkdat              gc_tab
                wa_table-erdat              gc_tab
                wa_table-zz_re_count        gc_tab
                wa_table-zz_srvc_count      gc_crlf
                INTO lv_string.
    CLEAR lv_zz_amount.
  ENDLOOP.
  ELSEIF lt_table IS INITIAL.
    CONCATENATE lc_noorder                 "#EC NOTEXT
              gc_crlf gc_crlf
              INTO lv_string.

  ENDIF.
  ENDIF.
  IF ls_body EQ space.
  IF lt_table1 IS NOT INITIAL.
* excel header
  CONCATENATE lc_title2                  "#EC NOTEXT
              gc_crlf gc_crlf
              INTO lv_string.
  CONCATENATE 'Order No'   gc_tab
              'Status'     gc_crlf
              INTO lv_string.
  LOOP AT lt_table1 INTO DATA(ls_table1).
    CONCATENATE lv_string
                ls_table1-zz_sales_order gc_tab
                'Canceled'               gc_crlf
                INTO lv_string.
  ENDLOOP.
  ELSE.
    CONCATENATE lc_noorder                 "#EC NOTEXT
              gc_crlf gc_crlf
              INTO lv_string.
  ENDIF.
  ENDIF.
* convert the text string into UTF-16LE binary data including
* byte-order-mark. Mircosoft Excel prefers these settings

  TRY.
      cl_bcs_convert=>string_to_solix(
        EXPORTING
          iv_string   = lv_string
          iv_codepage = '4103'  "suitable for MS Excel, leave empty
          iv_add_bom  = 'X'     "for other doc types
        IMPORTING
          et_solix  = binary_content
          ev_size   = size ).
    CATCH cx_bcs.
      MESSAGE e445(so).
  ENDTRY.
*add document attachment'
      TRY.
          CALL METHOD lr_document->add_attachment
            EXPORTING
              i_attachment_type    = lc_xls
              i_attachment_subject = lc_title1
              i_att_content_hex    = binary_content.
        CATCH cx_document_bcs.
      ENDTRY.
*set document
  TRY.
      lr_send_request->set_document( lr_document ).
    CATCH cx_send_req_bcs.
  ENDTRY.

  TRY.
      lr_recipient = cl_cam_address_bcs=>create_internet_address( p_email ).           "EMAIL ID
    CATCH cx_address_bcs.
  ENDTRY.
    TRY.
      lr_send_request->add_recipient( lr_recipient ).
    CATCH cx_send_req_bcs.
  ENDTRY.
*Send email
  TRY.
      lv_sent_to_all = lr_send_request->send( i_with_error_screen = 'X' ).
      COMMIT WORK.
    CATCH cx_send_req_bcs.
  ENDTRY.
  IF sy-batch IS NOT INITIAL.
  MESSAGE 'Email sent.' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.
  ENDMETHOD.
ENDCLASS.
