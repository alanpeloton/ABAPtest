class ZFI_1566_CL_FACTORY definition
  public
  create public .

public section.

  methods FOR
    importing
      !I_OBJECT type BALOBJ_D
      !I_SUBOBJECT type BALSUBOBJ optional
      !I_ID type BALNREXT optional
    returning
      value(R_LOGGER) type ref to ZCL_UTIL_LOGGER .
  methods READ_FILE
    importing
      !IM_AFILE type RLGRAP-FILENAME
      !IM_RB_AF type CHAR1
      !IM_RB_ST type CHAR1 .
  methods DOCUMENT_POST
    importing
      !IMHEADER type BAPIACHE09
      !IMACCOUNTGL type BAPIACGL09_TAB
      !IMCURRENCY type BAPIACCR09_TAB
    exporting
      !EX_RETURN type BAPIRET2_TAB .
  methods UPDATE_ZTABLE
    importing
      !IV_VBELN type ZZ1_S_ZZ1_PAYMNT_INFO-ZZ_SALES_ORDER
      !IV_ZZ_PAY_STATUS type ZZ1_S_ZZ1_PAYMNT_INFO-ZZ_PAY_STATUS .
  methods GET_TVARVC
    exporting
      !EX_AFF_CLEAR_ACC type RVARI_VAL_255
      !EX_AFF_DISPT_ACC type RVARI_VAL_255
      !EX_AFF_FEEVAR_ACC type RVARI_VAL_255
      !EX_AFF_INCMGBANK_ACC type RVARI_VAL_255
      !EX_AFF_FEE_ACC type RVARI_VAL_255
      !EX_AFF_DOC_TYPE type RVARI_VAL_255 .
  methods DOCUMENT_CHECK
    importing
      !IMHEADER type BAPIACHE09
      !IMACCOUNTGL type BAPIACGL09_TAB
      !IMCURRENCY type BAPIACCR09_TAB
    exporting
      !EX_RETURN type BAPIRET2_TAB .
protected section.
private section.
ENDCLASS.



CLASS ZFI_1566_CL_FACTORY IMPLEMENTATION.


  METHOD document_check.
    DATA: ls_head TYPE bapiache09,
          lt_gl   TYPE STANDARD TABLE OF bapiacgl09,
          lt_curr TYPE STANDARD TABLE OF bapiaccr09,
          lt_ret  TYPE STANDARD TABLE OF bapiret2.
    ls_head = imheader.
    lt_gl = imaccountgl.
    lt_curr = imcurrency.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = ls_head
      TABLES
        accountgl      = lt_gl
        currencyamount = lt_curr
        return         = lt_ret.
    ex_return = lt_ret.
  ENDMETHOD.


  METHOD document_post.

    DATA: ls_head TYPE bapiache09,
          lt_gl   TYPE STANDARD TABLE OF bapiacgl09,
          lt_curr TYPE STANDARD TABLE OF bapiaccr09,
          lt_ret  TYPE STANDARD TABLE OF bapiret2.
    ls_head = imheader.
    lt_gl = imaccountgl.
    lt_curr = imcurrency.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_head
      TABLES
        accountgl      = lt_gl
        currencyamount = lt_curr
        return         = lt_ret.
    ex_return = lt_ret.
  ENDMETHOD.


  METHOD for.

  ENDMETHOD.


  METHOD get_tvarvc.
    DATA : lr_tvarvc TYPE RANGE OF tvarvc-name.
    CONSTANTS : lc_zfi_1622_afrm_clear_acc_gl TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_CLEAR_ACC_GL'   ##NO_TEXT,
                lc_zfi_1622_afrm_dispt_acc_gl TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_DISPUTE_ACC_GL' ##NO_TEXT,
                lc_zfi_1622_afrm_feevaracc_gl TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_FEE_VAR_ACC_GL' ##NO_TEXT,
                lc_zfi_1622_afrm_incmngbnk_gl TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_INCMNG_BANK_GL' ##NO_TEXT,
                lc_zfi_1622_afrm_fee_acc_gl   TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_FEE_ACC_GL'     ##NO_TEXT,
                lc_zfi_1622_afrm_doc_type     TYPE rvari_vnam VALUE 'ZFI_1622_AFFIRM_RECON_DOC_TYPE' ##NO_TEXT.

    lr_tvarvc = VALUE #(
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_clear_acc_gl )
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_dispt_acc_gl )
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_feevaracc_gl )
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_incmngbnk_gl )
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_fee_acc_gl   )
                        ( sign = 'I' option = 'EQ' low = lc_zfi_1622_afrm_doc_type )   ).

    "Fetch Affirm GL Accounts maintained in TVARVC.
    SELECT name,low
     FROM tvarvc
     INTO TABLE @DATA(lt_tvarvc)
      WHERE name IN @lr_tvarvc.
    IF sy-subrc EQ 0.

      READ TABLE lt_tvarvc ASSIGNING FIELD-SYMBOL(<lfs_tvarvc>)
           WITH KEY name = lc_zfi_1622_afrm_clear_acc_gl.
      IF sy-subrc EQ 0.
        ex_aff_clear_acc = <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

      READ TABLE lt_tvarvc INTO <lfs_tvarvc>
           WITH KEY name = lc_zfi_1622_afrm_dispt_acc_gl.
      IF sy-subrc EQ 0.
        ex_aff_dispt_acc = <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

      READ TABLE lt_tvarvc INTO <lfs_tvarvc>
          WITH KEY name = lc_zfi_1622_afrm_feevaracc_gl.
      IF sy-subrc EQ 0.
        ex_aff_feevar_acc = <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

      READ TABLE lt_tvarvc INTO <lfs_tvarvc>
           WITH KEY name = lc_zfi_1622_afrm_fee_acc_gl.
      IF sy-subrc EQ 0.
        ex_aff_fee_acc = <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

      READ TABLE lt_tvarvc INTO <lfs_tvarvc>
           WITH KEY name = lc_zfi_1622_afrm_incmngbnk_gl.
      IF sy-subrc EQ 0.
        ex_aff_incmgbank_acc = <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

      READ TABLE lt_tvarvc INTO <lfs_tvarvc>
           WITH KEY name = lc_zfi_1622_afrm_doc_type.
      IF sy-subrc EQ 0.
        ex_aff_doc_type =  <lfs_tvarvc>-low.
        CLEAR <lfs_tvarvc>.
      ENDIF.

    ENDIF.
    UNASSIGN <lfs_tvarvc>.
  ENDMETHOD.


METHOD read_file.
*----------------------------------------------------------------------*
*  Method Name         : READ_AFFIRM_FILE                              *
*  Author              : Atul Kumar Kar                                *
*  Date                : 03/04/2021                                    *
*  Transport Request # : DS1K902448                                    *
*----------------------------------------------------------------------*
*  Description:        : Method to read file from AL11 and update      *
*                        custom table                                  *
*  Development Class   : ZFI                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* MODIFICATION HISTORY                                                 *
*                                                                      *
*  SAP Change Request #: NA                                            *
*  Change Driver       : NA                                            *
*  Author              : NA                                            *
*  Modification Date   : NA                                            *
*  Description         : NA                                            *
*----------------------------------------------------------------------*

  TYPES : BEGIN OF lty_file_data_temp,
            date           TYPE string,
            charge_date    TYPE string,
            charge_id      TYPE string,
            transaction_id TYPE string,
            order_id       TYPE string,
            event_type     TYPE string,
            sales          TYPE string,
            refunds        TYPE string,
            fees           TYPE string,
            total_settled  TYPE string,
            merchant_name  TYPE string,
            deposit_id     TYPE string,
          END   OF lty_file_data_temp.

  DATA : lwa_file_data_temp  TYPE lty_file_data_temp,
         lwa_zfi_ext_pay_rec TYPE zfi_ext_pay_rec,
         lv_depid            TYPE char50,
         lv_depositid        TYPE char50,
         lv_data             TYPE string,
         lv_strenlen         TYPE i.

  CONSTANTS : lc_n      TYPE char1  VALUE 'N',
              lc_e      TYPE char1  VALUE 'E',
              lc_s      TYPE char1  VALUE 'S',
              lc_affirm TYPE char12 VALUE 'AFFIRM'.

  DATA(lv_file) = im_afile.

  "Add timestamp to file name and to send file to archive folder.
  SPLIT lv_file AT '.' INTO DATA(lv_file_temp) DATA(lv_file_ext).
  DATA(lv_file_archive) = lv_file_temp && sy-uzeit && '.' && lv_file_ext.

  "Read Payment Reconciliation file and append data into internal table
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE TEXT-005 TYPE lc_e DISPLAY LIKE lc_s.
  ELSE.
    DO.

      READ DATASET lv_file INTO lv_data.
      IF sy-subrc EQ 0.
        IF sy-index EQ 1.
          CONTINUE.
        ENDIF.

        SPLIT lv_data AT cl_abap_char_utilities=>horizontal_tab
                      INTO lwa_file_data_temp-date
                           lwa_file_data_temp-charge_date
                           lwa_file_data_temp-charge_id
                           lwa_file_data_temp-transaction_id
                           lwa_file_data_temp-order_id
                           lwa_file_data_temp-event_type
                           lwa_file_data_temp-sales
                           lwa_file_data_temp-refunds
                           lwa_file_data_temp-fees
                           lwa_file_data_temp-total_settled
                           lwa_file_data_temp-merchant_name
                           lwa_file_data_temp-deposit_id.

*        "Remove the last '#' from deposit id.
*        lv_strenlen  = strlen( lwa_file_data_temp-deposit_id ).
*        lv_strenlen  = lv_strenlen - 1.
*        lv_depositid = lwa_file_data_temp-deposit_id+0(lv_strenlen).

        "Convert date to internal format
        SPLIT lwa_file_data_temp-date AT '-' INTO DATA(lv_month) DATA(lv_day) DATA(lv_year).
        DATA(lv_date) = lv_year && lv_month && lv_day.
        CLEAR : lv_month, lv_day,lv_year.
        SPLIT lwa_file_data_temp-charge_date AT '-' INTO lv_month lv_day lv_year.
        DATA(lv_date_2) = lv_year && lv_month && lv_day.


        lwa_zfi_ext_pay_rec-extid           = lc_affirm.
        lwa_zfi_ext_pay_rec-xref_id         = lwa_file_data_temp-charge_id.
        lwa_zfi_ext_pay_rec-deposit_id      = lv_depositid.
        lwa_zfi_ext_pay_rec-zdate           = lv_date.
        lwa_zfi_ext_pay_rec-charge_date     = lv_date_2.
        lwa_zfi_ext_pay_rec-transaction_id  = lwa_file_data_temp-transaction_id.
        lwa_zfi_ext_pay_rec-order_id        = lwa_file_data_temp-order_id.
        lwa_zfi_ext_pay_rec-status          = lc_n.
        lwa_zfi_ext_pay_rec-event_type      = lwa_file_data_temp-event_type.
        lwa_zfi_ext_pay_rec-sales           = lwa_file_data_temp-sales.
        lwa_zfi_ext_pay_rec-refunds         = lwa_file_data_temp-refunds.
        lwa_zfi_ext_pay_rec-fees            = lwa_file_data_temp-fees.
        lwa_zfi_ext_pay_rec-total_settled   = lwa_file_data_temp-total_settled.
        lwa_zfi_ext_pay_rec-merchant_name   = lwa_file_data_temp-merchant_name.
        lwa_zfi_ext_pay_rec-createdby       = sy-uname.
        lwa_zfi_ext_pay_rec-createdon       = sy-datum.
        lwa_zfi_ext_pay_rec-createdat       = sy-uzeit.

        "Update Custom table ZFI_EXT_PAY_REC
        MODIFY zfi_ext_pay_rec FROM lwa_zfi_ext_pay_rec.

        "Transfer file to archive filepath
        OPEN DATASET lv_file_archive FOR OUTPUT IN BINARY MODE.
        "Transfer contents of file to archive file
        TRANSFER lv_data TO lv_file_archive.
        CLOSE DATASET lv_file_archive.

      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
  CLOSE DATASET lv_file.

  "Delete file from Application server file path
*  DELETE DATASET lv_file.

  "Clear Variables.
  CLEAR : lv_file,lv_file_archive,lv_file_ext,lv_file_temp,lv_data,lwa_file_data_temp,lwa_zfi_ext_pay_rec.

ENDMETHOD.


  method UPDATE_ZTABLE.
     TYPES: BEGIN OF ty_key,
             zz_sales_order TYPE cbo_numeric10,
             zz_count       TYPE cbo_numeric2,
           END OF ty_key.

    FIELD-SYMBOLS: <lfs_mod> TYPE /bobf/s_frw_modification.

    DATA: ex_flag       TYPE char1,
          lv_cnt        TYPE i VALUE '0',
          lt_alt_key    TYPE TABLE OF ty_key,
          ls_alt_key    TYPE ty_key,
          lr_s_root     TYPE REF TO zz1_s_zz1_paymnt_info,
          lr_s_txt      TYPE REF TO /bobf/s_demo_short_text_k,
          lr_s_txt_hdr  TYPE REF TO /bobf/s_demo_longtext_hdr_k,
          lr_s_txt_cont TYPE REF TO /bobf/s_demo_longtext_item_k,
          lt_key_out    TYPE  /bobf/t_frw_key,
          lt_data       TYPE zz1_t_zz1_paymnt_info,
          lv_process    TYPE char1,
          lt_mod        TYPE /bobf/t_frw_modification,
          lv_boolean    TYPE boolean.

    TRY.
*   *Obtain a reference to the BOPF transaction manager:
        DATA(lo_txn_mngr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
*    *Obtain a reference to the BOPF service manager:
        DATA(lo_svc_mngr) =
          /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
            zz1_if_zz1_paymnt_info_c=>sc_bo_key ).
*Access the metadata for the ZZ1_IF_ZZ1_PAYMNT_INFO_C BO:
        DATA(lo_bo_conf) =
          /bobf/cl_frw_factory=>get_configuration(
            zz1_if_zz1_paymnt_info_c=>sc_bo_key ).

        lv_cnt = lv_cnt + 1.
        ls_alt_key-zz_sales_order = iv_vbeln.
        ls_alt_key-zz_count = lv_cnt.
        APPEND ls_alt_key TO lt_alt_key.

        CALL METHOD lo_svc_mngr->convert_altern_key
          EXPORTING
            iv_node_key   = zz1_if_zz1_paymnt_info_c=>sc_node-zz1_paymnt_info                                      " Node
            iv_altkey_key = zz1_if_zz1_paymnt_info_c=>sc_alternative_key-zz1_paymnt_info-sap_altkey
            it_key        = lt_alt_key
          IMPORTING
            et_key        = lt_key_out
            eo_message    = DATA(lo_message2).                                      " Interface of Message Object

        CALL METHOD lo_svc_mngr->retrieve
          EXPORTING
            iv_node_key   = zz1_if_zz1_paymnt_info_c=>sc_node-zz1_paymnt_info   " Node
            it_key        = lt_key_out                                  " Key Table
          IMPORTING
            eo_message    = DATA(lo_msg2)                                   " Interface of Message Object
            et_data       = lt_data
            et_failed_key = DATA(lt_key2).                                   " Key Table

        READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY zz_sales_order = iv_vbeln.
        IF sy-subrc EQ 0.

        "Build the ROOT node:
        CREATE DATA lr_s_root.

          lr_s_root->key = <fs_data>-key.
          lr_s_root->zz_pay_status    = iv_zz_pay_status.
         APPEND INITIAL LINE TO lt_mod ASSIGNING <lfs_mod>.
               <lfs_mod>-node        = zz1_if_zz1_paymnt_info_c=>sc_node-zz1_paymnt_info.
              <lfs_mod>-change_mode = /bobf/if_frw_c=>sc_modify_update.
              <lfs_mod>-key         = lr_s_root->key.
                <lfs_mod>-data        = lr_s_root.

          APPEND INITIAL LINE TO <lfs_mod>-changed_fields[] ASSIGNING FIELD-SYMBOL(<fs_val2>).
          CHECK sy-subrc EQ 0.
          <fs_val2> = 'ZZ_PAY_STATUS'.

        ENDIF.

        CALL METHOD lo_svc_mngr->modify
          EXPORTING
            it_modification = lt_mod
          IMPORTING
            eo_change       = DATA(lo_change)
            eo_message      = DATA(lo_message).

        "Apply the transactional changes:
        CALL METHOD lo_txn_mngr->save
          IMPORTING
            eo_message  = lo_message
            ev_rejected = DATA(lv_rejected).
        IF lv_rejected IS INITIAL.
          ex_flag = 'X'.
        ELSE.
          ex_flag = ' '.
        ENDIF.

      CATCH
        /bobf/cx_frw INTO DATA(lx_frw).

    ENDTRY.
  endmethod.
ENDCLASS.
