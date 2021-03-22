*"* use this source file for your ABAP unit test classes
CLASS ltcl_sd_sls_drf_so_processing DEFINITION DEFERRED.

CLASS ZCL_OM_SAP_ECOM_ORD_UPD DEFINITION LOCAL FRIENDS ltcl_sd_sls_drf_so_processing.

CLASS ltcl_sd_sls_drf_so_processing DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION MEDIUM
  FINAL
  .
  PRIVATE SECTION.
    CLASS-METHODS:
      class_setup,
      class_teardown.
    METHODS:
      setup,
      teardown,
      initialize_1 FOR TESTING,
      analyze_chp_1 FOR TESTING,
      send_message_1 FOR TESTING,
      read_complete_data_1 FOR TESTING.


ENDCLASS.

CLASS ltcl_sd_sls_drf_so_processing IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.

  METHOD class_teardown.




  ENDMETHOD.
  METHOD read_complete_data_1.
    DATA: lt_act_change_objects TYPE tdt_sdsls_drf_sales_order,
          lt_exp_change_objects TYPE tdt_sdsls_drf_sales_order,
          lt_vbak               TYPE STANDARD TABLE OF vbak.
    TEST-INJECTION seam_get_factory.
      so_drf_factory = NEW ldcl_drf_factory1142(  ).
    end-test-injection.
    TEST-INJECTION seam_message_provider.
    end-test-injection.

    lt_vbak = VALUE #( ( vbeln = '0123456789' vbtyp = 'C' )
                       ( vbeln = '0123456788' vbtyp = 'B' )
                       ( vbeln = '0123456786' vbtyp = 'C' vbklt = 'P' ) ).

    DATA(lo_osql_test_environment) = cl_osql_test_environment=>create( VALUE #( ( 'VBAK' ) ) ).
    lo_osql_test_environment->insert_test_data( lt_vbak ).

    DATA(lo_drf_proc) = NEW ZCL_OM_SAP_ECOM_ORD_UPD( is_runtime_parameter = VALUE #(  appl   = 'SD_UNITEST'
                                                                                          runmod = if_drf_const=>runmod_bypass_filter )  ).

    ldcl_drf_factory1142=>so_drf_instance = lo_drf_proc.
    lt_act_change_objects = VALUE #( (
     vbeln = '0123456789' ) (
     vbeln = '0123456788' ) (
     vbeln = '0123456787' ) (
     vbeln = '0123456786' ) ).
    TRY.
        lo_drf_proc->if_drf_outbound~read_complete_data(
          CHANGING
            ct_relevant_objects = lt_act_change_objects ).

      CATCH cx_root.
        cl_aunit_assert=>fail(
          EXPORTING
            msg    = 'Exception raised!' ).

    ENDTRY.

    lt_exp_change_objects = VALUE #( ( vbeln = '0123456789' vbtyp = 'C' ) ).

    cl_aunit_assert=>assert_equals(
       exp = lt_exp_change_objects
       act = lt_act_change_objects
       msg = 'Wrong change objects determined' ).

    lo_osql_test_environment->destroy( ).
  ENDMETHOD.


  METHOD send_message_1.
    CONSTANTS lc_test_key TYPE string VALUE 'send_message'.
    DATA: lt_change_pointer TYPE mdg_cp_t_cp,
          lt_act_rep_sta    TYPE drf_t_obj_rep_sta_full,
          lt_exp_rep_sta    TYPE drf_t_obj_rep_sta_full.
    TEST-INJECTION seam_get_factory.
      so_drf_factory = NEW ldcl_drf_factory1142(  ).
    end-test-injection.
    TEST-INJECTION seam_message_provider.
      mo_message_provider = NEW ldcl_message_provider( ).
    end-test-injection.
*    TEST-INJECTION send_async_out.
*    end-test-injection.


    DATA(lo_drf_proc) = NEW ZCL_OM_SAP_ECOM_ORD_UPD( is_runtime_parameter = VALUE #(  appl = 'SD_UNITEST'
                                                                                          business_system = 'SYST_ID' )  ).
    lt_act_rep_sta = VALUE #( ( object_id = '1234567890C    01' )
                                  ( object_id = '1234567891C    02' )
                                  ( object_id = '1234567892     03' ) ).
    CAST ldcl_message_provider( lo_drf_proc->mo_message_provider )->mt_test_data = VALUE #( (
      key                  = lc_test_key
*      it_sales_order_id    = value #( ( vbeln = '1234567890' vbtyp = 'C' vbklt = '' action_code = '01' )
*                                      ( vbeln = '1234567891' vbtyp = 'C' vbklt = '' action_code = '02' )
*                                      ( vbeln = '1234567892' vbtyp = 'C' vbklt = '' action_code = '03' ) )
*      ir_drf_runtime_param =  lo_drf_proc->ms_runtime_param
*      ct_obj_rep_sta_in    = lt_act_rep_sta
      ct_obj_rep_sta_out   = VALUE #( ( object_id = '1234567890' )
                                      ( object_id = '1234567891' )
                                      ( object_id = '1234567892' ) )
      rt_sales_order       = VALUE #( ( sales_order_id = '1234567890' sales_order_type = 'C' )
                                      ( sales_order_id = '1234567891' sales_order_type = 'C' )
                                      ( sales_order_id = '1234567892' sales_order_type = 'C' )  ) ) ).

    ldcl_drf_factory1142=>so_drf_instance = lo_drf_proc.
    CAST ldcl_message_provider( lo_drf_proc->mo_message_provider )->mv_key = lc_test_key.

    TRY.
        lo_drf_proc->if_drf_outbound~send_message(
          EXPORTING
            iv_object_count =  20                " Package Size of an Outgoing Message
            is_bus_sys_tech = VALUE #( bus_sys_id = 'SYST_ID' )
          CHANGING
            ct_obj_rep_sta  =   lt_act_rep_sta ).
      CATCH cx_root.
        cl_aunit_assert=>fail(
          EXPORTING
            msg    = 'Exception raised!' ).
    ENDTRY.

    lt_exp_rep_sta = VALUE #( ( object_id = '1234567890' )
                              ( object_id = '1234567891' )
                              ( object_id = '1234567892' ) ).

    READ TABLE  CAST ldcl_message_provider( lo_drf_proc->mo_message_provider )->mt_test_data INTO DATA(ls_test_data) WITH KEY key = lc_test_key.

*  data(lt_act_it_sales) = cast ldcl_message_provider( lo_drf_proc->mo_message_provider )->mt_test_data[ key = lc_test_key].

    cl_aunit_assert=>assert_equals(
      exp = lt_exp_rep_sta
      act = lt_act_rep_sta
      msg = 'Wrong change objects determined' ).
    cl_aunit_assert=>assert_equals(
      exp = VALUE if_sd_sls_drf_message_provider=>tct_sales_order_id( ( vbeln = '1234567890' vbtyp = 'C' vbklt = '' action_code = '01' )
                                                                      ( vbeln = '1234567891' vbtyp = 'C' vbklt = '' action_code = '02' )
                                                                      ( vbeln = '1234567892' vbtyp = '' vbklt = '' action_code = '03' ) )
      act = ls_test_data-it_sales_order_id
      msg = 'wrong it_Sales_order detected' ).

    cl_aunit_assert=>assert_equals(
      exp = lo_drf_proc->ms_runtime_param
      act = ls_test_data-ir_drf_runtime_param
      msg = 'Wrong runtime paramter' ).

    cl_aunit_assert=>assert_equals(
      exp = VALUE drf_t_obj_rep_sta_full( ( object_id = '1234567890C    01' )
                                          ( object_id = '1234567891C    02' )
                                          ( object_id = '1234567892     03' ) )
      act = ls_test_data-ct_obj_rep_sta_in
      msg = 'Wrong changing parameter' ).

    cl_aunit_assert=>assert_not_initial(
      act =  lo_drf_proc->ms_output-sales_order_bulk_replication-message_header
      msg = 'Message header should be filled ').

    cl_aunit_assert=>assert_equals(
     exp = lo_drf_proc->ms_output-sales_order_bulk_replication-message_header-recipient_business_system_id
     act = 'SYST_ID'
     msg = 'Business System needs to be filled' ).





  ENDMETHOD.
  METHOD analyze_chp_1.
    DATA: lt_change_pointer     TYPE mdg_cp_t_cp,
          lt_act_change_objects TYPE tdt_sdsls_drf_sales_order,
          lt_exp_change_objects TYPE tdt_sdsls_drf_sales_order.
    TEST-INJECTION seam_get_factory.
      so_drf_factory = NEW ldcl_drf_factory1142(  ).
    end-test-injection.
    TEST-INJECTION seam_message_provider.
    end-test-injection.

    DATA(lo_drf_proc) = NEW ZCL_OM_SAP_ECOM_ORD_UPD( is_runtime_parameter = VALUE #(  appl = 'SD_UNITEST'  )  ).

    ldcl_drf_factory1142=>so_drf_instance = lo_drf_proc.

    lt_change_pointer = VALUE #( (
      created_on =  '20181009161201'
      object_key = '0123456789C    01' ) (
      created_on =  '20181009161203'
      object_key = '0123456788C    03' )
      (
      created_on =  '20181009161204'
      object_key = '0123456789C    02' ) )
      .

    lt_exp_change_objects = VALUE #( (
       vbeln = '0123456789'
       vbklt = ''
       vbtyp = 'C'
       action_code = '02') (
       vbeln = '0123456788'
       vbklt = ''
       vbtyp = 'C'
       action_code = '03') ).


    TRY.
        lo_drf_proc->if_drf_outbound~analyze_changes_by_mdg_cp(
          EXPORTING
            it_change_pointer  = lt_change_pointer
          CHANGING
            ct_changed_objects = lt_act_change_objects  ).
      CATCH cx_root.
        cl_aunit_assert=>fail(
          EXPORTING
            msg    = 'Exception raised!' ).
    ENDTRY.

    cl_aunit_assert=>assert_equals(
      exp = lt_exp_change_objects
      act = lt_act_change_objects
      msg = 'Wrong change objects determined' ).


  ENDMETHOD.

  METHOD initialize_1.

    TEST-INJECTION seam_get_factory.
      so_drf_factory = NEW ldcl_drf_factory1142(  ).
    end-test-injection.
    TEST-INJECTION seam_message_provider.
    end-test-injection.

    DATA(lo_drf_proc) = NEW ZCL_OM_SAP_ECOM_ORD_UPD( is_runtime_parameter = VALUE #(  appl = 'SD_UNITEST'  )  ).

    ldcl_drf_factory1142=>so_drf_instance = lo_drf_proc.
**  cast ldcl_drf_factory1142( ZCL_OM_SAP_ECOM_ORD_UPD=>so_drf_factory )->mo_drf_instance = lo_drf_proc.

    ZCL_OM_SAP_ECOM_ORD_UPD=>if_drf_outbound~initialize(
      EXPORTING
        is_runtime_param          = VALUE #( appl = 'SD_UNITEST'  )
      IMPORTING
        eo_if_drf_outbound        = DATA(lo_eo_drf_outbound)
        es_runtime_param_out_impl = DATA(ls_param)
    ).

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp                  = lo_drf_proc
        act                  = lo_eo_drf_outbound
        msg                  = 'Wrong isntance returned' ).

    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp                  = VALUE drf_s_runtime_param_out_impl( table_type_name = 'TDT_SDSLS_DRF_SALES_ORDER')
        act                  = ls_param ).

  ENDMETHOD.

  METHOD setup.

  ENDMETHOD.

  METHOD teardown.

  ENDMETHOD.

ENDCLASS.
