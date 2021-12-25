class ltcl_test_errors definition final
  for testing
  duration short
  risk level harmless.

  private section.

    methods raise_error for testing.

endclass.

class ltcl_test_errors implementation.

  method raise_error.

    data lx type ref to zcx_mlt_error.
    data lv_msg type string.

    lv_msg = repeat( val = 'a' occ = 60 ).

    try.
      zcx_mlt_error=>raise(
        msg = lv_msg
        loc = 'some_loc'
        rc  = 'XYZ' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mlt_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->msg
        exp = lv_msg ).
      cl_abap_unit_assert=>assert_equals(
        act = lx->loc
        exp = 'some_loc' ).
      cl_abap_unit_assert=>assert_equals(
        act = lx->rc
        exp = 'XYZ' ).
      cl_abap_unit_assert=>assert_equals(
        act = lx->get_text( )
        exp = |{ lv_msg } @some_loc| ).
    endtry.

  endmethod.

endclass.
