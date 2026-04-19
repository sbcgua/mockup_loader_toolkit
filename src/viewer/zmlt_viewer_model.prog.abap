**********************************************************************
* EDITOR STORAGE
**********************************************************************

class lcl_storage definition final.
  public section.

    methods constructor
      importing
        iv_obj_name type wwwdata-objid
      raising zcx_mlt_error.

    methods mock_list
      returning value(rt_list) type string_table.

    methods mock_format
      returning value(rv_format) type string.

    methods get_mock
      importing
        iv_path type string
      exporting
        er_mock   type ref to data
        et_fields type string_table
      raising zcx_mlt_error.

    methods get_mock_raw
      importing
        iv_path type string
      returning value(rv_data) type xstring
      raising zcx_mlt_error.

    methods delete_mock
      importing
        iv_path type string
      raising zcx_mlt_error.

    methods put_mock
      importing
        iv_path type string
        iv_mock type data
      raising zcx_mlt_error.

    methods put_mock_raw
      importing
        iv_path type string
        iv_mock type string
      raising zcx_mlt_error.

    methods commit
      raising zcx_mlt_error.

    class-methods xstr2str
      importing
        i_xstr type xstring
      returning
        value(r_str) type string
      raising zcx_mlt_error.

  private section.
    data mi_ml type ref to zif_mockup_loader.
*    data mv_obj_name type wwwdata-objid.
*    data mo_zip_writer type ref to zcl_w3mime_zip_writer.

endclass.

class lcl_storage implementation.

  method constructor.
    data lx type ref to zcx_mockup_loader_error.

    try.
      mi_ml = zcl_mockup_loader=>create(
        i_path = |{ iv_obj_name }|
        i_type = 'MIME' ).
    catch zcx_mockup_loader_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@constructor' ).
    endtry.

  endmethod.

  method mock_list.

    mi_ml->info( importing e_files = rt_list ).

  endmethod.

  method mock_format.

    mi_ml->info( importing e_src_format = rv_format ).

  endmethod.

  method xstr2str.

    data lv_xdata type xstring.

    lv_xdata = i_xstr.

    " Remove unicode signatures
    " Assume UTF-8, check also ZCL_MOCKUP_LOADER->READ_ZIP
    shift lv_xdata left deleting leading cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.

    try.
      data lo_conv type ref to cl_abap_conv_in_ce.
      lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
      lo_conv->convert(
        exporting
          input = lv_xdata
        importing
          data  = r_str ).
    catch cx_root.
      zcx_mlt_error=>raise( msg = 'Codepage conversion error' ). "#EC NOTEXT
    endtry.

  endmethod.


  method get_mock.

    data lv_xdata type xstring.
    data lv_data type string.
    data lx type ref to cx_static_check.

    try.
      lv_xdata = mi_ml->load_blob( iv_path ).

      lv_data = xstr2str( lv_xdata ).

      zcl_text2tab_parser=>create_typeless( )->parse(
        exporting
          i_data = lv_data
        importing
          e_container   = er_mock
          e_head_fields = et_fields ).

    catch zcx_mockup_loader_error zcx_text2tab_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@get_mock' ).
    endtry.

  endmethod.

  method get_mock_raw.
    data lx type ref to cx_static_check.
    try.
      rv_data = mi_ml->load_blob( iv_path ).
    catch zcx_mockup_loader_error zcx_text2tab_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@get_mock_raw' ).
    endtry.
  endmethod.

  method put_mock.
    data lv_data type string.
    data lx type ref to cx_static_check.

    zcx_mlt_error=>raise( msg = 'Unsupported for now' loc = '@put_mock' ).
    try.
*      lv_data = zcl_text2tab_serializer=>create( )->serialize( iv_mock ).
*      mo_zip_writer->add(
*        iv_filename = iv_path
*        iv_data     = lv_data ).

    catch zcx_w3mime_error zcx_text2tab_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@put_mock' ).
    endtry.

  endmethod.  " put_mock.

  method put_mock_raw.
    data lx type ref to cx_static_check.

    zcx_mlt_error=>raise( msg = 'Unsupported for now' loc = '@put_mock_raw' ).
    try.
*      mo_zip_writer->add(
*        iv_filename = iv_path
*        iv_data     = iv_mock ).

    catch zcx_w3mime_error zcx_text2tab_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@put_mock_raw' ).
    endtry.
  endmethod.  " put_mock.

  method commit.
    data lx type ref to zcx_w3mime_error.

    zcx_mlt_error=>raise( msg = 'Unsupported for now' loc = '@commit' ).
    try.
*      zcl_w3mime_storage=>update_object_x(
*        iv_key  = mv_obj_name
*        iv_data = mo_zip_writer->get_blob( ) ).

    catch zcx_w3mime_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@commit' ).
    endtry.
  endmethod.  " commit.

  method delete_mock.
    data lx type ref to cx_static_check.

    zcx_mlt_error=>raise( msg = 'Unsupported for now' loc = '@delete_mock' ).
    try.
*      mo_zip_writer->delete( iv_filename = iv_path ).
    catch zcx_w3mime_error zcx_text2tab_error into lx.
      zcx_mlt_error=>raise( msg = lx->get_text( ) loc = '@delete_mock' ).
    endtry.

  endmethod.

endclass.
