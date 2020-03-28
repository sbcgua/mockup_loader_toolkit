**********************************************************************
* EDITOR STORAGE
**********************************************************************

class lcl_storage definition final.
  public section.

    methods constructor
      importing
        iv_obj_name type wwwdata-objid
      raising lcx_error.

    methods mock_list
      returning value(rt_list) type string_table.

    methods get_mock
      importing
        iv_path type string
      exporting
        er_mock   type ref to data
        et_fields type string_table
      raising lcx_error.

    methods get_mock_raw
      importing
        iv_path type string
      returning value(rv_data) type string
      raising lcx_error.

    methods delete_mock
      importing
        iv_path type string
      raising lcx_error.

    methods put_mock
      importing
        iv_path type string
        iv_mock type data
      raising lcx_error.

    methods put_mock_raw
      importing
        iv_path type string
        iv_mock type string
      raising lcx_error.

    methods commit
      raising lcx_error.

  private section.
    data mv_obj_name type wwwdata-objid.
    data mo_zip_writer type ref to zcl_w3mime_zip_writer.

endclass.

class lcl_storage implementation.

  method constructor.
    data lx type ref to zcx_w3mime_error.
    data lo_zip type ref to cl_abap_zip.

    mv_obj_name = iv_obj_name.

    try.
      create object lo_zip.
      lo_zip->load(
        exporting
          zip = zcl_w3mime_storage=>read_object_x( iv_obj_name )
        exceptions
          others = 4 ).

      create object mo_zip_writer
        exporting
          io_zip = lo_zip.

    catch zcx_w3mime_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@constructor' ).
    endtry.

  endmethod.

  method mock_list.
    rt_list = mo_zip_writer->list( ).
  endmethod.

  method get_mock.
    data lv_data type string.
    data lx type ref to cx_static_check.

    try.
      lv_data = mo_zip_writer->read( iv_path ).
      zcl_text2tab_parser=>create_typeless( )->parse(
        exporting
          i_data = lv_data
        importing
          e_container   = er_mock
          e_head_fields = et_fields ).

    catch zcx_w3mime_error zcx_text2tab_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@get_mock' ).
    endtry.

  endmethod.

  method get_mock_raw.
    data lx type ref to cx_static_check.
    try.
      rv_data = mo_zip_writer->read( iv_path ).
    catch zcx_w3mime_error zcx_text2tab_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@get_mock_raw' ).
    endtry.
  endmethod.

  method put_mock.
    data lv_data type string.
    data lx type ref to cx_static_check.

    try.
      lv_data = zcl_text2tab_serializer=>create( )->serialize( iv_mock ).
      mo_zip_writer->add(
        iv_filename = iv_path
        iv_data     = lv_data ).

    catch zcx_w3mime_error zcx_text2tab_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@put_mock' ).
    endtry.

  endmethod.  " put_mock.

  method put_mock_raw.
    data lx type ref to cx_static_check.
    try.
      mo_zip_writer->add(
        iv_filename = iv_path
        iv_data     = iv_mock ).

    catch zcx_w3mime_error zcx_text2tab_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@put_mock_raw' ).
    endtry.
  endmethod.  " put_mock.

  method commit.
    data lx type ref to zcx_w3mime_error.

    try.
      zcl_w3mime_storage=>update_object_x(
        iv_key  = mv_obj_name
        iv_data = mo_zip_writer->get_blob( ) ).

    catch zcx_w3mime_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@commit' ).
    endtry.
  endmethod.  " commit.

  method delete_mock.
    data lx type ref to cx_static_check.

    try.
      mo_zip_writer->delete( iv_filename = iv_path ).
    catch zcx_w3mime_error zcx_text2tab_error into lx.
      lcx_error=>raise( msg = lx->get_text( ) loc = '@delete_mock' ).
    endtry.

  endmethod.

endclass.
