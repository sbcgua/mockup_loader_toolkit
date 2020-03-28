class lcl_app definition final create private.
  public section.

    constants:
      begin of routes,
        index type char8 value 'INDEX',
        mock  type char8 value 'MOCK',
        text  type char8 value 'TEXT',
      end of routes.

    types:
      begin of ty_route,
        route type char8,
        param type string,
      end of ty_route,
      tt_route type standard table of ty_route.

    class-methods start
      importing
        iv_mime_key type w3objid
      raising
        lcx_error.
    class-methods on_screen_output.

    methods run
      raising
        lcx_error.

    methods handle_mock_selected
      for event mock_selected of lcl_content_view
      importing mock_name.

    methods handle_mock_delete
      for event request_mock_delete of lcl_content_view
      importing mock_name.

    methods handle_text_save
      for event on_save of lcl_text_view.

    class-methods on_user_command
      importing
        iv_cmd type char70
      returning
        value(rv_processed) type abap_bool.

  private section.
    class-data go_instance type ref to lcl_app.

    data mo_storage type ref to lcl_storage.
    data mt_route_queue type tt_route.
    data mo_current_view type ref to lcl_view_base.
    data mv_mime_key type w3objid.

    methods constructor
      importing
        iv_mime_key type w3objid
      raising
        lcx_error.

    methods queue_loop
      raising
        lcx_error.

    methods enqueue
      importing
        iv_route type char8
        iv_param type string optional
        is_next  type abap_bool optional.

    methods show_index
      raising
        lcx_error.

    methods show_mock
      importing
        iv_param type string
      raising
        lcx_error.

    methods show_text
      importing
        iv_param type string
      raising
        lcx_error.

endclass.

class lcl_app implementation.

  method on_user_command.
    assert go_instance is bound.

    if go_instance->mo_current_view is bound.
      rv_processed = go_instance->mo_current_view->on_user_command( iv_cmd ).
    endif.
  endmethod.

  method start.
    create object go_instance
      exporting
        iv_mime_key = iv_mime_key.
    go_instance->run( ).
  endmethod.

  method on_screen_output.
    go_instance->mo_current_view->on_output( ).
  endmethod.

  method constructor.
    mv_mime_key = iv_mime_key.
    create object mo_storage
      exporting
        iv_obj_name = iv_mime_key.
  endmethod.

  method run.
    enqueue( routes-index ).
    queue_loop( ).
  endmethod.

  method queue_loop.
    data ls_route like line of mt_route_queue.

    do.
      read table mt_route_queue into ls_route index 1.
      if sy-subrc is not initial. " No more routes equeued
        exit.
      endif.
      delete mt_route_queue index 1.

      " TODO refactor this shit
      case ls_route-route.
        when routes-index.
          show_index( ).
        when routes-mock.
          try.
            show_mock( ls_route-param ).
          catch lcx_error.
            " probably parsing error, try displaying as text
            enqueue(
              iv_route = routes-text
              iv_param = ls_route-param
              is_next  = abap_true ).
            continue.
          endtry.
        when routes-text.
          show_text( ls_route-param ).
        when others.
          message 'Unknown route' type 'E'.
      endcase.

      if ls_route-route <> routes-index and lines( mt_route_queue ) = 0.
        enqueue( routes-index ). " go back to index from other views
      endif.
    enddo.

  endmethod.

  method show_index.
    data lt_mock_list type string_table.
    lt_mock_list = mo_storage->mock_list( ).

    data lo_content_view type ref to lcl_content_view.
    create object lo_content_view
      exporting
        iv_mock_name = |{ mv_mime_key }|
        it_contents = lt_mock_list.

    mo_current_view ?= lo_content_view.
    set handler handle_mock_selected for lo_content_view.
    set handler handle_mock_delete for lo_content_view.
    lo_content_view->display( ).
    set handler handle_mock_selected for lo_content_view activation ''.
    set handler handle_mock_delete for lo_content_view activation ''.
    free lo_content_view.
    clear mo_current_view.
  endmethod.

  method show_mock.
    data lr_mock type ref to data.
    mo_storage->get_mock(
      exporting
        iv_path = iv_param
      importing
        er_mock = lr_mock ).

    data lo_mock_view type ref to lcl_mock_view.
    create object lo_mock_view
      exporting
        iv_mock_name = iv_param
        ir_mock      = lr_mock.

    mo_current_view ?= lo_mock_view.
    lo_mock_view->display( ).
    free lo_mock_view.
    free lr_mock.
    clear mo_current_view.
  endmethod.

  method show_text.
    data lv_raw_mock type string.
    lv_raw_mock = mo_storage->get_mock_raw( iv_param ).

    data lo_text_view type ref to lcl_text_view.
    create object lo_text_view
      exporting
        iv_mock_name = iv_param
        iv_data      = lv_raw_mock.

    mo_current_view ?= lo_text_view.
    set handler handle_text_save for lo_text_view.
    lo_text_view->display( ).
    set handler handle_text_save for lo_text_view activation ''.
    free lo_text_view.
    clear mo_current_view.
  endmethod.

  method handle_mock_selected.
    enqueue(
      iv_route = routes-mock
      iv_param = mock_name ).
    mo_current_view->close( ).
  endmethod.

  method enqueue.
    data lv_route like line of mt_route_queue.
    lv_route-route = iv_route.
    lv_route-param = iv_param.
    if is_next = abap_true.
      insert lv_route into mt_route_queue index 1.
    else.
      append lv_route to mt_route_queue.
    endif.
  endmethod.

  method handle_mock_delete.

*    data lx type ref to lcx_error.
*    data msg type string.
*    try.
*      mo_storage->delete_mock( mock_name ).
*    catch lcx_error into lx.
*      msg = lx->get_text( ).
*      message msg type 'E'.
*    endtry.
*
*    data lo_content_view type ref to lcl_content_view.
*    lo_content_view ?= mo_current_view.
*    lo_content_view->update_data( mo_storage->mock_list( ) ).

  endmethod.

  method handle_text_save.

*    data lo_text_view type ref to lcl_text_view.
*    lo_text_view ?= mo_current_view.
*
*    data lx type ref to lcx_error.
*    data msg type string.
*    try.
*      mo_storage->put_mock_raw(
*        iv_path = lo_text_view->mv_mock_name
*        iv_mock = lo_text_view->mv_data ).
*    catch lcx_error into lx.
*      msg = lx->get_text( ).
*      message msg type 'E'.
*    endtry.

  endmethod.

endclass.
