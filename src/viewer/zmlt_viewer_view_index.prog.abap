class lcl_content_view definition final inheriting from lcl_view_base.
  public section.
    types:
      begin of ty_mock_index,
        folder type string,
        name   type string,
      end of ty_mock_index,
      tt_mock_index type standard table of ty_mock_index with default key.

    methods constructor
      importing
        iv_mock_name type string
        it_contents type string_table.

    methods display redefinition.
    methods on_user_command redefinition.

    methods handle_double_click
      for event double_click of cl_salv_events_table
      importing row column.

    methods on_alv_user_command
      for event added_function of cl_salv_events
      importing e_salv_function.

    methods update_data
      importing
        it_contents type string_table.

    events mock_selected exporting value(mock_name) type string.
    events request_mock_delete exporting value(mock_name) type string.

  private section.
    data mt_mock_index type tt_mock_index.
    data mv_mock_name type string.

    class-methods convert_content
      importing
        it_contents type string_table
      returning
        value(rt_mocks) type tt_mock_index.

endclass.

class lcl_content_view implementation.

  method convert_content.
    field-symbols <i> like line of it_contents.
    field-symbols <mock> like line of rt_mocks.
    loop at it_contents assigning <i>.
      append initial line to rt_mocks assigning <mock>.
      split <i> at '/' into <mock>-folder <mock>-name.
    endloop.
  endmethod.

  method constructor.
    super->constructor( ).
    mt_mock_index = convert_content( it_contents ).
    mv_mock_name = iv_mock_name.
  endmethod.

  method display.
    data lx_alv type ref to cx_salv_error.

    try.
      cl_salv_table=>factory(
        importing
          r_salv_table = mo_alv
        changing
          t_table      = mt_mock_index ).
    catch cx_salv_msg into lx_alv.
      write / 'Error creating SALV'.
    endtry.

    set_columns( mo_alv ).

    data lo_functions type ref to cl_salv_functions_list.
    lo_functions = mo_alv->get_functions( ).
    lo_functions->set_default( abap_true ).
    mo_alv->set_screen_status(
      report = sy-cprog
      pfstatus = 'INDEX_VIEW' ).

    data lo_display type ref to cl_salv_display_settings.
    lo_display = mo_alv->get_display_settings( ).
    lo_display->set_striped_pattern( 'X' ).
    lo_display->set_list_header( |Mockup viewer - { mv_mock_name } index| ).

    data lo_event type ref to cl_salv_events_table.
    lo_event = mo_alv->get_event( ).
    set handler handle_double_click for lo_event.
    set handler on_alv_user_command for lo_event.

    data lo_selections type ref to cl_salv_selections.
    lo_selections = mo_alv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    data lo_sorts type ref to cl_salv_sorts.
    lo_sorts = mo_alv->get_sorts( ).

    try.
      lo_sorts->add_sort( 'FOLDER' ).
      lo_sorts->add_sort( 'NAME' ).
    catch cx_salv_error into lx_alv.
      lcx_error=>raise( lx_alv->get_text( ) ).
    endtry.

    mo_alv->display( ).

  endmethod.

  method on_alv_user_command.
    on_user_command( e_salv_function ).
  endmethod.

  method on_user_command.
    data lo_selections type ref to cl_salv_selections.
    lo_selections = mo_alv->get_selections( ).

    data lt_rows type salv_t_row.
    data lv_row like line of lt_rows.
    lt_rows = lo_selections->get_selected_rows( ).

*    case iv_cmd.
*      when '%DEL'.
*        read table lt_rows index 1 into lv_row.
*        if sy-subrc is initial. " TODO improve
*          data lv_path type string.
*          field-symbols <mock> like line of mt_mock_index.
*          read table mt_mock_index assigning <mock> index lv_row.
*          assert sy-subrc is initial.
*          lv_path = <mock>-folder && '/' && <mock>-name.
*          raise event request_mock_delete exporting mock_name = lv_path.
*        endif.
*
*      when others.
*    endcase.

  endmethod.

  method handle_double_click.
    data lv_path type string.
    field-symbols <mock> like line of mt_mock_index.

    read table mt_mock_index assigning <mock> index row.
    assert sy-subrc is initial.

    lv_path = <mock>-folder && '/' && <mock>-name.
    raise event mock_selected exporting mock_name = lv_path.
  endmethod.

  method update_data.
    mt_mock_index = convert_content( it_contents ).
    mo_alv->refresh( ).
  endmethod.

endclass.
