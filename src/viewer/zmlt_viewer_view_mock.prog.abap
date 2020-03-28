class lcl_mock_view definition final inheriting from lcl_view_base.
  public section.

    methods constructor
      importing
        iv_mock_name type string
        ir_mock type ref to data.

    methods display redefinition.
    methods on_user_command redefinition.

    methods on_alv_user_command
      for event added_function of cl_salv_events
      importing e_salv_function.

    data mv_mock_name type string read-only.

  private section.
    data mr_mock type ref to data.

endclass.

class lcl_mock_view implementation.

  method constructor.
    super->constructor( ).
    mr_mock = ir_mock.
    mv_mock_name = iv_mock_name.
  endmethod.

  method display.
    data lx_alv type ref to cx_salv_msg.
    field-symbols <tab> type any table.
    assign mr_mock->* to <tab>.

    try.
      cl_salv_table=>factory(
        importing
          r_salv_table = mo_alv
        changing
          t_table      = <tab> ).
    catch cx_salv_msg into lx_alv.
      write / 'Error creating SALV @view_mock'.
    endtry.

    set_columns( mo_alv ).

    data: lo_functions type ref to cl_salv_functions_list.
    lo_functions = mo_alv->get_functions( ).
    lo_functions->set_default( abap_true ).
    mo_alv->set_screen_status(
      report = sy-cprog
      pfstatus = 'MOCK_VIEW' ).

    data: lo_display type ref to cl_salv_display_settings.
    lo_display = mo_alv->get_display_settings( ).
    lo_display->set_striped_pattern( 'X' ).
    lo_display->set_list_header( |Mockup viewer - { mv_mock_name }| ).

    data lo_event type ref to cl_salv_events_table.
    lo_event = mo_alv->get_event( ).
    set handler on_alv_user_command for lo_event.

    data: lo_selections type ref to cl_salv_selections.
    lo_selections = mo_alv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

    mo_alv->display( ).
  endmethod.

  method on_alv_user_command.
    on_user_command( e_salv_function ).
  endmethod.

  method on_user_command.

*    case iv_cmd.
*      when '%FREE_EDIT'.
*        lcl_salv_edit_enabler=>toggle_editable( mo_alv ).
*        mo_alv->refresh( ).
*    endcase.

  endmethod.

endclass.
