**********************************************************************
* lcl_salv_agent, lcl_salv_enabler
* code based on Naimesh Patel (aka zevolving) example
* http://zevolving.com/2015/06/salv-table-21-editable-with-single-custom-method/

class lcl_salv_agent definition inheriting from cl_salv_model_base.
  public section.
    class-methods get_grid
      importing
        io_salv_model type ref to cl_salv_model
      returning
        value(ro_gui_alv_grid) type ref to cl_gui_alv_grid
      raising
        cx_salv_msg.
endclass.

class lcl_salv_agent implementation.
  method get_grid.
    data:
      lo_grid_adap type ref to cl_salv_grid_adapter,
      lo_fs_adap   type ref to cl_salv_fullscreen_adapter,
      lo_root      type ref to cx_root.

    try.
      lo_grid_adap ?= io_salv_model->r_controller->r_adapter.
    catch cx_root into lo_root.
      try. "could be fullscreen adaptper
        lo_fs_adap ?= io_salv_model->r_controller->r_adapter.
      catch cx_root into lo_root.
        raise exception type cx_salv_msg
          exporting
            previous = lo_root
            msgid    = '00'
            msgno    = '001'
            msgty    = 'E'
            msgv1    = 'Check PREVIOUS exception'.
      endtry.
    endtry.

    if lo_grid_adap is not initial.
      ro_gui_alv_grid = lo_grid_adap->get_grid( ).
    elseif lo_fs_adap is not initial.
      ro_gui_alv_grid = lo_fs_adap->get_grid( ).
    else.
      raise exception type cx_salv_msg
        exporting
          msgid = '00'
          msgno = '001'
          msgty = 'W'
          msgv1 = 'Adapter is not bound yet'.
    endif.
  endmethod.

endclass.

class lcl_salv_enabler definition final.

  public section.

    types:
      begin of ty_tracked,
        salv type ref to cl_salv_table,
        buttons type ttb_button,
      end of ty_tracked.

    class-methods toggle_toolbar
      importing
        io_salv    type ref to cl_salv_table
        it_buttons type ttb_button.

    class-methods get_grid
      importing
        io_salv_model type ref to cl_salv_model
      returning
        value(ro_grid) type ref to cl_gui_alv_grid
      raising
        cx_salv_error.

    methods:
      on_after_refresh
        for event after_refresh of cl_gui_alv_grid
        importing sender,
      on_toolbar
        for event toolbar of cl_gui_alv_grid
        importing e_object e_interactive sender.

  private section.
    data mt_tracked_salv type standard table of ty_tracked.
    class-data go_event_handler type ref to object.

endclass.

class lcl_salv_enabler implementation.

  method get_grid.
    data lo_error type ref to cx_salv_msg.
    if io_salv_model->model ne if_salv_c_model=>table.
      raise exception type cx_salv_msg
        exporting
          msgid = '00'
          msgno = '001'
          msgty = 'E'
          msgv1 = 'Incorrect SALV Type'.
    endif.
    ro_grid = lcl_salv_agent=>get_grid( io_salv_model ).
  endmethod.

  method toggle_toolbar.

    if lcl_salv_enabler=>go_event_handler is not bound.
      create object lcl_salv_enabler=>go_event_handler type lcl_salv_enabler.
    endif.

    data lo_event_handler type ref to lcl_salv_enabler.
    lo_event_handler ?= lcl_salv_enabler=>go_event_handler.

    if lines( lo_event_handler->mt_tracked_salv ) = 0.
      set handler lo_event_handler->on_after_refresh
        for all instances
        activation 'X'.
      set handler lo_event_handler->on_toolbar
        for all instances
        activation 'X'.
    endif.

    field-symbols <t> like line of lo_event_handler->mt_tracked_salv.
    append initial line to lo_event_handler->mt_tracked_salv assigning <t>.
    <t>-salv    = io_salv.
    <t>-buttons = it_buttons.

  endmethod.

  method on_after_refresh.
    data lo_grid   type ref to cl_gui_alv_grid.
    data ls_layout type lvc_s_layo.
    data lv_idx  type i.
    field-symbols <t> like line of mt_tracked_salv.

    try.
      loop at mt_tracked_salv assigning <t>.
        lv_idx = sy-tabix.
        lo_grid = lcl_salv_enabler=>get_grid( <t>-salv ).
        check lo_grid eq sender.
        lo_grid->get_frontend_layout( importing es_layout = ls_layout ).
        ls_layout-no_toolbar = ''.
        lo_grid->set_frontend_layout( ls_layout ).
        delete mt_tracked_salv index lv_idx.
      endloop.
    catch cx_salv_error.
      return.
    endtry.

    if lines( mt_tracked_salv ) = 0.
      set handler me->on_after_refresh " deregister the event handler
        for all instances
        activation space.
      set handler me->on_toolbar " deregister the event handler
        for all instances
        activation space.
    endif.

  endmethod.

  method on_toolbar.

    data lo_grid type ref to cl_gui_alv_grid.
    field-symbols <t> like line of mt_tracked_salv.

    try.
      loop at mt_tracked_salv assigning <t>.
        lo_grid = lcl_salv_enabler=>get_grid( <t>-salv ).
        if lo_grid eq sender.
          append lines of <t>-buttons to e_object->mt_toolbar.
          exit.
        endif.
      endloop.
    catch cx_salv_msg cx_salv_error.
      return.
    endtry.

  endmethod.

endclass.
