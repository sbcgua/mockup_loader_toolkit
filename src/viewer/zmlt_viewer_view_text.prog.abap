class lcl_text_view definition final inheriting from lcl_view_base.
  public section.

    types: ty_w3_mime_tab type standard table of w3_mime with default key.

    methods constructor
      importing
        iv_mock_name type string
        iv_data type string.

    methods display redefinition.
    methods on_output redefinition.
    methods on_user_command redefinition.
    methods free_controls.

    data mv_data      type string read-only.
    data mv_mock_name type string read-only.

    events on_save.

  private section.
    data mv_initiated type abap_bool.
    data mv_as_xml    type abap_bool.
    data mo_text_ctl  type ref to cl_gui_textedit.
    data mo_html_ctl  type ref to cl_gui_html_viewer.

    methods save_if_modified.

    class-methods load_str_to_html_ctl
      importing
        io_html_ctl type ref to cl_gui_html_viewer
        iv_str      type string
      returning
        value(rv_url) type char256.
endclass.

class lcl_text_view implementation.

  method constructor.
    super->constructor( ).
    mv_data = iv_data.
    mv_mock_name = iv_mock_name.
  endmethod.

  method free_controls.
    if mo_text_ctl is bound.
      mo_text_ctl->free( ).
      free mo_text_ctl.
    endif.

    if mo_html_ctl is bound.
      mo_html_ctl->free( ).
      free mo_html_ctl.
    endif.
  endmethod.

  method display.
    call screen 100.
    free_controls( ).
  endmethod.

  method load_str_to_html_ctl.

    data lv_xstr  type xstring.
    data lt_xdata type ty_w3_mime_tab.
    data lv_size  type i.

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text   = iv_str
      importing
        buffer = lv_xstr
      exceptions
        others = 1.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = lv_xstr
      importing
        output_length = lv_size
      tables
        binary_tab    = lt_xdata.

    io_html_ctl->load_data(
      exporting
        size         = lv_size
      importing
        assigned_url = rv_url
      changing
        data_table   = lt_xdata
      exceptions
        others       = 1 ).

  endmethod.

  method on_output.

    if mv_initiated = abap_false.
      data title type string.
      title = |Mockup viewer - { mv_mock_name }|.
      set pf-status 'TEXT_VIEW'.
      set titlebar  'TEXT_VIEW' with title.
      mv_initiated = abap_true.
    endif.

    free_controls( ).
    if mv_as_xml = abap_true.
      create object mo_html_ctl
        exporting
          parent = cl_gui_container=>screen0.

      data lv_url   type char256.
      lv_url = load_str_to_html_ctl(
        io_html_ctl = mo_html_ctl
        iv_str = mv_data ).

      mo_html_ctl->show_url( lv_url ).

    else.
      create object mo_text_ctl
        exporting
          parent = cl_gui_container=>screen0.

      mo_text_ctl->set_font_fixed(  ).
      mo_text_ctl->set_textstream( mv_data ).
      mo_text_ctl->set_readonly_mode( ).
    endif.

  endmethod.

  method on_user_command.
    case iv_cmd.
      when 'AS_XML'.
*        mv_as_xml = boolc( mv_as_xml = abap_false ).
*        free_controls( ).
*        on_output( ).

        cl_abap_browser=>show_xml( xml_string = mv_data ).

        rv_processed = abap_true.

      when 'BACK' or 'EXIT'.
*        save_if_modified( ).

    endcase.
  endmethod.

  method save_if_modified.

    if mo_text_ctl is not bound.
      return.
    endif.

    data lv_is_modified type i.
    mo_text_ctl->get_textstream(
      exporting
        only_when_modified = 1
      importing
        text        = mv_data
        is_modified = lv_is_modified ).

    if lv_is_modified = 0.
      return.
    endif.

    data lv_answer type c.
    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = 'Unsaved data'
        text_question         = 'The data has changed, do you want to save?'
        text_button_1         = 'Save'
        icon_button_1         = 'ICON_SYSTEM_SAVE'
        text_button_2         = 'Don''t save'
        icon_button_2         = 'ICON_NO_STATUS'
        default_button        = 2
        display_cancel_button = abap_true
      importing
        answer                = lv_answer
      exceptions
        others                = 1.

    if sy-subrc is not initial or lv_answer = 'A' or lv_answer = '2'.
      return. " Abort
    elseif lv_answer = '1'.
      raise event on_save.
    endif.

  endmethod.

endclass.
