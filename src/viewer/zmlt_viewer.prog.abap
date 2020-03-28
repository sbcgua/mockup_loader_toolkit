report zmlt_viewer.

include zmlt_error.
include zmlt_w3mi_contrib.
include zmlt_viewer_model.

include zmlt_viewer_utils.
include zmlt_viewer_enabler.
include zmlt_viewer_view_base.
include zmlt_viewer_view_index.
include zmlt_viewer_view_mock.
include zmlt_viewer_view_text.

include zmlt_viewer_mock_app.

**********************************************************************
* ENTRY POINT
**********************************************************************
constants:
  gc_obj_param_name type char20 value 'ZMLT_MIME_OBJ'.

form main using pv_mimename type w3objid.
  data lx type ref to cx_static_check.
  data l_str type string.

  if pv_mimename is initial.
    message 'MIME object name is mandatory' type 'S' display like 'E'. "#EC NOTEXT
  endif.

  try.
    lcl_app=>start( iv_mime_key = pv_mimename ).
  catch lcx_error zcx_w3mime_error into lx.
    l_str = lx->get_text( ).
    message l_str type 'E'.
  endtry.

endform.

form f4_mime_path changing c_path type w3objid.
  c_path = zcl_w3mime_storage=>choose_mime_dialog( ).
  if c_path is not initial.
    set parameter id gc_obj_param_name field c_path.
  endif.
endform.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
selection-screen comment (24) t_mime for field p_mime.
parameters p_mime type w3objid visible length 40.
selection-screen comment (24) t_mime2.
selection-screen end of line.

selection-screen end of block b1.

**********************************************************************
* MODULES
**********************************************************************
module status_0100 output.
  lcl_app=>on_screen_output( ).
endmodule.

module user_command_0100 input.
  data lv_processed type abap_bool.
  case sy-ucomm.
    when 'BACK' or 'EXIT' or 'CANCEL'.
      lv_processed = lcl_app=>on_user_command( sy-ucomm ).
      if lv_processed = abap_false.
        leave to screen 0.
      endif.
    when others.
      lcl_app=>on_user_command( sy-ucomm ).
  endcase.
endmodule.

**********************************************************************
* ENTRY POINT
**********************************************************************

initialization.
  txt_b1   = 'Compile parameters'.        "#EC NOTEXT
  t_mime   = 'Target W3MI object'.        "#EC NOTEXT
  t_mime2  = '  (must be existing one)'.  "#EC NOTEXT

  get parameter id gc_obj_param_name field p_mime.

at selection-screen on value-request for p_mime.
  perform f4_mime_path changing p_mime.

at selection-screen on p_mime.
  if p_mime is not initial.
    set parameter id gc_obj_param_name field p_mime.
  endif.

start-of-selection.
  perform main using p_mime.
