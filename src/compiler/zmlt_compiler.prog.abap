report zmlt_compiler.

**********************************************************************
* INCLUDES
**********************************************************************

include zmlt_w3mi_contrib.
include zmlt_xlrd_contrib.
include zmlt_compiler_utils.
include zmlt_compiler_meta.
include zmlt_compiler_workbook_prs.
include zmlt_compiler_app.

**********************************************************************
* ENTRY POINT
**********************************************************************
constants:
  GC_DIR_PARAM_NAME TYPE CHAR20 VALUE 'ZMLT_COMPILER_DIR',
  GC_INC_PARAM_NAME TYPE CHAR20 VALUE 'ZMLT_COMPILER_INC',
  GC_OBJ_PARAM_NAME TYPE CHAR20 VALUE 'ZMLT_MIME_OBJ'.

form main using pv_srcdir pv_incdir pv_mimename pv_watch pv_rebuild.
  data lo_app type ref to lcl_app.
  data lx type ref to cx_static_check.
  data l_str type string.

  if pv_srcdir is initial.
    message 'Source directory parameter is mandatory' type 'S' display like 'E'. "#EC NOTEXT
  endif.

  if pv_mimename is initial.
    message 'MIME object name is mandatory' type 'S' display like 'E'. "#EC NOTEXT
  endif.

  try.
    create object lo_app
      exporting
        iv_dir      = |{ pv_srcdir }|
        iv_include  = |{ pv_incdir }|
        iv_mime_key = |{ pv_mimename }|
        iv_rebuild  = pv_rebuild
        iv_do_watch = pv_watch.
    lo_app->run( ).

  catch zcx_mlt_error zcx_w3mime_error into lx.
    l_str = lx->get_text( ).
    message l_str type 'E'.
  endtry.

endform.

form f4_include_path changing c_path type char255.
  c_path = zcl_w3mime_fs=>choose_dir_dialog( ).
  if c_path is not initial.
    set parameter id GC_INC_PARAM_NAME field c_path.
  endif.
endform.

form f4_srcdir_path changing c_path type char255.
  c_path = zcl_w3mime_fs=>choose_dir_dialog( ).
  if c_path is not initial.
    set parameter id GC_DIR_PARAM_NAME field c_path.
  endif.
endform.

form f4_mime_path changing c_path.
  c_path = zcl_w3mime_storage=>choose_mime_dialog( ).
  if c_path is not initial.
    set parameter id GC_OBJ_PARAM_NAME field c_path.
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

selection-screen begin of line.
selection-screen comment (24) t_dir for field p_dir.
parameters p_dir type char255 visible length 40.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) t_inc for field p_inc.
parameters p_inc type char255 visible length 40.
selection-screen comment (24) t_inc2.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) t_watch for field p_watch.
parameters p_watch type abap_bool as checkbox.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) t_rebld for field p_rebld.
parameters p_rebld type abap_bool as checkbox.
selection-screen comment (40) t_rebld2.
selection-screen end of line.

selection-screen end of block b1.

initialization.
  txt_b1   = 'Compile parameters'.        "#EC NOTEXT
  t_dir    = 'Source directory'.          "#EC NOTEXT
  t_inc    = 'Includes directory'.        "#EC NOTEXT
  t_inc2   = '  (optional)'.              "#EC NOTEXT
  t_mime   = 'Target W3MI object'.        "#EC NOTEXT
  t_mime2  = '  (must be existing one)'.  "#EC NOTEXT
  t_watch  = 'Keep watching source'.      "#EC NOTEXT
  t_rebld  = 'Re-build from scratch'.     "#EC NOTEXT
  t_rebld2 = '  (Ignore metadata)'.       "#EC NOTEXT

  get parameter id GC_DIR_PARAM_NAME field p_dir.
  get parameter id GC_INC_PARAM_NAME field p_inc.
  get parameter id GC_OBJ_PARAM_NAME field p_mime.

at selection-screen on value-request for p_dir.
  perform f4_srcdir_path changing p_dir.

at selection-screen on value-request for p_mime.
  perform f4_mime_path changing p_mime.

at selection-screen on value-request for p_inc.
  perform f4_include_path changing p_inc.

at selection-screen on p_dir.
  if p_dir is not initial.
    set parameter id GC_DIR_PARAM_NAME field p_dir.
  endif.

at selection-screen on p_inc.
  if p_inc is not initial.
    set parameter id GC_INC_PARAM_NAME field p_inc.
  endif.

at selection-screen on p_mime.
  if p_mime is not initial.
    set parameter id GC_OBJ_PARAM_NAME field p_mime.
  endif.

start-of-selection.
  perform main using p_dir p_inc p_mime p_watch p_rebld.
