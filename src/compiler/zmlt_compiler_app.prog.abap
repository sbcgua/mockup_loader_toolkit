**********************************************************************
* APP
**********************************************************************

class lcl_app definition final.
  public section.

    constants c_xlfile_mask type string value '*.xlsx'.
    constants c_src_files_meta_path type string value '.meta/src_files'.

    methods constructor
      importing
        iv_dir      type string
        iv_include  type string optional
        iv_mime_key type string
        iv_rebuild  type abap_bool default abap_false
        iv_do_watch type abap_bool default abap_false
      raising lcx_error zcx_w3mime_error.

    methods run
      raising lcx_error zcx_w3mime_error.

    methods handle_changed for event changed of zcl_w3mime_poller importing changed_list.
    methods handle_error   for event error of zcl_w3mime_poller importing error_text.

  private section.

    types:
      begin of ty_src_timestamp,
        type      type char1,
        src_file  type string,
        timestamp type char14,
      end of ty_src_timestamp,
      tt_src_timestamp type standard table of ty_src_timestamp with key type src_file.

    data:
          mv_do_watch    type abap_bool,
          mo_meta        type ref to lcl_meta,
          mo_poller      type ref to zcl_w3mime_poller,
          mo_zip         type ref to zcl_w3mime_zip_writer,
          mv_dir         type string,
          mv_include_dir type string,
          mv_rebuild     type abap_bool,
          mv_mime_key    type wwwdata-objid,
          mt_inc_dirs    type string_table.

    methods process_excel
      importing
        iv_path type string
      returning
        value(rv_updated) type abap_bool
      raising lcx_error zcx_w3mime_error.

    methods process_include
      importing
        iv_path type string
      returning
        value(rv_updated) type abap_bool
      raising lcx_error zcx_w3mime_error.

    methods update_mime_object
      importing iv_update_mime_meta type abap_bool default abap_false
      raising lcx_error zcx_w3mime_error.

    methods process_excel_dir
      raising lcx_error zcx_w3mime_error.

    methods process_includes_dir
      importing
        iv_inc_dir type string
      returning value(rv_num) type i
      raising lcx_error zcx_w3mime_error.

    methods write_meta
      raising lcx_error zcx_w3mime_error.
    methods read_meta
      raising lcx_error zcx_w3mime_error.
    methods start_watcher
      raising lcx_error zcx_w3mime_error.

    methods process_changed_file
      importing
        is_changed_file type zcl_w3mime_poller=>ty_file_state
      returning
        value(rv_processed_filename) type string
      raising
        lcx_error
        zcx_w3mime_error.

endclass.

class lcl_app implementation.

  method constructor.
    if iv_mime_key is initial.
      lcx_error=>raise( 'iv_mime_key must be specified' ). "#EC NOTEXT
    endif.

    if iv_dir is initial.
      lcx_error=>raise( 'iv_dir must be specified' ). "#EC NOTEXT
    endif.

    if cl_gui_frontend_services=>directory_exist( iv_dir ) = abap_false.
      lcx_error=>raise( 'source dir does not exist' ). "#EC NOTEXT
    endif.

    if iv_include is not initial and cl_gui_frontend_services=>directory_exist( iv_include ) = abap_false.
      lcx_error=>raise( 'include dir does not exist' ). "#EC NOTEXT
    endif.

    if iv_include is not initial and zcl_w3mime_fs=>path_is_relative( iv_to = iv_dir iv_from = iv_include ) = abap_true.
      lcx_error=>raise( 'iv_dir cannot be relevant to iv_include' ). "#EC NOTEXT
    endif.

    data lo_zip type ref to cl_abap_zip.
    if iv_rebuild = abap_false.
      try.
        create object lo_zip.
        lo_zip->load(
          exporting
            zip = zcl_w3mime_storage=>read_object_x( |{ iv_mime_key }| )
          exceptions others = 4 ).
      catch zcx_w3mime_error.
        sy-subrc = 4. " Ignore errors
      endtry.
    endif.

    create object mo_zip exporting io_zip = lo_zip.
    if lo_zip is bound.
      read_meta( ).
    else.
      mo_meta = lcl_meta=>create( ).
    endif.

    mv_mime_key    = iv_mime_key.
    mv_dir         = zcl_w3mime_fs=>path_ensure_dir_tail( iv_dir ).
    mv_include_dir = zcl_w3mime_fs=>path_ensure_dir_tail( iv_include ).
    mv_do_watch    = iv_do_watch.
    mv_rebuild     = iv_rebuild.

  endmethod.

  method run.
    write: / 'Processing directory:', mv_dir. "#EC NOTEXT
    process_excel_dir( ).

    data lv_num_files type i.
    if mv_include_dir is not initial.
      write: /.
      write: / 'Processing includes...'. "#EC NOTEXT
      lv_num_files = process_includes_dir( mv_include_dir ).
      write: /3 'Found (changed):', lv_num_files.
    endif.

    write: /.
    if mo_zip->is_dirty( ) = abap_true.
      write_meta( ).
      update_mime_object( mv_rebuild ).
      write: / 'MIME object updated:', mv_mime_key. "#EC NOTEXT
    else.
      write: / 'Changes not detected, update skipped'. "#EC NOTEXT
    endif.

    if mv_do_watch = abap_true.
      uline.
      write: / 'Start polling ...'. "#EC NOTEXT
      start_watcher( ).
    endif.

  endmethod.

  method process_excel_dir.
    data lt_files type zcl_w3mime_fs=>tt_files.
    data lv_path type string.
    field-symbols <f> like line of lt_files.

    lt_files = zcl_w3mime_fs=>read_dir(
      iv_dir    = mv_dir
      iv_filter = c_xlfile_mask ).

    data lv_num_files type i.
    lv_num_files = lines( lt_files ).

    loop at lt_files assigning <f>.
      if lcl_utils=>is_tempfile( |{ <f>-filename }| ) = abap_true.
        lv_num_files = lv_num_files - 1.
        continue.
      endif.
      lv_path = mv_dir && <f>-filename.

      cl_progress_indicator=>progress_indicate(
        i_text               = | Processing XL { sy-tabix } / { lv_num_files }: { <f>-filename }| "#EC NOTEXT
        i_processed          = sy-tabix
        i_total              = lv_num_files
        i_output_immediately = abap_true ).

      data lv_updated   type abap_bool.
      lv_updated = process_excel( lv_path ).

      write: /3 <f>-filename.
      if lv_updated = abap_false.
        write at 50 '<skipped, meta timestamp unchanged>'. "#EC NOTEXT
        continue.
      endif.
    endloop.
  endmethod.

  method process_excel.

    data lv_blob type xstring.
    data lt_mocks type lcl_workbook_parser=>tt_mocks.
    data lv_folder_name type string.
    data lv_filename type string.
    data li_excel type ref to lif_excel.


    lv_blob  = zcl_w3mime_fs=>read_file_x( iv_path ).
*    li_excel = lcl_excel_abap2xlsx=>load( lv_blob ).
    li_excel = lcl_excel_xlreader=>load( lv_blob ).
    lt_mocks = lcl_workbook_parser=>parse( li_excel ).
    lv_folder_name = lcl_utils=>get_uppercase_filename( iv_path ).
    lv_filename    = lcl_utils=>get_full_filename( iv_path ).

    rv_updated = mo_meta->update(
      iv_type      = lcl_meta=>c_type-excel
      iv_filename  = lv_filename
      iv_blob      = lv_blob ).
*      iv_timestamp = is_changed_file-timestamp ).
    if rv_updated = abap_false.
      return.
    endif.

    field-symbols <mock> like line of lt_mocks.
    loop at lt_mocks assigning <mock>.
      <mock>-name = lv_folder_name && '/' && to_lower( <mock>-name ) && '.txt'.
      mo_zip->add(
        iv_filename = <mock>-name
        iv_data     = <mock>-data ).
    endloop.

  endmethod.  " process_excel.

  method process_include.

    if zcl_w3mime_fs=>path_is_relative( iv_to = iv_path iv_from = mv_include_dir ) <> abap_true.
      lcx_error=>raise( 'Unexpected include path' ).
    endif.

    data lv_blob type xstring.
    data lv_relative_path type string.

    lv_blob = zcl_w3mime_fs=>read_file_x( iv_path ).
    lv_relative_path = zcl_w3mime_fs=>path_relative(
      iv_from = mv_include_dir
      iv_to   = iv_path ).

    rv_updated = mo_meta->update(
      iv_type      = lcl_meta=>c_type-include
      iv_filename  = lv_relative_path
      iv_blob      = lv_blob ).
*      iv_timestamp = is_changed_file-timestamp ).
    if rv_updated = abap_false.
      return.
    endif.

    lv_relative_path = replace(
      val = lv_relative_path
      sub = zcl_w3mime_fs=>c_sep
      with = '/'
      occ = 0 ).

    mo_zip->addx(
      iv_filename = lv_relative_path
      iv_xdata    = lv_blob  ).

  endmethod.

  method process_includes_dir.
    data lt_files type zcl_w3mime_fs=>tt_files.
    data lv_subdir type string.
    field-symbols <f> like line of lt_files.

    append iv_inc_dir to mt_inc_dirs. " remember for future watch targets

    lv_subdir  = zcl_w3mime_fs=>path_relative( iv_from = mv_include_dir iv_to = iv_inc_dir ).
    if lv_subdir is not initial.
      write /3 lv_subdir.
    endif.
    cl_progress_indicator=>progress_indicate(
      i_text               = | Processing includes: { lv_subdir }| "#EC NOTEXT
      i_processed          = 1
      i_total              = 1
      i_output_immediately = abap_true ).

    lt_files = zcl_w3mime_fs=>read_dir( iv_inc_dir ).
    loop at lt_files assigning <f>.
      data lv_full_path type string.
      lv_full_path = zcl_w3mime_fs=>path_join(
        iv_p1 = iv_inc_dir
        iv_p2 = |{ <f>-filename }| ).

      if <f>-isdir is initial.
        " TODO count skipped
        data lv_updated type abap_bool.
        lv_updated = process_include( lv_full_path ).
        if lv_updated = abap_true.
          rv_num = rv_num + 1.
        endif.
      else.
        rv_num = rv_num + process_includes_dir( lv_full_path ).
      endif.
    endloop.

  endmethod.  " process_includes_dir.

  method update_mime_object.
    data lv_blob type xstring.
    lv_blob = mo_zip->get_blob( ).

    if iv_update_mime_meta = abap_true.
      zcl_w3mime_storage=>update_object_meta(
        iv_key       = mv_mime_key
        iv_filename  = 'mockup-compiler-build.zip'
        iv_extension = '.zip'
        iv_mime_type = 'application/zip' ).
    endif.

    zcl_w3mime_storage=>update_object_x(
      iv_key  = mv_mime_key
      iv_data = lv_blob ).
  endmethod.  " update_mime_object.

  method process_changed_file.

    data lv_filename type string.
    lv_filename = lcl_utils=>get_full_filename( is_changed_file-path ).

    if lcl_utils=>is_tempfile( lv_filename ) = abap_true.
      return.
    endif.

    data lv_is_include type abap_bool.
    lv_is_include = zcl_w3mime_fs=>path_is_relative(
      iv_to   = is_changed_file-path
      iv_from = mv_include_dir ).

    if mv_include_dir is not initial and lv_is_include = abap_true.
      process_include( is_changed_file-path ).
    else.
      process_excel( is_changed_file-path ).
    endif.

    rv_processed_filename = lv_filename.

  endmethod.

  method handle_changed.
    data lx           type ref to cx_static_check.
    data l_msg        type string.
    data lt_filenames type string_table.
    data lv_filename  type string.
    field-symbols <i> like line of changed_list.

    try.
      loop at changed_list assigning <i>.
        lv_filename = process_changed_file( <i> ).
        if lv_filename is not initial.
          append lv_filename to lt_filenames.
        endif.
      endloop.

      if lines( lt_filenames ) = 0.
        return.
      endif.

      write_meta( ).
      update_mime_object( ).

      " Report result
      l_msg = |{ lcl_utils=>fmt_dt( <i>-timestamp ) }: { concat_lines_of( table = lt_filenames sep = ', ' ) }|.

    catch lcx_error zcx_w3mime_error into lx.
      l_msg = lx->get_text( ).
      message l_msg type 'E'.
      l_msg = |Error: { l_msg }|.
    endtry.

    write / l_msg.

  endmethod.  "handle_changed

  method handle_error.
    message error_text type 'E'.
  endmethod.  " handle_error.

  method write_meta.
    data l_tmp type string.
    l_tmp = mo_meta->serialize( ).
    mo_zip->add(
      iv_filename = c_src_files_meta_path
      iv_data     = l_tmp ).
  endmethod.

  method read_meta.
    data l_str type string.
    try.
      l_str = mo_zip->read( c_src_files_meta_path ).
    catch zcx_w3mime_error.
      " Ignore errors
    endtry.
    mo_meta = lcl_meta=>create( l_str ).
  endmethod.

  method start_watcher.

    data lt_targets type zcl_w3mime_poller=>tt_target.
    field-symbols: <ft> like line of lt_targets.

    append initial line to lt_targets assigning <ft>.
    <ft>-directory = mv_dir.
    <ft>-filter    = c_xlfile_mask.

    field-symbols <i> like line of mt_inc_dirs.
    loop at mt_inc_dirs assigning <i>.
      append initial line to lt_targets assigning <ft>.
      <ft>-directory = <i>.
      <ft>-filter    = '*.*'.
    endloop.

    create object mo_poller
      exporting
        it_targets  = lt_targets
        iv_interval = 1.
    set handler me->handle_changed for mo_poller.
    set handler me->handle_error for mo_poller.

    mo_poller->start( ).

  endmethod.

endclass.
