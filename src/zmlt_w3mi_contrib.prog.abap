class ZCX_W3MIME_ERROR definition
*  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

  public section.

    interfaces IF_T100_MESSAGE .

    constants:
      begin of ZCX_W3MIME_ERROR,
        msgid type symsgid value 'SY',
        msgno type symsgno value '499',
        attr1 type scx_attrname value 'MSG',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of ZCX_W3MIME_ERROR .
    data MSG type STRING read-only .

    methods CONSTRUCTOR
      importing
        !TEXTID like IF_T100_MESSAGE=>T100KEY optional
        !PREVIOUS like PREVIOUS optional
        !MSG type STRING optional .
    class-methods RAISE
      importing
        !MSG type STRING
      raising
        ZCX_W3MIME_ERROR .
  protected section.
  private section.
ENDCLASS.



CLASS ZCX_W3MIME_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
    SUPER->CONSTRUCTOR( PREVIOUS = PREVIOUS ).
    me->MSG = MSG .
    clear me->textid.
    if textid is initial.
      IF_T100_MESSAGE~T100KEY = ZCX_W3MIME_ERROR .
    else.
      IF_T100_MESSAGE~T100KEY = TEXTID.
    endif.
  endmethod.


  method raise.
    raise exception type zcx_w3mime_error
      exporting
        textid = zcx_w3mime_error
        msg    = msg.
  endmethod.
ENDCLASS.


class ZCL_W3MIME_FS definition
*  public
  final
  create public .

  public section.

    types:
      tt_files type standard table of file_info with key filename .

    class-data C_SEP type CHAR1 read-only .

    class-methods CHOOSE_DIR_DIALOG
      returning
        value(RV_PATH) type CHAR255 .
    class-methods CHOOSE_FILE_DIALOG
      returning
        value(RV_PATH) type CHAR255 .
    class-methods READ_FILE
      importing
        !IV_FILENAME type STRING
      exporting
        !ET_DATA type LVC_T_MIME
        !EV_SIZE type I
      raising
        ZCX_W3MIME_ERROR .
    class-methods WRITE_FILE
      importing
        !IV_FILENAME type STRING
        !IV_SIZE type I
      changing
        !CT_DATA type LVC_T_MIME
      raising
        ZCX_W3MIME_ERROR .
    class-methods READ_FILE_X
      importing
        !IV_FILENAME type STRING
      returning
        value(RV_DATA) type XSTRING
      raising
        ZCX_W3MIME_ERROR .
    class-methods WRITE_FILE_X
      importing
        !IV_FILENAME type STRING
        !IV_DATA type XSTRING
      raising
        ZCX_W3MIME_ERROR .
    class-methods PARSE_PATH
      importing
        value(IV_PATH) type STRING
      exporting
        value(EV_DIRECTORY) type STRING
        value(EV_FILENAME) type STRING
        value(EV_EXTENSION) type STRING .
    class-methods RESOLVE_FILENAME
      importing
        value(IV_PATH) type STRING
      exporting
        value(EV_FILENAME) type STRING
        value(EV_DIRECTORY) type STRING
      raising
        ZCX_W3MIME_ERROR .
    class-methods READ_DIR
      importing
        !IV_DIR type STRING
        !IV_FILTER type STRING default '*.*'
      returning
        value(RT_FILES) type TT_FILES
      raising
        ZCX_W3MIME_ERROR .
    class-methods PATH_JOIN
      importing
        !IV_P1 type STRING
        !IV_P2 type STRING
      returning
        value(RT_JOINED) type STRING .
    class-methods PATH_IS_RELATIVE
      importing
        !IV_TO type STRING
        !IV_FROM type STRING
      returning
        value(RV_YES) type ABAP_BOOL .
    class-methods PATH_RELATIVE
      importing
        !IV_FROM type STRING
        !IV_TO type STRING
      returning
        value(RV_PATH) type STRING .
    class-methods CLASS_CONSTRUCTOR .
    class-methods PATH_ENSURE_DIR_TAIL
      importing
        !I_PATH type STRING
      returning
        value(R_PATH) type STRING .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_W3MIME_FS IMPLEMENTATION.


  method choose_dir_dialog.
    data l_str type string.

    cl_gui_frontend_services=>directory_browse(
      changing
        selected_folder      = l_str
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        others               = 4 ).

    if sy-subrc is initial.
      rv_path = l_str.
    endif.

  endmethod.


  method choose_file_dialog.
    data:
          lt_files type filetable,
          lv_rc    type i,
          lv_uact  type i.

    field-symbols <file> like line of lt_files.

    cl_gui_frontend_services=>file_open_dialog(
      changing
        file_table  = lt_files
        rc          = lv_rc
        user_action = lv_uact
      exceptions others = 4 ).

    if sy-subrc > 0 OR lv_uact <> cl_gui_frontend_services=>action_ok.
      return. " Empty value
    endif.

    read table lt_files assigning <file> index 1.
    if sy-subrc = 0.
      rv_path = <file>-filename.
    endif.

  endmethod.


  method class_constructor.
    cl_gui_frontend_services=>get_file_separator( changing file_separator = c_sep exceptions others = 4 ).
    if sy-subrc is not initial.
      c_sep = '\'. " Assume windows (eclipse ???)
    endif.
  endmethod.


  method parse_path.
    data:
          lv_offs type i.

    clear: ev_filename, ev_extension, ev_directory.
    if strlen( iv_path ) = 0.
      return.
    endif.

    find first occurrence of c_sep in reverse( iv_path ) match offset lv_offs.

    if sy-subrc = 0.
      lv_offs      = strlen( iv_path ) - lv_offs.
      ev_directory = substring( val = iv_path len = lv_offs ).
      ev_filename  = substring( val = iv_path off = lv_offs ).
    else.
      ev_filename  = iv_path.
    endif.

    find first occurrence of '.' in reverse( ev_filename ) match offset lv_offs.

    if sy-subrc = 0.
      lv_offs      = strlen( ev_filename ) - lv_offs - 1.
      ev_extension = substring( val = ev_filename off = lv_offs ).
      ev_filename  = substring( val = ev_filename len = lv_offs ).
    endif.

  endmethod. "#EC CI_VALPAR


  method path_ensure_dir_tail.
    r_path = i_path.
    if r_path is not initial and substring( val = r_path off = strlen( r_path ) - 1 len = 1 ) <> c_sep.
      r_path = r_path && c_sep.
    endif.
  endmethod.


  method path_is_relative.
    " Does not support .. at the moment
    data l_offs type i.
    l_offs = find( val = iv_to sub = iv_from ).
    rv_yes = boolc( l_offs = 0 ). " Match, starts from start
  endmethod.


  method path_join.
    " Does not support .. at the moment

    if iv_p1 is not initial.
      rt_joined = iv_p1.
    endif.

    if iv_p2 is not initial.
      if rt_joined is not initial and substring( val = rt_joined off = strlen( rt_joined ) - 1 len = 1 ) <> c_sep.
        rt_joined = rt_joined && c_sep.
      endif.

      rt_joined = rt_joined && iv_p2.
    endif.

  endmethod.


  method path_relative.
    " Does not support .. at the moment
    if path_is_relative( iv_to = iv_to iv_from = iv_from ) = abap_true.
      rv_path = substring( val = iv_to off = strlen( iv_from ) ).
      if strlen( rv_path ) > 0 and rv_path+0(1) = c_sep.
        shift rv_path left by 1 places.
      endif.
    else.
      rv_path = iv_to. " Hmmm, right just for abs path, think how to do it properly
    endif.
  endmethod.


  method read_dir.
    data:
          lv_cnt   type i.

    cl_gui_frontend_services=>directory_list_files(
      exporting directory  = iv_dir
                filter     = iv_filter
      changing  file_table = rt_files
                count      = lv_cnt
      exceptions others    = 4 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( 'Cannot read directory' ). "#EC NOTEXT
    endif.

  endmethod.  " read_dir.


  method read_file.
    clear: et_data, ev_size.

    cl_gui_frontend_services=>gui_upload(
      exporting
        filename   = iv_filename
        filetype   = 'BIN'
      importing
        filelength = ev_size
      changing
        data_tab   = et_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot read file' ). "#EC NOTEXT
    endif.

  endmethod.  " read_file.


  method read_file_x.
    data:
          lt_data type lvc_t_mime,
          lv_size type i.

    read_file(
      exporting
        iv_filename = iv_filename
      importing
        et_data = lt_data
        ev_size = lv_size ).

    call function 'SCMS_BINARY_TO_XSTRING'
      exporting
        input_length = lv_size
      importing
        buffer       = rv_data
      tables
        binary_tab   = lt_data.

  endmethod.  " read_file_x.


  method resolve_filename.
    data:
          lv_sep       type c,
          lv_extension type string.

    parse_path(
      exporting
        iv_path = iv_path
      importing
        ev_directory = ev_directory
        ev_filename  = ev_filename
        ev_extension = lv_extension ).

    ev_filename = ev_filename && lv_extension.
    if strlen( ev_filename ) = 0.
      zcx_w3mime_error=>raise( 'Cannot resolve filename' ). "#EC NOTEXT
    endif.

    if ev_directory is initial.
      cl_gui_frontend_services=>get_sapgui_workdir( changing sapworkdir = ev_directory exceptions others = 4 ).
      cl_gui_cfw=>flush( ).
      if sy-subrc is initial. " hmmm ? eclipse ?
        ev_directory = ev_directory && c_sep.
      else.
        ev_directory = 'c:\tmp\'. " TODO refactor, hack for eclipse unit test
      endif.
    endif.

  endmethod.  "#EC CI_VALPAR


  method write_file.

    cl_gui_frontend_services=>gui_download(
      exporting
        filename     = iv_filename
        filetype     = 'BIN'
        bin_filesize = iv_size
      changing
        data_tab   = ct_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot write file' ). "#EC NOTEXT
    endif.

  endmethod.  " write_file.


  method write_file_x.

    data:
          lt_data type lvc_t_mime,
          lv_size type i.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = iv_data
      importing
        output_length = lv_size
      tables
        binary_tab    = lt_data.

    write_file(
      exporting
        iv_filename = iv_filename
        iv_size = lv_size
      changing
        ct_data = lt_data ).

  endmethod.  " write_file_x.
ENDCLASS.


class ZCL_W3MIME_POLLER definition
*  public
  final
  create public .

  public section.

    types:
      begin of ty_target,
        directory type string,
        filter    type string,
      end of ty_target .
    types:
      tt_target type standard table of ty_target with default key .
    types:
      begin of ty_file_state,
        path      type string,
        isdir     type abap_bool, " for future use
        timestamp type char14,
      end of ty_file_state .
    types:
      tt_file_state type standard table of ty_file_state with key path .

    events CHANGED
      exporting
        value(CHANGED_LIST) type TT_FILE_STATE .
    events ERROR
      exporting
        value(ERROR_TEXT) type STRING .

    methods CONSTRUCTOR
      importing
        !IT_TARGETS type TT_TARGET
        !IV_INTERVAL type I default 1 " 1 second
      raising
        ZCX_W3MIME_ERROR .
    methods START
      raising
        ZCX_W3MIME_ERROR .
    methods HANDLE_TIMER
      for event FINISHED of CL_GUI_TIMER .
    methods READ_CURRENT_STATE
      returning
        value(RT_STATE) type TT_FILE_STATE
      raising
        ZCX_W3MIME_ERROR .
    methods UPDATE_STATE
      returning
        value(RT_CHANGES) type TT_FILE_STATE
      raising
        ZCX_W3MIME_ERROR .
    class-methods DETECT_CHANGES
      importing
        !IT_PREV type TT_FILE_STATE
        !IT_CUR type TT_FILE_STATE
      returning
        value(RT_CHANGES) type TT_FILE_STATE .
    class-methods MERGE_CHANGES
      importing
        !IT_CHANGES type TT_FILE_STATE
      changing
        !CT_STATE type TT_FILE_STATE .
  protected section.
  private section.
    data mt_targets    type tt_target.
    data mt_file_state type tt_file_state.
    data mo_timer      type ref to cl_gui_timer.
ENDCLASS.



CLASS ZCL_W3MIME_POLLER IMPLEMENTATION.


  method constructor.
    if lines( it_targets ) = 0.
      zcx_w3mime_error=>raise( 'Specify poll targets' ). "#EC NOTEXT
    endif.

    data lv_sep type c.
    cl_gui_frontend_services=>get_file_separator( changing file_separator = lv_sep ).
    mt_targets = it_targets.

    field-symbols: <t> like line of mt_targets.
    loop at mt_targets assigning <t>.
      if <t>-directory is initial.
        zcx_w3mime_error=>raise( 'Target directory cannot be empty' ). "#EC NOTEXT
      endif.
      if substring( val = <t>-directory off = strlen( <t>-directory ) - 1 len = 1 ) <> lv_sep.
        <t>-directory = <t>-directory && lv_sep.
      endif.
      if <t>-filter is initial.
        <t>-filter = '*.*'.
      endif.
    endloop.

    create object mo_timer.
    set handler me->handle_timer for mo_timer.
    mo_timer->interval = iv_interval.

  endmethod.  " constructor.


  method detect_changes.
    " TODO detect deletions ! file in prev but not in cur
    field-symbols <c> like line of it_cur.
    field-symbols <p> like line of it_cur.

    " assuming both inputs are sorted
    loop at it_cur assigning <c>.
      read table it_prev assigning <p> with key path = <c>-path binary search.
      if sy-subrc is not initial or <c>-timestamp <> <p>-timestamp.
        append <c> to rt_changes.
      endif.
    endloop.
  endmethod.  " detect_changes.


  method handle_timer.
    data:
          lx type ref to zcx_w3mime_error,
          lv_text type string,
          lt_changes like mt_file_state.

    try.
      lt_changes = update_state( ).
    catch zcx_w3mime_error into lx.
      lv_text = lx->get_text( ).
      raise event error exporting error_text = lv_text.
    endtry.

    if lines( lt_changes ) > 0.
      raise event changed exporting changed_list = lt_changes.
    endif.

    mo_timer->run( ).

  endmethod.  "handle_timer


  method merge_changes.
    if lines( it_changes ) = 0.
      return.
    endif.
    append lines of it_changes to ct_state.
    sort ct_state by path ascending timestamp descending.
    delete adjacent duplicates from ct_state comparing path.
  endmethod.  " merge_changes.


  method read_current_state.

    data lt_files type standard table of file_info.
    field-symbols:
                  <state> like line of rt_state,
                  <t> like line of mt_targets.

    loop at mt_targets assigning <t>.
      lt_files = zcl_w3mime_fs=>read_dir(
        iv_dir    = <t>-directory
        iv_filter = <t>-filter ).

      field-symbols <file> like line of lt_files.
      loop at lt_files assigning <file> where isdir is initial.
        append initial line to rt_state assigning <state>.
        <state>-path = <t>-directory && <file>-filename.
        concatenate <file>-writedate <file>-writetime into <state>-timestamp. " Hmmm
*      cl_abap_tstmp=>systemtstmp_syst2utc(
*        exporting
*          syst_date = <file>-writedate
*          syst_time = <file>-writetime
*        importing
*          utc_tstmp = <state>-timestamp ).
      endloop.
    endloop.

    sort rt_state by path.

  endmethod.  " read_current_state.


  method start.
    mt_file_state = read_current_state( ). " Sorted
    mo_timer->run( ).
  endmethod.  "start.


  method update_state.
    data: lt_state like mt_file_state.

    lt_state = read_current_state( ). " Sorted

    rt_changes = detect_changes(
      it_prev = mt_file_state
      it_cur  = lt_state ).

    merge_changes(
      exporting
        it_changes = rt_changes
      changing
        ct_state = mt_file_state ).

  endmethod.  " update_state.
ENDCLASS.


class ZCL_W3MIME_STORAGE definition
*  public
  final
  create public .

  public section.

    class-methods CHECK_OBJ_EXISTS
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      returning
        value(RV_YES) type ABAP_BOOL .
    class-methods READ_OBJECT
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      exporting
        !ET_DATA type LVC_T_MIME
        !EV_SIZE type I
      raising
        ZCX_W3MIME_ERROR .
    class-methods UPDATE_OBJECT
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
        !IT_DATA type LVC_T_MIME
        !IV_SIZE type I
      raising
        ZCX_W3MIME_ERROR .
    class-methods GET_OBJECT_INFO
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      returning
        value(RS_OBJECT) type WWWDATATAB
      raising
        ZCX_W3MIME_ERROR .
    class-methods READ_OBJECT_X
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      returning
        value(RV_DATA) type XSTRING
      raising
        ZCX_W3MIME_ERROR .
    class-methods UPDATE_OBJECT_X
      importing
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
        !IV_DATA type XSTRING
      raising
        ZCX_W3MIME_ERROR .
    class-methods CHOOSE_MIME_DIALOG
      returning
        value(RV_OBJ_NAME) type SHVALUE_D .
    class-methods READ_OBJECT_SINGLE_META
      importing
        !IV_PARAM type W3_NAME
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      returning
        value(RV_VALUE) type W3_QVALUE
      raising
        ZCX_W3MIME_ERROR .
    class-methods UPDATE_OBJECT_META
      importing
        !IV_FILENAME type W3_QVALUE optional
        !IV_EXTENSION type W3_QVALUE optional
        !IV_MIME_TYPE type W3_QVALUE optional
        !IV_VERSION type W3_QVALUE optional
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      raising
        ZCX_W3MIME_ERROR .
    class-methods UPDATE_OBJECT_SINGLE_META
      importing
        !IV_PARAM type W3_NAME
        !IV_VALUE type W3_QVALUE
        !IV_KEY type WWWDATA-OBJID
        !IV_TYPE type WWWDATA-RELID default 'MI'
      raising
        ZCX_W3MIME_ERROR .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_W3MIME_STORAGE IMPLEMENTATION.


  method check_obj_exists.

    data dummy type wwwdata-relid.

    select single relid into dummy
      from wwwdata
      where relid = iv_type
      and   objid = iv_key
      and   srtf2 = 0.

    rv_yes = boolc( sy-subrc = 0 ).

  endmethod.  " check_obj_exists.


  method choose_mime_dialog.

    types:
      begin of t_w3head,
        objid type wwwdata-objid,
        text  type wwwdata-text,
      end of t_w3head.

    data:
          ls_return type ddshretval,
          lt_data   type standard table of t_w3head,
          lt_return type standard table of ddshretval.

    select distinct objid text from wwwdata
      into corresponding fields of table lt_data
      where relid = 'MI'
      and   objid like 'Z%'
      order by objid.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'OBJID'
        value_org       = 'S'
      tables
        value_tab       = lt_data
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    if sy-subrc is not initial.
      return. " Empty value
    endif.

    read table lt_return into ls_return index 1. " fail is ok => empty return
    rv_obj_name = ls_return-fieldval.

  endmethod.


  method get_object_info.

    select single * into corresponding fields of rs_object
      from wwwdata
      where relid = iv_type
      and   objid = iv_key
      and   srtf2 = 0.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot read W3xx info' ). "#EC NOTEXT
    endif.

  endmethod.  " get_object_info.


  method read_object.

    data: lv_value  type w3_qvalue,
          ls_object type wwwdatatab.

    clear: et_data, ev_size.

    call function 'WWWPARAMS_READ'
      exporting
        relid = iv_type
        objid = iv_key
        name  = 'filesize'
      importing
        value = lv_value
      exceptions
        others = 1.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot read W3xx filesize parameter' ). "#EC NOTEXT
    endif.

    ev_size         = lv_value.
    ls_object-relid = iv_type.
    ls_object-objid = iv_key.

    call function 'WWWDATA_IMPORT'
      exporting
        key               = ls_object
      tables
        mime              = et_data
      exceptions
        wrong_object_type = 1
        import_error      = 2.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
    endif.

  endmethod.  " read_object.


  method READ_OBJECT_SINGLE_META.

    assert iv_type = 'MI' or iv_type = 'HT'.

    call function 'WWWPARAMS_READ'
      exporting
        relid = iv_type
        objid = iv_key
        name  = iv_param
      importing
        value = rv_value
      exceptions
        others = 1.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( |Cannot read W3xx metadata: { iv_param }| ). "#EC NOTEXT
    endif.

  endmethod.


  method read_object_x.
    data:
          lt_data type lvc_t_mime,
          lv_size type i.

    read_object(
      exporting
        iv_key  = iv_key
        iv_type = iv_type
      importing
        et_data = lt_data
        ev_size = lv_size ).

    call function 'SCMS_BINARY_TO_XSTRING'
      exporting
        input_length = lv_size
      importing
        buffer       = rv_data
      tables
        binary_tab   = lt_data.

  endmethod.  " read_object_x.


  method update_object.

    data: lv_temp   type wwwparams-value,
          ls_object type wwwdatatab.

    " update file size
    lv_temp = iv_size.
    condense lv_temp.
    update_object_single_meta(
      iv_type  = iv_type
      iv_key   = iv_key
      iv_param = 'filesize'
      iv_value = lv_temp ).

    " update version
    try .
      lv_temp = read_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'version' ).

      if lv_temp is not initial and strlen( lv_temp ) = 5 and lv_temp+0(5) co '1234567890'.
        data lv_version type numc_5.
        lv_version = lv_temp.
        lv_version = lv_version + 1.
        lv_temp    = lv_version.
        update_object_single_meta(
          iv_type  = iv_type
          iv_key   = iv_key
          iv_param = 'version'
          iv_value = lv_temp ).
      endif.

    catch zcx_w3mime_error.
      " ignore errors
      clear lv_temp.
    endtry.

    " update data
    ls_object = get_object_info( iv_key = iv_key iv_type = iv_type ).
    ls_object-chname = sy-uname.
    ls_object-tdate  = sy-datum.
    ls_object-ttime  = sy-uzeit.

    call function 'WWWDATA_EXPORT'
      exporting
        key               = ls_object
      tables
        mime              = it_data
      exceptions
        wrong_object_type = 1
        export_error      = 2.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
    endif.

  endmethod.  " update_object.


  method UPDATE_OBJECT_META.

    data: ls_param  type wwwparams,
          ls_object type wwwdatatab.

    ls_param-relid = iv_type.
    ls_param-objid = iv_key.

    if iv_filename is supplied.
      update_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'filename'
        iv_value = iv_filename ).
    endif.

    if iv_extension is supplied.
      update_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'fileextension'
        iv_value = iv_extension ).
    endif.

    if iv_mime_type is supplied.
      update_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'mimetype'
        iv_value = iv_mime_type ).
    endif.

    if iv_version is supplied.
      update_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'version'
        iv_value = iv_version ).
    endif.

  endmethod.


  method update_object_single_meta.

    data: ls_param  type wwwparams,
          ls_object type wwwdatatab.

    assert iv_type = 'MI' or iv_type = 'HT'.

    ls_param-relid = iv_type.
    ls_param-objid = iv_key.
    ls_param-name  = iv_param.
    ls_param-value = iv_value.

    call function 'WWWPARAMS_MODIFY_SINGLE'
      exporting
        params = ls_param
      exceptions
        others = 1.

    if sy-subrc > 0.
      zcx_w3mime_error=>raise( |Cannot update W3xx metadata { iv_param }| ). "#EC NOTEXT
    endif.

  endmethod.


  method update_object_x.
    data:
      lt_data type lvc_t_mime,
      lv_size type i.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = iv_data
      importing
        output_length = lv_size
      tables
        binary_tab    = lt_data.

    update_object(
        iv_key  = iv_key
        iv_type = iv_type
        iv_size = lv_size
        it_data = lt_data ).

  endmethod.  " update_object_x.
ENDCLASS.


class ZCL_W3MIME_ZIP_WRITER definition
*  public
  final
  create public .

  public section.

    type-pools ABAP .
    methods CONSTRUCTOR
      importing
        !IO_ZIP type ref to CL_ABAP_ZIP optional
        !IV_ENCODING type ABAP_ENCODING optional .
    methods ADD
      importing
        !IV_FILENAME type STRING
        !IV_DATA type STRING .
    methods ADDX
      importing
        !IV_FILENAME type STRING
        !IV_XDATA type XSTRING .
    methods GET_BLOB
      returning
        value(RV_BLOB) type XSTRING .
    methods READ
      importing
        !IV_FILENAME type STRING
      returning
        value(RV_DATA) type STRING
      raising
        ZCX_W3MIME_ERROR .
    methods READX
      importing
        !IV_FILENAME type STRING
      returning
        value(RV_XDATA) type XSTRING
      raising
        ZCX_W3MIME_ERROR .
    methods HAS
      importing
        !IV_FILENAME type STRING
      returning
        value(R_YES) type ABAP_BOOL .
    methods IS_DIRTY
      returning
        value(R_YES) type ABAP_BOOL .
    methods DELETE
      importing
        !IV_FILENAME type STRING
      raising
        ZCX_W3MIME_ERROR .
  protected section.
  private section.

    data MV_IS_DIRTY type ABAP_BOOL .
    data MO_ZIP type ref to CL_ABAP_ZIP .
    data MO_CONV_OUT type ref to CL_ABAP_CONV_OUT_CE .
    data MO_CONV_IN type ref to CL_ABAP_CONV_IN_CE .
    type-pools ABAP .
    data MV_ENCODING type ABAP_ENCODING .
ENDCLASS.



CLASS ZCL_W3MIME_ZIP_WRITER IMPLEMENTATION.


  method add.
    data lv_xdata type xstring.
    mo_conv_out->convert(
      exporting data = iv_data
      importing buffer = lv_xdata ).

    addx(
      iv_filename = iv_filename
      iv_xdata    = lv_xdata ).
  endmethod.  " add.


  method addx.
    mo_zip->delete(
      exporting
        name = iv_filename
      exceptions others = 1 ). " ignore exceptions

    mo_zip->add( name = iv_filename content = iv_xdata ).
    mv_is_dirty = abap_true.
  endmethod.  " addx.


  method constructor.
    if io_zip is bound.
      mo_zip = io_zip.
    else.
      create object mo_zip.
    endif.

    if iv_encoding is not initial.
      mv_encoding = iv_encoding.
    else.
      mv_encoding = '4110'. " UTF8
    endif.

    mo_conv_out = cl_abap_conv_out_ce=>create( encoding = mv_encoding ).
    mo_conv_in  = cl_abap_conv_in_ce=>create( encoding = mv_encoding ).
  endmethod.  " constructor.


  method delete.
    mo_zip->delete( exporting name = iv_filename exceptions others = 4 ).
    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( 'delete failed' ).
    endif.
    mv_is_dirty = abap_true.
  endmethod.


  method get_blob.
    rv_blob = mo_zip->save( ).
    mv_is_dirty = abap_false.
  endmethod.  " get_blob


  method HAS.
    read table mo_zip->files with key name = iv_filename transporting no fields.
    r_yes = boolc( sy-subrc is initial ).
  endmethod.


  method is_dirty.
    r_yes = mv_is_dirty.
  endmethod.


  method READ.
    data:
          lv_xdata type xstring,
          lx       type ref to cx_root.

    lv_xdata = readx( iv_filename ).

    try.
      mo_conv_in->convert( exporting input = lv_xdata importing data = rv_data ).
    catch cx_root into lx.
      zcx_w3mime_error=>raise( msg = 'Codepage conversion error' ). "#EC NOTEXT
    endtry.

  endmethod.


  method READX.

    mo_zip->get(
      exporting
        name    = iv_filename
      importing
        content = rv_xdata
      exceptions zip_index_error = 1 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( msg = |Cannot read { iv_filename }| ). "#EC NOTEXT
    endif.

    " Remove unicode signatures
    case mv_encoding.
      when '4110'. " UTF-8
        shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
      when '4103'. " UTF-16LE
        shift rv_xdata left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.
    endcase.

  endmethod.
ENDCLASS.
