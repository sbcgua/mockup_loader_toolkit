class lcl_meta definition final create private.
  public section.

    types ty_file_type type c length 1.

    constants:
      begin of c_type,
        excel type ty_file_type value 'X',
        include type ty_file_type value 'I',
      end of c_type.

    types:
      begin of ty_src_timestamp,
        type      type ty_file_type,
        src_file  type string,
        timestamp type char14,
        sha1      type hash160,
      end of ty_src_timestamp,
      tt_src_timestamp type standard table of ty_src_timestamp with key type src_file.

    class-methods create
      importing
        iv_str type string optional
      returning
        value(ro_meta) type ref to lcl_meta.

    methods serialize
      returning
        value(rv_str) type string
      raising
        lcx_error.

    methods update
      importing
        iv_type type ty_file_type
        iv_filename type string
        iv_timestamp type zcl_w3mime_poller=>ty_file_state-timestamp optional
        iv_blob type xstring
      returning
        value(rv_has_changed) type abap_bool
      raising
        lcx_error.

  private section.
    data mt_src_ts type tt_src_timestamp.


endclass.

class lcl_meta implementation.

  method create.
    create object ro_meta.
    if iv_str is initial.
      return.
    endif.

    try.
      zcl_text2tab_parser=>create( ro_meta->mt_src_ts )->parse(
        exporting
          i_data        = iv_str
          i_strict      = abap_false
        importing
          e_container   = ro_meta->mt_src_ts ).
    catch zcx_text2tab_error.
      return. " Ignore errors
    endtry.
  endmethod.

  method serialize.
    data lx type ref to zcx_text2tab_error.
    try.
      sort mt_src_ts by type src_file.
      rv_str = zcl_text2tab_serializer=>create( )->serialize( mt_src_ts ).
    catch zcx_text2tab_error into lx.
      lcx_error=>raise( |Meta serialization failed: { lx->get_text( ) }| ).
    endtry.
  endmethod.

  method update.
    field-symbols <m> like line of mt_src_ts.
    data lv_sha1 like <m>-sha1.

    lv_sha1 = lcl_utils=>sha1( iv_blob ).

    read table mt_src_ts assigning <m>
      with key
        type     = iv_type
        src_file = iv_filename .
    if sy-subrc is not initial. " new file
      append initial line to mt_src_ts assigning <m>.
      <m>-type     = iv_type.
      <m>-src_file = iv_filename.
    endif.

*    if <m>-timestamp <> iv_timestamp.
    if <m>-sha1 <> lv_sha1.
      <m>-timestamp = iv_timestamp.
      <m>-sha1      = lv_sha1.
      rv_has_changed = abap_true.
    endif.
  endmethod.

endclass.
