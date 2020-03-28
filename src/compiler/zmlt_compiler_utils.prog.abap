class lcl_utils definition final.
  public section.

    class-methods fmt_dt
      importing
        iv_ts         type zcl_w3mime_poller=>ty_file_state-timestamp
      returning
        value(rv_str) type string.

    class-methods is_tempfile
      importing
        iv_filename type string
      returning
        value(rv_yes) type abap_bool.

    class-methods sha1
      importing
        iv_data type xstring
      returning
        value(rv_hash) type hash160
      raising
        lcx_error.

    class-methods get_uppercase_filename
      importing
        iv_path type string
      returning
        value(rv_folder_name) type string.

    class-methods get_full_filename
      importing
        iv_path type string
      returning
        value(rv_filename) type string.

endclass.

class lcl_utils implementation.

  method get_full_filename.

    data l_filename type string.
    data l_ext type string.

    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = iv_path
      importing
        ev_filename  = l_filename
        ev_extension = l_ext ).

    rv_filename = l_filename && l_ext.

  endmethod.

  method fmt_dt.
    data ts type char14.
    ts = iv_ts.
    rv_str = |{ ts+0(4) }-{ ts+4(2) }-{ ts+6(2) } |
          && |{ ts+8(2) }:{ ts+10(2) }:{ ts+12(2) }|.
  endmethod.

  method is_tempfile.
    rv_yes = boolc( strlen( iv_filename ) >= 2 and substring( val = iv_filename len = 2 ) = '~$' ).
  endmethod.

  method sha1.

    data lv_hash type string.

    try .
      cl_abap_message_digest=>calculate_hash_for_raw(
        exporting
          if_algorithm = 'SHA1'
          if_data      = iv_data
        importing
          ef_hashstring = lv_hash ).
    catch cx_abap_message_digest.
      lcx_error=>raise( 'sha1 calculation error' ).
    endtry.

    rv_hash = to_lower( lv_hash ).

  endmethod.

  method get_uppercase_filename.
    zcl_w3mime_fs=>parse_path(
      exporting
        iv_path = iv_path
      importing
        ev_filename = rv_folder_name ).
    rv_folder_name = to_upper( rv_folder_name ).
  endmethod.

endclass.
