class lcl_excel_abap2xlsx definition final.
  public section.
    interfaces lif_excel.

    types:
      begin of ty_ws_item,
        title     type string,
        worksheet type ref to object,
      end of ty_ws_item,
      tt_worksheets type standard table of ty_ws_item with key title.

    types:
      begin of ty_style_map,
        uuid type uuid,
        index type i,
        format type lif_excel=>ty_style-format,
      end of ty_style_map,
      th_style_map type hashed table of ty_style_map with unique key uuid.

    class-methods load
      importing
        iv_xdata type xstring
      returning
        value(ro_excel) type ref to lcl_excel_abap2xlsx
      raising
        lcx_excel.
  private section.
    data mo_excel type ref to object.
    data mt_worksheets type tt_worksheets.
    data mt_style_map type th_style_map.
    methods build_style_map.
endclass.

class lcl_excel_abap2xlsx implementation.

  method load.

    data lx_xls type ref to cx_static_check.
    data lo_reader type ref to object.

    create object ro_excel.

    try.
      create object lo_reader type ('ZCL_EXCEL_READER_2007').
      call method lo_reader->('ZIF_EXCEL_READER~LOAD')
        exporting
          i_excel2007 = iv_xdata
        receiving
          r_excel = ro_excel->mo_excel.
    catch cx_static_check into lx_xls.
      lcx_excel=>excel_error( 'zcl_excel error: ' && lx_xls->get_text( ) ). "#EC NOTEXT
    endtry.

  endmethod.

  method lif_excel~get_sheet_names.

    data lo_iter type ref to cl_object_collection_iterator.
    field-symbols <w> like line of mt_worksheets.

    if mt_worksheets is initial.
      call method mo_excel->('GET_WORKSHEETS_ITERATOR') receiving eo_iterator = lo_iter.
      while lo_iter->has_next( ) is not initial.
        append initial line to mt_worksheets assigning <w>.
        <w>-worksheet ?= lo_iter->get_next( ).
        call method <w>-worksheet->('GET_TITLE') receiving ep_title = <w>-title.
      endwhile.
    endif.

    loop at mt_worksheets assigning <w>.
      append <w>-title to rt_sheet_names.
    endloop.

  endmethod.

  method lif_excel~get_sheet_content.

    field-symbols <w> like line of mt_worksheets.

    read table mt_worksheets assigning <w> with key title = iv_sheet_name.
    if sy-subrc <> 0.
      lcx_excel=>excel_error( msg = |Workbook does not contain [{ iv_sheet_name }] sheet| ). "#EC NOTEXT
    endif.

    if mt_style_map is initial.
      build_style_map( ).
    endif.

    field-symbols <srctab> type any table.
    field-symbols <src> type any.
    field-symbols <val> type any.
    field-symbols <dst> like line of rt_content.
    field-symbols <style> like line of mt_style_map.

    assign <w>-worksheet->('SHEET_CONTENT') to <srctab>.
    assert sy-subrc = 0.

    loop at <srctab> assigning <src>.
      append initial line to rt_content assigning <dst>.
      assign component 'CELL_ROW' of structure <src> to <val>.
      assert sy-subrc = 0.
      <dst>-cell_row    = <val>.
      assign component 'CELL_COLUMN' of structure <src> to <val>.
      assert sy-subrc = 0.
      <dst>-cell_column = <val>.
      assign component 'CELL_VALUE' of structure <src> to <val>.
      assert sy-subrc = 0.
      <dst>-cell_value  = <val>.
      assign component 'CELL_COORDS' of structure <src> to <val>.
      assert sy-subrc = 0.
      <dst>-cell_coords = <val>.
      assign component 'DATA_TYPE' of structure <src> to <val>.
      assert sy-subrc = 0.
      <dst>-data_type   = <val>.

      assign component 'CELL_STYLE' of structure <src> to <val>.
      assert sy-subrc = 0.
      read table mt_style_map assigning <style> with key uuid = <val>.
      if sy-subrc = 0.
        <dst>-cell_style = <style>-index.
      endif.
    endloop.

  endmethod.

  method lif_excel~get_styles.

    if mt_style_map is initial.
      build_style_map( ).
    endif.

    field-symbols <src> like line of mt_style_map.
    field-symbols <dst> like line of rt_styles.

    loop at mt_style_map assigning <src>.
      append initial line to rt_styles assigning <dst>.
      <dst>-id     = <src>-index.
      <dst>-format = <src>-format.
    endloop.

    sort rt_styles by id.

  endmethod.

  method build_style_map.

    data lo_style type ref to object.
    data lo_number_format type ref to object.
    data lo_iter  type ref to cl_object_collection_iterator.
    data style_map like line of mt_style_map.
    field-symbols <ref> type any.
    field-symbols <number_format> type ref to object.

    call method mo_excel->('GET_STYLES_ITERATOR') receiving eo_iterator = lo_iter.
    while lo_iter->has_next( ) is not initial.
      style_map-index = sy-index.
      lo_style ?= lo_iter->get_next( ).

      call method lo_style->('GET_GUID') receiving ep_guid = style_map-uuid.
      assign lo_style->('NUMBER_FORMAT') to <ref>.
      assert sy-subrc = 0.
      lo_number_format = <ref>.
      assign lo_number_format->('FORMAT_CODE') to <ref>.
      assert sy-subrc = 0.
      style_map-format = <ref>.

      insert style_map into table mt_style_map.
    endwhile.
  endmethod.

endclass.

**********************************************************************
* UNIT TEST
**********************************************************************

class ltcl_excel_abap2xlsx definition final
  for testing
  risk level harmless
  duration short.
  private section.

    types:
      begin of ty_content_sample,
        contents type lif_excel=>tt_sheet_content,
        excludes type lif_excel=>tt_sheet_content,
        simple type lif_excel=>tt_sheet_content,
        with_dates type lif_excel=>tt_sheet_content,
      end of ty_content_sample.

    methods load for testing raising zcx_w3mime_error lcx_excel.
    methods assert_sheet_names importing ii_excel type ref to lif_excel raising lcx_excel.
    methods assert_styles importing ii_excel type ref to lif_excel raising lcx_excel.
    methods assert_content importing ii_excel type ref to lif_excel raising lcx_excel.
    methods get_expected_content returning value(rs_content_samples) type ty_content_sample.
endclass.

class ltcl_excel_abap2xlsx implementation.

  method load.

    data xstr type xstring.
    xstr = zcl_w3mime_storage=>read_object_x( 'ZMLT_COMPILER_UNIT_TEST' ).

    data li_excel type ref to lif_excel.
    li_excel = lcl_excel_abap2xlsx=>load( xstr ).

    assert_sheet_names( li_excel ).
    assert_styles( li_excel ).
    assert_content( li_excel ).

  endmethod.

  method assert_sheet_names.

    data lt_exp type string_table.

    append '_contents' to lt_exp.
    append '_exclude' to lt_exp.
    append 'Sheet1' to lt_exp.
    append 'Sheet2' to lt_exp.
    append 'Sheet3' to lt_exp.
    append 'Sheet4' to lt_exp.

    cl_abap_unit_assert=>assert_equals(
      act = ii_excel->get_sheet_names( )
      exp = lt_exp ).

  endmethod.

  method assert_styles.

    data lt_act type lif_excel=>tt_styles.
    data style like line of lt_act.

    lt_act = ii_excel->get_styles( ).
    read table lt_act into style index 6.

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_act )
      exp = 7 ).
    cl_abap_unit_assert=>assert_equals(
      act = style-format
      exp = 'mm-dd-yy' ).

  endmethod.

  method assert_content.

    data ls_samples type ty_content_sample.
    data lt_act type lif_excel=>tt_sheet_content.
    ls_samples = get_expected_content( ).

    lt_act = ii_excel->get_sheet_content( '_contents' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-contents ).

    lt_act = ii_excel->get_sheet_content( '_exclude' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-excludes ).

    lt_act = ii_excel->get_sheet_content( 'Sheet1' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-simple ).

    lt_act = ii_excel->get_sheet_content( 'Sheet2' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-with_dates ).

    lt_act = ii_excel->get_sheet_content( 'Sheet3' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-with_dates ).

    lt_act = ii_excel->get_sheet_content( 'Sheet4' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = ls_samples-with_dates ).

  endmethod.

  method get_expected_content.
    data lt_content type lif_excel=>tt_sheet_content.
    field-symbols <i> like line of lt_content.

    define _add_cell.
      append initial line to lt_content assigning <i>.
      <i>-cell_row     = &1.
      <i>-cell_column  = &2.
      <i>-cell_value   = &3.
      <i>-cell_coords  = &4.
      <i>-cell_style   = &5.
      <i>-data_type    = &6.
    end-of-definition.

    clear lt_content.
    _add_cell 1 1 'Content'    'A1'  4  's'.
    _add_cell 1 2 'SaveToText' 'B1'  4  's'.
    _add_cell 2 1 'Sheet1'     'A2'  0  's'.
    _add_cell 2 2 'X'          'B2'  0  's'.
    _add_cell 3 1 'Sheet2'     'A3'  0  's'.
    _add_cell 4 1 'Sheet3'     'A4'  0  's'.
    _add_cell 4 2 'X'          'B4'  0  's'.
    _add_cell 5 1 'Sheet4'     'A5'  0  's'.
    _add_cell 5 2 'X'          'B5'  0  's'.
    rs_content_samples-contents = lt_content.

    clear lt_content.
    _add_cell 1 1 'to_exclude' 'A1' 7  's'.
    _add_cell 2 1 'Sheet3'     'A2' 0  's'.
    rs_content_samples-excludes = lt_content.

    clear lt_content.
    _add_cell 1 1 'Column1'   'A1' 4 's'.
    _add_cell 1 2 'Column2'   'B1' 4 's'.
    _add_cell 2 1 'A'         'A2' 0 's'.
    _add_cell 2 2 '1'         'B2' 5 ''.
    _add_cell 3 1 'B'         'A3' 0 's'.
    _add_cell 3 2 '2'         'B3' 0 ''.
    _add_cell 4 1 'C'         'A4' 0 's'.
    _add_cell 4 2 '3'         'B4' 0 ''.
    _add_cell 6 1 'More_data' 'A6' 0 's'.
    _add_cell 6 2 'to_skip'   'B6' 0 's'.
    rs_content_samples-simple = lt_content.

    clear lt_content.
    _add_cell 1 1 'A'     'A1' 4 's'.
    _add_cell 1 2 'B'     'B1' 4 's'.
    _add_cell 1 3 'C'     'C1' 4 's'.
    _add_cell 1 4 'D'     'D1' 4 's'.
    _add_cell 2 1 'Vasya' 'A2' 0 's'.
    _add_cell 2 2 '43344' 'B2' 6 ''.
    _add_cell 2 3 '15'    'C2' 0 ''.
    _add_cell 2 4 '1'     'D2' 0 'b'.
    _add_cell 3 1 'Petya' 'A3' 0 's'.
    _add_cell 3 2 '43345' 'B3' 6 ''.
    _add_cell 3 3 '16.37' 'C3' 0 ''.
    _add_cell 3 4 '0'     'D3' 0 'b'.
    rs_content_samples-with_dates = lt_content.

  endmethod.

endclass.
