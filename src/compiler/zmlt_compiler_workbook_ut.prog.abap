**********************************************************************
* UNIT TESTS
**********************************************************************

class lcl_wb_parser_test_mock definition final.
  public section.
    interfaces lif_excel.
endclass.

class lcl_wb_parser_test_mock implementation.

  method lif_excel~get_sheet_names.
    append '_contents' to rt_sheet_names.
    append '_exclude' to rt_sheet_names.
    append 'Sheet1' to rt_sheet_names.
    append 'Sheet2' to rt_sheet_names.
    append 'Sheet3' to rt_sheet_names.
    append 'Sheet4' to rt_sheet_names.
  endmethod.

  method lif_excel~get_sheet_content.
    field-symbols <i> like line of rt_content.

    define _add_cell.
      append initial line to rt_content assigning <i>.
      <i>-cell_row     = &1.
      <i>-cell_column  = &2.
      <i>-cell_value   = &3.
      <i>-cell_coords  = &4.
      <i>-cell_style   = &5.
      <i>-data_type    = &6.
    end-of-definition.

    case iv_sheet_name.
      when '_contents'.
        _add_cell 1 1 'Content'    'A1'  4  's'.
        _add_cell 1 2 'SaveToText' 'B1'  4  's'.
        _add_cell 2 1 'Sheet1'     'A2'  0  's'.
        _add_cell 2 2 'X'          'B2'  0  's'.
        _add_cell 3 1 'Sheet2'     'A3'  0  's'.
        _add_cell 4 1 'Sheet3'     'A4'  0  's'.
        _add_cell 4 2 'X'          'B4'  0  's'.
        _add_cell 5 1 'Sheet4'     'A5'  0  's'.
        _add_cell 5 2 'X'          'B5'  0  's'.
      when '_exclude'.
        _add_cell 1 1 'to_exclude' 'A1' 7  's'.
        _add_cell 2 1 'Sheet3'     'A2' 0  's'.
      when 'Sheet1'.
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
      when others. " Sheet 2,3,4
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
    endcase.
  endmethod.

  method lif_excel~get_styles.
    data style like line of rt_styles.

    define _add_style.
      style-id     = &1.
      style-format = &2.
      append style to rt_styles.
    end-of-definition.

    _add_style 1 ''.
    _add_style 2 ''.
    _add_style 3 ''.
    _add_style 4 ''.
    _add_style 5 ''.
    _add_style 6 'mm-dd-yy'.
    _add_style 7 ''.
  endmethod.

endclass.

**********************************************************************

class ltcl_workbook_parser_test definition final for testing
  duration short
  risk level harmless.

  private section.
    data mt_dummy_sheet    type lif_excel=>tt_sheet_content.
    data mt_dummy_contents type lif_excel=>tt_sheet_content.
    data mt_dummy_exclude  type lif_excel=>tt_sheet_content.

    methods setup.
    methods read_row for testing.
    methods is_row_empty for testing.
    methods clip_header for testing.
    methods clip_rows for testing.
    methods read_contents for testing.
    methods convert_sheet for testing.
    methods read_exclude for testing.
    methods integration_test for testing raising lcx_error.
    methods date_processing for testing raising lcx_error.

endclass.

class ltcl_workbook_parser_test implementation.

  define _add_cell.
    cell-cell_row = &2.
    cell-cell_column = &3.
    cell-cell_value = &4.
    insert cell into table &1.
  end-of-definition.

  method setup.

    data cell like line of mt_dummy_sheet.

    _add_cell mt_dummy_sheet 1 1 '_idx'.
    _add_cell mt_dummy_sheet 1 2 'col1'.
    _add_cell mt_dummy_sheet 1 3 'col2'.
    _add_cell mt_dummy_sheet 1 4 '_comment'.
    _add_cell mt_dummy_sheet 1 6 'aux_col'.

    _add_cell mt_dummy_sheet 2 1 '1'.
    _add_cell mt_dummy_sheet 2 2 'A'.
    _add_cell mt_dummy_sheet 2 3 '10'.
    _add_cell mt_dummy_sheet 2 4 'some comment'.

    _add_cell mt_dummy_sheet 3 1 '2'.
    _add_cell mt_dummy_sheet 3 2 'B'.
    _add_cell mt_dummy_sheet 3 3 '20'.

    _add_cell mt_dummy_sheet 5 2 'some excluded data'.

    " _contents

    _add_cell mt_dummy_contents 1 1 'SheetName'.
    _add_cell mt_dummy_contents 1 2 'ToSave'.
    _add_cell mt_dummy_contents 1 3 'comment'.
    _add_cell mt_dummy_contents 2 1 'Sheet1'.
    _add_cell mt_dummy_contents 2 2 'X'.
    _add_cell mt_dummy_contents 3 1 'Sheet2'.
    _add_cell mt_dummy_contents 3 2 ''.
    _add_cell mt_dummy_contents 4 1 'Sheet3'.
    _add_cell mt_dummy_contents 4 2 'X'.

    " _exclude
    _add_cell mt_dummy_exclude 1 1 'SheetName'.
    _add_cell mt_dummy_exclude 1 2 'Some comment'.
    _add_cell mt_dummy_exclude 2 1 'SheetX'.
    _add_cell mt_dummy_exclude 2 2 'comment 1'.
    _add_cell mt_dummy_exclude 3 1 'SheetY'.

  endmethod.   " setup.

  method date_processing.

    data lx type ref to cx_root.
    data lt_act type string_table.
    data lt_exp type string_table.
    data lt_sheet like mt_dummy_sheet.
    data cell like line of lt_sheet.
    data lt_date_styles type lcl_workbook_parser=>ts_style_list.

    cell-cell_row = 1.
    cell-cell_column = 1.
    cell-cell_value = '43895'.
    cell-cell_style = 10.
    insert cell into table lt_sheet.

    insert 10 into table lt_date_styles.

    clear lt_exp.
    append '05.03.2020' to lt_exp.

    lt_act = lcl_workbook_parser=>read_row(
      it_content     = lt_sheet
      i_row          = 1
      it_date_styles = lt_date_styles ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

    cell-cell_row = 1.
    cell-cell_column = 2.
    cell-cell_value = 'X'.
    cell-cell_style = 10.
    insert cell into table lt_sheet.

    try .
      lcl_workbook_parser=>read_row(
        it_content     = lt_sheet
        i_row          = 1
        it_date_styles = lt_date_styles ).
      cl_abap_unit_assert=>fail( ).
    catch lcx_excel into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->get_text( )
        exp = 'expected date @R1C2' ).
    endtry.

  endmethod.

  method read_row.
    data lx type ref to cx_root.
    data lt_act type string_table.
    data lt_exp type string_table.

    clear lt_exp.
    append '_idx' to lt_exp.
    append 'col1' to lt_exp.
    append 'col2' to lt_exp.
    append '_comment' to lt_exp.
    append '' to lt_exp.
    append 'aux_col' to lt_exp.

    try.

      lt_act = lcl_workbook_parser=>read_row(
        it_content = mt_dummy_sheet
        i_row      = 1
        i_colmax   = 6 ).
      cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

      lt_act = lcl_workbook_parser=>read_row(
        it_content = mt_dummy_sheet
        i_row      = 1 ).
      cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

      " 2
      lt_act = lcl_workbook_parser=>read_row(
        it_content = mt_dummy_sheet
        i_row      = 5
        i_colmax   = 3 ).

      clear lt_exp.
      append '' to lt_exp.
      append 'some excluded data' to lt_exp.
      append '' to lt_exp.

      cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

      " 3
      lt_act = lcl_workbook_parser=>read_row(
        it_content = mt_dummy_sheet
        i_row      = 5
        i_colmin   = 2
        i_colmax   = 4 ).

      clear lt_exp.
      append 'some excluded data' to lt_exp.
      append '' to lt_exp.
      append '' to lt_exp.

      cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).
    catch cx_root into lx.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.
  endmethod.  " read_row.

  method is_row_empty.

    data lv_act type abap_bool.

    lv_act = lcl_workbook_parser=>is_row_empty(
      it_content = mt_dummy_sheet
      i_row      = 1
      i_colmax   = 4 ).
    cl_abap_unit_assert=>assert_false( lv_act ).

    lv_act = lcl_workbook_parser=>is_row_empty(
      it_content = mt_dummy_sheet
      i_row      = 5
      i_colmax   = 4 ).
    cl_abap_unit_assert=>assert_false( lv_act ).

    lv_act = lcl_workbook_parser=>is_row_empty(
      it_content = mt_dummy_sheet
      i_row      = 4
      i_colmax   = 4 ).
    cl_abap_unit_assert=>assert_true( lv_act ).

    lv_act = lcl_workbook_parser=>is_row_empty(
      it_content = mt_dummy_sheet
      i_row      = 5
      i_colmin   = 3
      i_colmax   = 4 ).
    cl_abap_unit_assert=>assert_true( lv_act ).

  endmethod.  " is_row_empty.

  method clip_header.
    data lt_head type string_table.
    data ls_range type lcl_workbook_parser=>ty_range.

    append '_idx' to lt_head.
    append 'col1' to lt_head.
    append 'col2' to lt_head.
    append '_comment' to lt_head.
    append '' to lt_head.
    append 'aux_col' to lt_head.

    lcl_workbook_parser=>clip_header(
      exporting it_head = lt_head
      changing  cs_range = ls_range ).

    cl_abap_unit_assert=>assert_equals( act = ls_range-col_min exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_range-col_max exp = 3 ).

  endmethod.  " clip_header.

  method clip_rows.
    data ls_range type lcl_workbook_parser=>ty_range.

    ls_range-col_min = 2.
    ls_range-col_max = 3.

    lcl_workbook_parser=>clip_rows(
      exporting it_content = mt_dummy_sheet
      changing  cs_range = ls_range ).

    cl_abap_unit_assert=>assert_equals( act = ls_range-row_min exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_range-row_max exp = 3 ).

  endmethod.  " clip_rows.

  method read_contents.
    data lt_act type string_table.
    data lt_exp type string_table.

    try .
      lt_act = lcl_workbook_parser=>read_contents( mt_dummy_contents ).
    catch lcx_error.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

    append 'Sheet1' to lt_exp.
    append 'Sheet3' to lt_exp.

    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.  " read_contents.

  method convert_sheet.
    data lv_act type string.
    data lf  like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline.
    data tab like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.

    try.
      lv_act = lcl_workbook_parser=>convert_sheet( mt_dummy_sheet ).
    catch lcx_error.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = |col1{ tab }col2{ lf }A{ tab }10{ lf }B{ tab }20| ).

  endmethod.  " convert_sheet.

  method read_exclude.
    data lt_act type string_table.
    data lt_exp type string_table.

    try .
      lt_act = lcl_workbook_parser=>read_exclude( mt_dummy_exclude ).
    catch lcx_error.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

    append 'SheetX' to lt_exp.
    append 'SheetY' to lt_exp.

    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.  " read_exclude.

  method integration_test.

    data lo_excel_mock type ref to lcl_wb_parser_test_mock.
    create object lo_excel_mock.

    data lt_act type lcl_workbook_parser=>tt_mocks.
    lt_act = lcl_workbook_parser=>parse( lo_excel_mock ).

    data lt_exp type lcl_workbook_parser=>tt_mocks.
    field-symbols <exp> like line of lt_exp.

    append initial line to lt_exp assigning <exp>.
    <exp>-name = 'Sheet1'.
    <exp>-data = 'Column1\tColumn2\nA\t1\nB\t2\nC\t3'.
    <exp>-data = replace( val = <exp>-data sub = '\n' with = cl_abap_char_utilities=>newline occ = 0 ).
    <exp>-data = replace( val = <exp>-data sub = '\t' with = cl_abap_char_utilities=>horizontal_tab occ = 0 ).

    append initial line to lt_exp assigning <exp>.
    <exp>-name = 'Sheet4'.
    <exp>-data = 'A\tB\tC\tD\nVasya\t01.09.2018\t15\t1\nPetya\t02.09.2018\t16.37\t0'.
    <exp>-data = replace( val = <exp>-data sub = '\n' with = cl_abap_char_utilities=>newline occ = 0 ).
    <exp>-data = replace( val = <exp>-data sub = '\t' with = cl_abap_char_utilities=>horizontal_tab occ = 0 ).

    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

endclass.
