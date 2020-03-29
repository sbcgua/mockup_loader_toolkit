**********************************************************************
* WORKBOOK PARSER
**********************************************************************
include zmlt_compiler_excel_defs.
include zmlt_compiler_abap2xlsx.
include zmlt_compiler_xlreader.

class ltcl_workbook_parser_test definition deferred.
class lcl_workbook_parser definition final
  friends ltcl_workbook_parser_test.
  public section.
    types:
      begin of ty_mock,
        name type string,
        data type string,
      end of ty_mock,
      tt_mocks type standard table of ty_mock with key name.

    types:
      begin of ty_range,
        row_min type i,
        row_max type i,
        col_min type i,
        col_max type i,
      end of ty_range.

    types:
      tt_style_list type standard table of i with default key,
      ts_style_list type sorted table of i with unique key table_line.

    constants contents_sheet_name type string value '_contents'.
    constants exclude_sheet_name type string value '_exclude'.

    class-methods parse
      importing
        ii_excel type ref to lif_excel
      returning
        value(rt_mocks) type tt_mocks
      raising
        lcx_excel.

  private section.

    class-methods read_contents
      importing
        it_content type lif_excel=>tt_sheet_content
      returning
        value(rt_sheets_to_save) type string_table
      raising
        lcx_excel.

    class-methods read_exclude
      importing
        it_exclude type lif_excel=>tt_sheet_content
      returning
        value(rt_sheets_to_exclude) type string_table
      raising
        lcx_excel.

    class-methods find_date_styles
      importing
        ii_excel type ref to lif_excel
      returning
        value(rt_style_list) type tt_style_list
      raising
        lcx_excel.

    class-methods clip_header
      importing
        it_head type string_table
      changing
        cs_range type ty_range.

    class-methods clip_rows
      importing
        it_content   type lif_excel=>tt_sheet_content
        iv_start_row type i default 1
      changing
        cs_range type ty_range.

    class-methods clip_range
      importing
        it_content type lif_excel=>tt_sheet_content
      returning
        value(rs_range) type ty_range
      raising
        lcx_excel.

    class-methods convert_to_date
      importing
        iv_value type string
      returning
        value(rv_date) type string
      raising
        cx_sy_conversion_error.

    class-methods read_row
      importing
        it_content type lif_excel=>tt_sheet_content
        i_row type i
        i_colmin type i default 1
        i_colmax type i default 9999
        it_date_styles type ts_style_list optional
      returning
        value(rt_values) type string_table
      raising
        lcx_excel.

    class-methods is_row_empty
      importing
        it_content type lif_excel=>tt_sheet_content
        i_row type i
        i_colmin type i default 1
        i_colmax type i
      returning
        value(rv_yes) type abap_bool.

    class-methods convert_sheet
      importing
        it_content type lif_excel=>tt_sheet_content
        it_date_styles type ts_style_list optional
      returning
        value(rv_data) type string
      raising
        lcx_excel.

endclass.


class lcl_workbook_parser implementation.

  method parse.

    " Get list of work sheets
    data lt_worksheets type string_table.
    data lt_sheets_to_save type string_table.
    lt_worksheets = ii_excel->get_sheet_names( ).

    " Check and read content
    field-symbols <ws> like line of lt_worksheets.
    read table lt_worksheets assigning <ws> with key table_line = contents_sheet_name.
    if sy-subrc is not initial.
      lt_sheets_to_save = lt_worksheets. " Just parse all
    else.
      lt_sheets_to_save = read_contents( ii_excel->get_sheet_content( contents_sheet_name ) ).

      " Check all sheets exist
      field-symbols <sheet_name> like line of lt_sheets_to_save.
      loop at lt_sheets_to_save assigning <sheet_name>.
        read table lt_worksheets with key table_line = <sheet_name> transporting no fields.
        if sy-subrc is not initial.
          lcx_excel=>excel_error( msg = |Workbook does not contain [{ <sheet_name> }] sheet| ). "#EC NOTEXT
        endif.
      endloop.
    endif.

    " Read excludes
    data lt_excludes type sorted table of string with non-unique key table_line.
    read table lt_worksheets assigning <ws> with key table_line = exclude_sheet_name.
    if sy-subrc = 0.
      lt_excludes = read_exclude( ii_excel->get_sheet_content( exclude_sheet_name ) ).
    endif.

    " exclude sheets
    data lv_index type sy-tabix.
    loop at lt_sheets_to_save assigning <sheet_name>.
      lv_index = sy-tabix.
      read table lt_excludes with key table_line = <sheet_name> transporting no fields.
      if sy-subrc = 0 or <sheet_name> = exclude_sheet_name or <sheet_name>+0(1) = '-'.
        delete lt_sheets_to_save index lv_index.
      endif.
    endloop.

    data lt_date_styles type ts_style_list.
    lt_date_styles = find_date_styles( ii_excel ).

    " convert sheets
    data lx type ref to lcx_excel.
    field-symbols <mock> like line of rt_mocks.
    try .
      loop at lt_sheets_to_save assigning <sheet_name>.
        read table lt_worksheets with key table_line = <sheet_name> assigning <ws>.
        append initial line to rt_mocks assigning <mock>.
        <mock>-name = <sheet_name>.
        <mock>-data = convert_sheet(
          it_content     = ii_excel->get_sheet_content( <sheet_name> )
          it_date_styles = lt_date_styles ).
      endloop.
    catch lcx_excel into lx.
      if lx->rc = 'date'.
        lcx_excel=>excel_error( |{ lx->msg } @{ <sheet_name> }| ).
      else.
        raise exception lx.
      endif.
    endtry.

  endmethod.  " parse.

  method convert_sheet.

    data ls_range type ty_range.
    ls_range = clip_range( it_content ).

    data lt_data type string_table.
    data lt_values type string_table.
    data lv_temp_line type string.
    do ls_range-row_max - ls_range-row_min + 1 times. " starts from 1 always
      lt_values = read_row(
        it_content     = it_content
        it_date_styles = it_date_styles
        i_row          = sy-index + ls_range-row_min - 1
        i_colmin       = ls_range-col_min
        i_colmax       = ls_range-col_max ).
      lv_temp_line = concat_lines_of( table = lt_values sep = cl_abap_char_utilities=>horizontal_tab ).
      append lv_temp_line to lt_data.
    enddo.

    rv_data = concat_lines_of( table = lt_data sep = cl_abap_char_utilities=>newline ).

  endmethod.

  method find_date_styles.

    data lt_styles type lif_excel=>tt_styles.
    field-symbols <s> like line of lt_styles.

    lt_styles = ii_excel->get_styles( ).

    loop at lt_styles assigning <s>.
      if <s>-format ca 'd' and <s>-format ca 'm' and <s>-format ca 'y'. " Guess it is date ...
        append <s>-id to rt_style_list.
      endif.
    endloop.

  endmethod.

  method convert_to_date.
    if iv_value is initial.
      return. " rv_date also initial
    endif.

    " this code is taken from abap2xlsx
    constants c_excel_baseline_date type d value '19000101'. "#EC NOTEXT
    constants c_excel_1900_leap_year type d value '19000228'. "#EC NOTEXT
    data lv_date_int type i.
    data lv_date type d.

    lv_date_int = iv_value.
    lv_date     = lv_date_int + c_excel_baseline_date - 2.
    " Needed hack caused by the problem that:
    " Excel 2000 incorrectly assumes that the year 1900 is a leap year
    " http://support.microsoft.com/kb/214326/en-us
    if lv_date < c_excel_1900_leap_year.
      lv_date = lv_date + 1.
    endif.
    " end of code, taken from abap2xlsx

    rv_date = lv_date.
    rv_date = rv_date+6(2) && '.' && rv_date+4(2) && '.' && rv_date+0(4). " TODO reafactor
  endmethod.

  method read_row.

    assert i_row > 0.
    assert i_colmin > 0.
    assert i_colmin <= i_colmax.

    field-symbols <c> like line of it_content.
    data last_col type i.
    data tmp type string.
    last_col = i_colmin - 1.

    try.
      loop at it_content assigning <c> where cell_row = i_row and cell_column between i_colmin and i_colmax.
        do <c>-cell_column - last_col - 1 times. " fill blanks
          append initial line to rt_values.
        enddo.
        last_col = <c>-cell_column.
        tmp      = <c>-cell_value.
        if it_date_styles is supplied and <c>-cell_style is not initial.
          read table it_date_styles with key table_line = <c>-cell_style transporting no fields.
          if sy-subrc is initial.
            tmp = convert_to_date( tmp ).
          endif.
        endif.
        append tmp to rt_values.
      endloop.
    catch cx_sy_conversion_error.
      lcx_excel=>excel_error( msg = |expected date @R{ i_row }C{ last_col }| rc = 'date' ).
    endtry.

    if last_col < i_colmax and i_colmax is supplied. " fill blanks
      do i_colmax - last_col times.
        append initial line to rt_values.
      enddo.
    endif.

  endmethod.

  method is_row_empty.

    assert i_row > 0.
    assert i_colmin > 0.
    assert i_colmin <= i_colmax.

    field-symbols <c> like line of it_content.
    rv_yes = abap_true.

    loop at it_content assigning <c> where cell_row = i_row and cell_column between i_colmin and i_colmax.
      if <c>-cell_value is not initial.
        rv_yes = abap_false.
        exit.
      endif.
    endloop.

  endmethod.

  method clip_header.

    field-symbols <str> like line of it_head.
    cs_range-col_min = 1.
    cs_range-col_max = lines( it_head ).

    " clip on first empty field
    read table it_head with key table_line = '' transporting no fields.
    if sy-subrc is initial.
      cs_range-col_max = sy-tabix - 1.
    endif.

    " clip technical fields from the end
    while cs_range-col_max > 0.
      read table it_head index cs_range-col_max assigning <str>.
      if <str>+0(1) = '_'.
        cs_range-col_max = cs_range-col_max - 1.
      else.
        exit.
      endif.
    endwhile.

    " clip technical fields from the start
    while cs_range-col_min < cs_range-col_max.
      read table it_head index cs_range-col_min assigning <str>.
      if <str>+0(1) = '_'.
        cs_range-col_min = cs_range-col_min + 1.
      else.
        exit.
      endif.
    endwhile.

  endmethod.

  method clip_rows.

    data lv_is_empty type abap_bool.

    cs_range-row_min = iv_start_row.
    cs_range-row_max = iv_start_row.

    do.
      lv_is_empty = is_row_empty(
        it_content = it_content
        i_row = cs_range-row_max + 1
        i_colmin = cs_range-col_min
        i_colmax = cs_range-col_max ).
      if lv_is_empty = abap_true.
        exit.
      else.
        cs_range-row_max = cs_range-row_max + 1.
      endif.
    enddo.

  endmethod.

  method clip_range.
    field-symbols <c> like line of it_content.
    " Assuming content is sorted
    read table it_content assigning <c> index 1.
    if sy-subrc is not initial or <c>-cell_row <> 1 or <c>-cell_column <> 1 or <c>-cell_value is initial.
      lcx_excel=>excel_error( msg = 'Sheet data must start at A1 cell' ). "#EC NOTEXT
    endif.

    data lt_head type string_table.
    data lv_start_row type i value 1.
    field-symbols <cell> type string.

    lt_head = read_row(
      it_content = it_content
      i_row      = 1 ).

    read table lt_head index 1 assigning <cell>.
    if <cell>+0(1) = '#'. " Skip first comment row
      lv_start_row = 2.
      lt_head = read_row(
        it_content = it_content
        i_row      = 2 ).
    endif.

    clip_header(
      exporting
        it_head = lt_head
      changing
        cs_range = rs_range ).
    if rs_range-col_max < rs_range-col_min.
      lcx_excel=>excel_error( msg = 'Only contains technical field (_...) in the header' ). "#EC NOTEXT
    endif.

    clip_rows(
      exporting
        it_content   = it_content
        iv_start_row = lv_start_row
      changing
        cs_range = rs_range ).

  endmethod.

  method read_contents.

    data ls_range type ty_range.
    ls_range = clip_range( it_content ).

    if ls_range-col_max - ls_range-col_min + 1 < 2.
      lcx_excel=>excel_error( msg = '_contents sheet must have at least 2 columns' ). "#EC NOTEXT
    endif.

    if ls_range-row_max < 2.
      lcx_excel=>excel_error( msg = '_contents sheet must have at least 1 sheet config' ). "#EC NOTEXT
    endif.

    ls_range-col_max = ls_range-col_min + 1. " Consider only 1st 2 columns

    data lt_values type string_table.
    field-symbols <str> type string.
    do ls_range-row_max - ls_range-row_min times.
      lt_values = read_row(
        it_content = it_content
        i_row      = sy-index + 1
        i_colmin   = ls_range-col_min
        i_colmax   = ls_range-col_max ).
      read table lt_values index 2 assigning <str>.
      if <str> is not initial.
        read table lt_values index 1 assigning <str>.
        append <str> to rt_sheets_to_save.
      endif.
    enddo.

  endmethod.

  method read_exclude.

    data ls_range type ty_range.
    ls_range = clip_range( it_exclude ).

    if ls_range-col_max - ls_range-col_min + 1 < 1.
      lcx_excel=>excel_error( msg = '_exclude sheet must have at least 1 column' ). "#EC NOTEXT
    endif.

    if ls_range-row_max < 2.
      lcx_excel=>excel_error( msg = '_exclude sheet must have at least 1 sheet config' ). "#EC NOTEXT
    endif.

    ls_range-col_max = ls_range-col_min. " Consider only 1st column

    data lt_values type string_table.
    field-symbols <str> type string.
    do ls_range-row_max - ls_range-row_min times.
      lt_values = read_row(
        it_content = it_exclude
        i_row      = sy-index + 1
        i_colmin   = ls_range-col_min
        i_colmax   = ls_range-col_max ).
      read table lt_values index 1 assigning <str>.
      append <str> to rt_sheets_to_exclude.
    enddo.

  endmethod.

endclass.

include zmlt_compiler_workbook_ut.
