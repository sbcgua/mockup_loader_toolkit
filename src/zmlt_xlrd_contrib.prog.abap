interface ZIF_XLSXREADER_NODE_PROCESSOR.
*  public .

  methods process_node
    importing
      io_node type ref to if_ixml_node
      i_context type any optional.

endinterface.

class ZCL_XLSXREADER_XML_UTILS definition
*  public
  final
  create public .

  public section.

    class-methods iterate_children
      importing
        io_node type ref to if_ixml_node
        ii_item_processor type ref to zif_xlsxreader_node_processor
        i_context type any optional.

    class-methods iterate_children_by_tag_name
      importing
        io_element type ref to if_ixml_element
        iv_tag_name type string
        ii_item_processor type ref to zif_xlsxreader_node_processor
        i_context type any optional.

    class-methods parse_xmldoc
      importing
        !iv_xml type xstring
      returning
        value(ro_xmldoc) type ref to if_ixml_document .

    class-methods attributes_to_struc
      importing
        io_node type ref to if_ixml_node
        iv_no_clear type abap_bool default abap_false
      exporting
        es_struc type any.

    class-methods children_to_table
      importing
        io_node type ref to if_ixml_node
        iv_value_to type abap_compname optional
        iv_no_attributes type abap_bool default abap_false
      exporting
        et_tab type standard table.


  protected section.
  private section.
    class-methods iterate_nodes
      importing
        io_node_iterator type ref to if_ixml_node_iterator
        ii_item_processor type ref to zif_xlsxreader_node_processor
        i_context type any.

ENDCLASS.



CLASS ZCL_XLSXREADER_XML_UTILS IMPLEMENTATION.


  method attributes_to_struc.

    data lo_attrs     type ref to if_ixml_named_node_map.
    data lo_attr      type ref to if_ixml_attribute.
    data lo_iterator  type ref to if_ixml_node_iterator.
    data lv_attr_name type string.
    field-symbols <fld> type any.

    if iv_no_clear = abap_false.
      clear es_struc.
    endif.

    lo_attrs    = io_node->get_attributes( ).
    lo_iterator = lo_attrs->create_iterator( ).
    lo_attr    ?= lo_iterator->get_next( ).
    while lo_attr is bound.
      lv_attr_name = to_upper( lo_attr->get_name( ) ).
      assign component lv_attr_name of structure es_struc to <fld>.
      if sy-subrc = 0.
        <fld> = lo_attr->get_value( ).
      endif.
      lo_attr ?= lo_iterator->get_next( ).
    endwhile.

  endmethod.


  method children_to_table.

    data lo_node_iterator type ref to if_ixml_node_iterator.
    data lo_node type ref to if_ixml_node.
    data lv_value_to like iv_value_to.

    clear et_tab.

    lo_node_iterator = io_node->get_children( )->create_iterator( ).
    lo_node          = lo_node_iterator->get_next( ).
    lv_value_to      = to_upper( iv_value_to ).

    field-symbols <i> type any.
    field-symbols <fld> type any.

    while lo_node is bound.
      append initial line to et_tab assigning <i>.
      if iv_no_attributes = abap_false.
        attributes_to_struc(
          exporting
            io_node = lo_node
          importing
            es_struc = <i> ).
      endif.
      if lv_value_to = '*'.
        <i> = lo_node->get_value( ).
      elseif lv_value_to is not initial.
        assign component lv_value_to of structure <i> to <fld>.
        if sy-subrc = 0.
          <fld> = lo_node->get_value( ).
        endif.
      endif.
      lo_node = lo_node_iterator->get_next( ).
    endwhile.

  endmethod.


  method iterate_children.
    iterate_nodes(
      io_node_iterator  = io_node->get_children( )->create_iterator( )
      i_context         = i_context
      ii_item_processor = ii_item_processor ).
  endmethod.


  method iterate_children_by_tag_name.
    iterate_nodes(
      io_node_iterator  = io_element->get_elements_by_tag_name_ns( name = iv_tag_name )->create_iterator( )
      i_context         = i_context
      ii_item_processor = ii_item_processor ).
  endmethod.


  method iterate_nodes.
    data lo_node type ref to if_ixml_node.
    lo_node          = io_node_iterator->get_next( ).

    while lo_node is bound.
      ii_item_processor->process_node(
        i_context = i_context
        io_node   = lo_node ).
      lo_node = io_node_iterator->get_next( ).
    endwhile.
  endmethod.


  method parse_xmldoc.

    data lo_ixml type ref to if_ixml.
    data lo_ixml_sf type ref to if_ixml_stream_factory.
    data lo_ixml_stream type ref to if_ixml_istream.
    data lo_ixml_parser type ref to if_ixml_parser.

    lo_ixml        = cl_ixml=>create( ).
    lo_ixml_sf     = lo_ixml->create_stream_factory( ).
    lo_ixml_stream = lo_ixml_sf->create_istream_xstring( iv_xml ).
    ro_xmldoc      = lo_ixml->create_document( ).
    lo_ixml_parser = lo_ixml->create_parser(
      document       = ro_xmldoc
      istream        = lo_ixml_stream
      stream_factory = lo_ixml_sf ).

    lo_ixml_parser->parse( ).

  endmethod.
ENDCLASS.


class ZCL_XLSXREADER definition
*  public
  create public .

  public section.

    interfaces ZIF_XLSXREADER_NODE_PROCESSOR .

    types:
      begin of ty_num_format,
        numfmtid   type i,
        formatcode type string,
      end of ty_num_format .
    types:
      tt_num_formats type table of ty_num_format with key numfmtid .
    types:
      ts_num_formats type sorted table of ty_num_format with unique key numfmtid .
    types:
      begin of ty_sheet,
        name    type string,
        sheetid type i,
        id      type string,
      end of ty_sheet .
    types:
      tt_sheets type table of ty_sheet with key name .
    types:
      begin of ty_cell_style,
        numfmtid   type i,
      end of ty_cell_style .
    types:
      tt_cell_styles type table of ty_cell_style with default key .
    types:
      begin of ty_raw_cell,
        r     type string,
        s     type i,
        t     type string,
        row   type string,
        value type string,
      end of ty_raw_cell .
    types:
      tt_raw_cells type standard table of ty_raw_cell with key r .
    types:
      begin of ty_parsing_context,
        stage type string,
        data  type ref to data,
      end of ty_parsing_context .
    types:
      begin of ty_cell,
        col   type i,
        row   type i,
        type  type c length 1,
        style type i,
        value type string,
        ref   type string,
      end of ty_cell .
    types:
      begin of ty_style,
        num_format type string,
      end of ty_style .
    types:
      tt_styles type standard table of ty_style with default key .
    types:
      tt_cells type standard table of ty_cell with key col row .

    constants C_OPENXML_NAMESPACE_URI type STRING value 'http://schemas.openxmlformats.org/spreadsheetml/2006/main' ##NO_TEXT.

    class-methods LOAD
      importing
        !IV_XDATA type XSTRING
      returning
        value(RO_INSTANCE) type ref to ZCL_XLSXREADER
      raising
        CX_OPENXML_FORMAT
        CX_OPENXML_NOT_FOUND .
    methods GET_SHEET
      importing
        !IV_NAME type STRING
      returning
        value(RT_CELLS) type TT_CELLS
      raising
        CX_OPENXML_NOT_FOUND
        CX_OPENXML_FORMAT .
    methods CONSTRUCTOR
      importing
        !IV_XDATA type XSTRING
      raising
        CX_OPENXML_FORMAT
        CX_OPENXML_NOT_FOUND .
    methods GET_SHEET_NAMES
      returning
        value(RT_SHEET_NAMES) type STRING_TABLE
      raising
        CX_OPENXML_FORMAT .
    methods GET_STYLES
      returning
        value(RT_STYLES) type TT_STYLES
      raising
        CX_OPENXML_FORMAT
        CX_OPENXML_NOT_FOUND .
  protected section.
  private section.

    constants c_excldt type dats value '19000101' ##NO_TEXT.

    data mo_workbook type ref to cl_xlsx_workbookpart .
    data mt_sheets type tt_sheets .
    data mo_xlsx type ref to cl_xlsx_document .
    data mt_shared_strings type string_table.

    methods get_sheets
      returning
        value(rt_sheets) type tt_sheets
      raising
        cx_openxml_format .

    methods convert_date
      importing
        !iv_days type string
      returning
        value(rv_date) type dats .

    methods get_shared_string
      importing
        iv_index type i
      returning
        value(rv_str) type string.

    methods load_shared_strings
      raising
        cx_openxml_format cx_openxml_not_found.

    methods parse_worksheet
      importing
        iv_name type string
      returning
        value(rt_raw_cells) type tt_raw_cells
      raising
        cx_openxml_format cx_openxml_not_found.

    methods add_default_num_formats
      changing ct_num_formats type tt_num_formats.

    methods convert_raw_cells
      importing
        it_raw_cells type tt_raw_cells
      returning
        value(rt_cells) type tt_cells.

    class-methods column_to_index
      importing
        iv_col type string
      returning
        value(rv_index) type i.

ENDCLASS.



CLASS ZCL_XLSXREADER IMPLEMENTATION.


  method add_default_num_formats.

    data ls_num_format like line of ct_num_formats.

    define _add_num_format.
      ls_num_format-numfmtid   = &1.
      ls_num_format-formatcode = &2.
      append ls_num_format to ct_num_formats.
    end-of-definition.

    _add_num_format 0  'General'.
    _add_num_format 1  '0'.
    _add_num_format 2  '0.00'.
    _add_num_format 3  '#,##0'.
    _add_num_format 4  '#,##0.00'.
    _add_num_format 5  '$#,##0_);($#,##0)'.
    _add_num_format 6  '$#,##0_);[Red]($#,##0)'.
    _add_num_format 7  '$#,##0.00_);($#,##0.00)'.
    _add_num_format 8  '$#,##0.00_);[Red]($#,##0.00)'.
    _add_num_format 9  '0%'.
    _add_num_format 10 '0.00%'.
    _add_num_format 11 '0.00E+00'.
    _add_num_format 12 '# ?/?'.
    _add_num_format 13 '# ??/??'.
    _add_num_format 14 'm/d/yy'.
    _add_num_format 15 'd-mmm-yy'.
    _add_num_format 16 'd-mmm'.
    _add_num_format 17 'mmm-yy'.
    _add_num_format 18 'h:mm AM/PM'.
    _add_num_format 19 'h:mm:ss AM/PM'.
    _add_num_format 20 'h:mm'.
    _add_num_format 21 'h:mm:ss'.
    _add_num_format 22 'm/d/yy h:mm'.
    _add_num_format 36 'm/d/yy'.
    _add_num_format 37 '#,##0 ;(#,##0)'.
    _add_num_format 38 '#,##0 ;[Red](#,##0)'.
    _add_num_format 39 '#,##0.00;(#,##0.00)'.
    _add_num_format 40 '#,##0.00;[Red](#,##0.00)'.
    _add_num_format 45 'mm:ss'.
    _add_num_format 46 '[h]:mm:ss'.
    _add_num_format 47 'mmss.0'.
    _add_num_format 48 '##0.0E+0'.
    _add_num_format 49 '@'.
    _add_num_format 50 'm/d/yy'.
    _add_num_format 51 'm/d/yy'.
    _add_num_format 52 'm/d/yy'.
    _add_num_format 53 'm/d/yy'.
    _add_num_format 54 'm/d/yy'.
    _add_num_format 55 'm/d/yy'.
    _add_num_format 56 'm/d/yy'.
    _add_num_format 57 'm/d/yy'.
    _add_num_format 58 'm/d/yy'.
    _add_num_format 59 '0'.
    _add_num_format 60 '0.00'.
    _add_num_format 61 '#,##0'.
    _add_num_format 62 '#,##0.00'.
    _add_num_format 63 '$#,##0_);($#,##0)'.
    _add_num_format 64 '$#,##0_);[Red]($#,##0)'.
    _add_num_format 65 '$#,##0.00_);($#,##0.00)'.
    _add_num_format 66 '$#,##0.00_);[Red]($#,##0.00)'.
    _add_num_format 67 '0%'.
    _add_num_format 68 '0.00%'.
    _add_num_format 69 '# ?/?'.
    _add_num_format 70 '# ??/??'.
    _add_num_format 71 'm/d/yy'.
    _add_num_format 72 'm/d/yy'.
    _add_num_format 73 'd-mmm-yy'.
    _add_num_format 74 'd-mmm'.
    _add_num_format 75 'mmm-yy'.
    _add_num_format 76 'h:mm'.
    _add_num_format 77 'h:mm:ss'.
    _add_num_format 78 'm/d/yy h:mm'.
    _add_num_format 79 'mm:ss'.
    _add_num_format 80 '[h]:mm:ss'.
    _add_num_format 81 'mmss.0'.

  endmethod.


  method column_to_index.

    data lv_len type i.
    data lv_code type i.
    data lv_char type c length 1.
    field-symbols <bin> type x.

    lv_len = strlen( iv_col ).
    assert lv_len <= 3. " >3 unxepected ...
    do lv_len times.
      rv_index = rv_index * 26. " A..Z = 26
      sy-index = sy-index - 1.
      lv_char = iv_col+sy-index(1).
      assign lv_char to <bin> casting.
      lv_code = <bin>+0(1).
      rv_index = rv_index + lv_code - 65 + 1. " A = 4100 in unicode / 16640
    enddo.

  endmethod.


  method constructor.
    mo_xlsx = cl_xlsx_document=>load_document( iv_xdata ).
    mo_workbook = mo_xlsx->get_workbookpart( ).
  endmethod.


  method convert_date.
    data lv_days type i.

    if iv_days co '0123456789'.
      lv_days = iv_days.
      rv_date = c_excldt + lv_days.
    endif.
  endmethod.


  method convert_raw_cells.

    field-symbols <c> like line of it_raw_cells.
    field-symbols <res> like line of rt_cells.
    data lv_row type string.
    data lv_col type string.

    loop at it_raw_cells assigning <c>.
      append initial line to rt_cells assigning <res>.

      "get column
      lv_row = <c>-row.
      lv_col = <c>-r. " cell ref
      condense lv_row no-gaps.
      replace lv_row in lv_col with space.

      <res>-row   = lv_row.
      <res>-col   = column_to_index( lv_col ).
      <res>-type  = <c>-t.
      <res>-ref   = <c>-r.

      " Styles are not clear, need to read the spec ...
      " +1 because the are indexed from 0
      " +2 because there are 2 default excel styles, however cell style seems to refer real style list from style.xml
      " style = -1 if undefined (filled in parse)
      if <c>-s >= 0.
        <res>-style = <c>-s + 1 + 2.
      endif.

      if <c>-t eq 's'.
        <res>-value = get_shared_string( <c>-value + 1 ).
      else.
        <res>-value = <c>-value.
      endif.
    endloop.

  endmethod.


  method get_shared_string.
    read table mt_shared_strings into rv_str index iv_index.
  endmethod.


  method get_sheet.
    data lt_raw_cells type tt_raw_cells.

    load_shared_strings( ).
    lt_raw_cells = parse_worksheet( iv_name ).
    rt_cells     = convert_raw_cells( lt_raw_cells ).
  endmethod.


  method get_sheets.

    if mt_sheets is initial.
      data lo_xml_doc type ref to if_ixml_document.
      lo_xml_doc = zcl_xlsxreader_xml_utils=>parse_xmldoc( mo_workbook->get_data( ) ).
      zcl_xlsxreader_xml_utils=>children_to_table(
        exporting
          io_node = lo_xml_doc->find_from_name_ns(
            name = 'sheets'
            uri  = c_openxml_namespace_uri )
        importing
          et_tab = mt_sheets ).
    endif.

    rt_sheets = mt_sheets.

  endmethod.


  method get_sheet_names.

    data lt_sheets like mt_sheets.
    field-symbols <s> like line of lt_sheets.

    lt_sheets = get_sheets( ).

    loop at lt_sheets assigning <s>.
      append <s>-name to rt_sheet_names.
    endloop.

  endmethod.


  method get_styles.

    data lo_style_part type ref to cl_xlsx_stylespart.
    data lo_xml_doc type ref to if_ixml_document.
    data lo_node type ref to if_ixml_node.
    lo_style_part = mo_workbook->get_stylespart( ).
    lo_xml_doc    = zcl_xlsxreader_xml_utils=>parse_xmldoc( lo_style_part->get_data( ) ).

    data lt_num_formats type tt_num_formats.
    lo_node = lo_xml_doc->find_from_name_ns(
      name = 'numFmts'
      uri  = c_openxml_namespace_uri ).
    if lo_node is bound.
      zcl_xlsxreader_xml_utils=>children_to_table(
        exporting
          io_node = lo_node
        importing
          et_tab = lt_num_formats ).
    endif.
    add_default_num_formats( changing ct_num_formats = lt_num_formats ).

    data lt_cell_styles type tt_cell_styles.
    lo_node = lo_xml_doc->find_from_name_ns(
      name = 'cellXfs'
      uri  = c_openxml_namespace_uri ).
    if lo_node is bound.
      zcl_xlsxreader_xml_utils=>children_to_table(
        exporting
          io_node = lo_node
        importing
          et_tab = lt_cell_styles ).
    endif.

    data lt_num_formats_sorted type ts_num_formats.
    field-symbols <cell_style> like line of lt_cell_styles.
    field-symbols <num_format> like line of lt_num_formats_sorted.
    field-symbols <style> like line of rt_styles.

    lt_num_formats_sorted = lt_num_formats.
    append initial line to rt_styles. " Default standard style
    append initial line to rt_styles. " Default standard style 2 ???

    loop at lt_cell_styles assigning <cell_style>.
      append initial line to rt_styles assigning <style>.
      read table lt_num_formats_sorted assigning <num_format> with key numfmtid = <cell_style>-numfmtid.
      if sy-subrc = 0.
        <style>-num_format = <num_format>-formatcode.
      endif.
    endloop.

  endmethod.


  method load.
    create object ro_instance
      exporting
        iv_xdata = iv_xdata.
  endmethod.


  method load_shared_strings.

    if lines( mt_shared_strings ) > 0.
      return.
    endif.

    data lo_shared_st type ref to cl_xlsx_sharedstringspart.
    data lo_xml_doc type ref to if_ixml_document.

    lo_shared_st = mo_workbook->get_sharedstringspart( ).
    lo_xml_doc   = zcl_xlsxreader_xml_utils=>parse_xmldoc( lo_shared_st->get_data( ) ).
    zcl_xlsxreader_xml_utils=>children_to_table(
      exporting
        io_node = lo_xml_doc->find_from_name_ns(
          name = 'sst'
          uri  = c_openxml_namespace_uri )
        iv_value_to = '*'
        iv_no_attributes = abap_true
      importing
        et_tab = mt_shared_strings ).

  endmethod.


  method parse_worksheet.

    data ls_sheet like line of mt_sheets.
    data lo_worksheet type ref to cl_xlsx_worksheetpart.
    data lo_xml_doc type ref to if_ixml_document.
    data ls_context type ty_parsing_context.

    read table mt_sheets into ls_sheet with table key name = iv_name.
    if sy-subrc ne 0.
      raise exception type cx_openxml_not_found.
    endif.

    ls_context-stage = 'row'.
    get reference of rt_raw_cells into ls_context-data.

    lo_worksheet ?= mo_workbook->get_part_by_id( ls_sheet-id ).
    lo_xml_doc    = zcl_xlsxreader_xml_utils=>parse_xmldoc( lo_worksheet->get_data( ) ).
    zcl_xlsxreader_xml_utils=>iterate_children(
      io_node = lo_xml_doc->find_from_name_ns(
        name = 'sheetData'
        uri  = c_openxml_namespace_uri )
      i_context = ls_context
      ii_item_processor = me ).

  endmethod.


  method zif_xlsxreader_node_processor~process_node.

    field-symbols <context> type ty_parsing_context.
    field-symbols <cells> type tt_raw_cells.
    field-symbols <c> like line of <cells>.
    assign i_context to <context>.

    case <context>-stage.
      when 'row'.
        data lo_row_element type ref to if_ixml_element.
        data lv_row type i.
        data lt_row_cells type tt_raw_cells.
        data ls_row_context type ty_parsing_context.

        ls_row_context-stage = 'cell'.
        get reference of lt_row_cells into ls_row_context-data.

        zcl_xlsxreader_xml_utils=>iterate_children(
          io_node   = io_node
          i_context = ls_row_context
          ii_item_processor = me ).

        lo_row_element ?= io_node.
        lv_row = lo_row_element->get_attribute_ns( 'r' ).
        loop at lt_row_cells assigning <c>.
          <c>-row = lv_row.
        endloop.

        assign <context>-data->* to <cells>.
        append lines of lt_row_cells to <cells>.

      when 'cell'.
        assign <context>-data->* to <cells>.
        append initial line to <cells> assigning <c>.
        <c>-s = -1. " undefined style
        zcl_xlsxreader_xml_utils=>attributes_to_struc(
          exporting
            io_node     = io_node
            iv_no_clear = abap_true
          importing
            es_struc = <c> ).

        data lo_element type ref to if_ixml_element.
        data lo_value_node type ref to if_ixml_node.
        lo_element ?= io_node.
        lo_value_node = lo_element->find_from_name_ns(
          name = 'v'
          uri  = c_openxml_namespace_uri ).
        if lo_value_node is bound.
          <c>-value = lo_value_node->get_value( ).
        endif.

      when others.
        assert 1 = 0.
    endcase.

  endmethod.
ENDCLASS.
