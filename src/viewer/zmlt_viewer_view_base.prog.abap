class lcl_view_base definition abstract.
  public section.
    methods display abstract
      raising lcx_error.

    methods on_user_command abstract
      importing
        iv_cmd type char70
      returning
        value(rv_processed) type abap_bool.
    methods on_output.
    methods close.

  protected section.
    data mo_alv type ref to cl_salv_table.

    class-methods set_columns
      importing
        io_alv type ref to cl_salv_table.

endclass.

class lcl_view_base implementation.
  method on_output.
  endmethod.

  method set_columns.

    data lo_cols type ref to cl_salv_columns.
    data lt_columns type salv_t_column_ref.

    lo_cols = io_alv->get_columns( ).
    lo_cols->set_optimize( abap_true ).
    lt_columns = lo_cols->get( ).

    field-symbols <c> like line of lt_columns.
    loop at lt_columns assigning <c>.
      <c>-r_column->set_short_text( |{ <c>-columnname }| ).
      <c>-r_column->set_medium_text( |{ <c>-columnname }| ).
      <c>-r_column->set_long_text( |{ <c>-columnname }| ).
    endloop.

  endmethod.

  method close.
    mo_alv->close_screen( ).
  endmethod.

endclass.
