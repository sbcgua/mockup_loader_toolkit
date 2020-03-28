**********************************************************************
* EXCEPTIONS
**********************************************************************

class lcx_error definition inheriting from cx_static_check.
  public section.
    interfaces if_t100_message.
    types ty_rc type c length 4.
    data msg type string read-only.
    data loc type string read-only.
    data rc type ty_rc read-only.

    methods constructor
      importing
        msg type string
        loc type string optional
        rc type ty_rc optional.

    class-methods raise
      importing
        msg type string
        loc type string optional
        rc type ty_rc optional
      raising
        lcx_error.
endclass.

class lcx_error implementation.

  method constructor.
    super->constructor( ).
    me->msg = msg.
    me->rc  = rc.
    me->if_t100_message~t100key-msgid = 'SY'. " & & & &
    me->if_t100_message~t100key-msgno = '499'.
    me->if_t100_message~t100key-attr1 = 'MSG'.
    me->if_t100_message~t100key-attr2 = 'LOC'.
  endmethod.

  method raise.
    raise exception type lcx_error
      exporting
        msg = msg
        loc = loc
        rc  = rc.
  endmethod.

endclass.
