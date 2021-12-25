class ZCX_MLT_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  types:
    ty_rc type c length 4 .

  constants:
    begin of ZCX_MLT_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of ZCX_MLT_ERROR .
  data MSG type STRING read-only .
  data LOC type STRING read-only .
  data RC type TY_RC read-only .
  data A1 type SYMSGV read-only .
  data A2 type SYMSGV read-only .
  data A3 type SYMSGV read-only .
  data A4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING optional
      !LOC type STRING optional
      !RC type TY_RC optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !MSG type STRING
      !LOC type STRING optional
      !RC type TY_RC optional
    raising
      ZCX_MLT_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MLT_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSG = MSG .
me->LOC = LOC .
me->RC = RC .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MLT_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method raise.

    data lv_msg type string.
    data:
      begin of ls_split_msg,
        a1 like a1,
        a2 like a1,
        a3 like a1,
        a4 like a1,
      end of ls_split_msg.


    if loc is initial.
      ls_split_msg = msg.
    else.
      lv_msg = |{ msg } @{ loc }|.
      ls_split_msg = lv_msg.
    endif.

    raise exception type zcx_mlt_error
      exporting
        msg = msg
        loc = loc
        rc  = rc
        a1  = ls_split_msg-a1
        a2  = ls_split_msg-a2
        a3  = ls_split_msg-a3
        a4  = ls_split_msg-a4.

  endmethod.
ENDCLASS.
