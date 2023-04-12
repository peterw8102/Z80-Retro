import defs.asm

  ; Symbols exported from ZLIB to clients.
  extrn  PRINT,PRINT_LN,PRINT_80,NL,GET_LINE,SET_LINE,EDT_LINE
  extrn  SKIPSPC,WASTESPC,BUFCHR,BUFCHUP,UNGET,RESINP
  extrn  WRITE_D,WRITE_8,WRITE_16,INHEX,INHEX_2,INHEX_4
  extrn  BRK_HK,SETHIST,GETHIST
  extrn  CMD_B

  extrn  GET_HEX,GET_DEC,INPTR,INBUF,INITSIO,RXA,TXA,CKINCHAR,SERINT
  extrn  DEC2BIN
  extrn  SD_PRES
  extrn  SW_CFG

  ; And the RTC/i2c library
  extrn  RTC_INI, RTC_GET, RTC_SET

  ; Utilities
  extrn  STRCMP,ADD8T16,TOUPPER,DEC2BIN,HEX2BIN,BIN2HEX

  ; Flash writing functions
  extrn  FL_ID,FL_CSECT,FL_CPAGE,FL_WSECT,FL_WPAGE

  ; Video card
  extrn  V_INIT,V_CENABLE,V_CTOG,V_PRT

  ; Keyboard
  extrn  KBDINIT,KBDSCAN,KBDCHAR,KBDCHK
