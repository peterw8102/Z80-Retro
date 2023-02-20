  extrn  NVRD,NVLD,NVSAV,NVRAM
  extrn  ZIOS_INI

  ; From mempage.asm
  extrn P_ALLOC,P_FREE,P_RES,P_MIN
  extrn P_ADJ,P_RESTX,P_REST,P_MAP,P_MAPX
  extrn _pages

  ; From process control
  extrn PR_INIT,PR_RUN,PR_REST

  ; Running code
  extrn INSTDRV

  ; SDCard mapping utilities imported from 'drive.asm'
  extrn  SDTXLTD
  extrn  SDMPADD
  extrn  SDMPRAW
  extrn  SDPREP
