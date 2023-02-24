

   extrn BP,NSTEP,SET_RGS,SSTEP,GO,SDDIR,SDLOAD,SDRUN,BOOTIX,BOOT
   extrn CONFIG,DUMPM,DUMPI,DNVRAM,DTIME,DUMP,FILL,CLS,INPUT,LDF,LDH,LDT
   extrn SDMOD,MODIFY,NSTEP,OUTPUT,PAGE,DECCHR,SDUMP,SMAP,SWRITE
   extrn MAPDSK,EXDBG,SHWHIST,RUN,WBOOT,IMG,HELP

   public BDG_TABLE,CMD_TABLE


   CSEG

; Alternate command table format: LETTER:ADDRESS
BDG_TABLE:      DC       'B'
                DW        BP
                DC       'N'
                DW        NSTEP
                DC       'R'
                DW        SET_RGS
                DC       'S'
                DW        SSTEP
                DC       'G'
                DW        GO

CMD_TABLE:      DC       'BOS?'
                DW        SDDIR
                DC       'BOS-'
                DW        SDLOAD
                DC       'BOS'
                DW        SDRUN
                DC       'BO-'
                DW        BOOTIX
                DC       'BO'
                DW        BOOT
                DC       'C'
                DW        CONFIG
                DC       'DM'
                DW        DUMPM
                DC       'DI'
                DW        DUMPI
                DC       'DN'
                DW        DNVRAM
                DC       'DT'
                DW        DTIME
                DC       'D'
                DW        DUMP
                DC       'F'
                DW        FILL
                DC       'H'
                DW        CLS
                DC       'I'
                DW        INPUT
                DC       'LF'
                DW        LDF
                DC       'LH'
                DW        LDH
                DC       'L'
                DW        LDT
                DC       'MS'
                DW        SDMOD
                DC       'M'
                DW        MODIFY
                DC       'N'
                DW        NSTEP
                DC       'O'
                DW        OUTPUT
                DC       'P'
                DW        PAGE              ; Display/change application page assignment
                DC       'Q'
                DW        DECCHR
                DC       'SD'
                DW        SDUMP             ; Display SDCard sector contents
                DC       'SM'
                DW        SMAP              ; Map a logical to physical SD card.
                DC       'SW'
                DW        SWRITE            ; Write to an SDCard sesctor
                DC       'S'
                DW        MAPDSK            ; Map a logical to physical SD card.
                DC       'T'
                DW        EXDBG
                DC       '.'
                DW        SHWHIST
                DC       'G'
                DW        RUN
                DC       'WB'
                DW       WBOOT
                DC       'WI'
                DW       IMG
                DC       '?'
                DW        HELP
                DB        0
