#/bin/sh
pushd ../zlib
./mk.sh
popd
pushd ../zios
./mk.sh
popd

# Assemble the library modules to be relocatable.
if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version"
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./charset.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./breakpoint.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdaddr.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdutils.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmddump.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdload.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmddtime.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdcfg.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdpage.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdexec.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdhist.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdregs.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdsdimg.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdsdboot.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdsd.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdtab.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdsdmap.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdsdwrite.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdhw.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdfill.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdio.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdmodify.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdhelp.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./more.asm
else
  echo "Building RELEASE version"
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./charset.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./breakpoint.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdaddr.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdutils.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmddump.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdload.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmddtime.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdcfg.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdpage.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdexec.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdhist.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdregs.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdsdimg.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdsdboot.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdsd.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdtab.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdsdmap.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdsdwrite.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdhw.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdio.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdmodify.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdhelp.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./more.asm
fi

ld80 -o ./loader.tmp -P 0040 -D 3400 -O ihex -s - -m -S 2048 \
        ./zout/loader.rel \
        ./zout/disassembler.rel \
        ./zout/breakpoint.rel \
        ./zout/charset.rel \
        ./zout/sdaddr.rel \
        ./zout/sdutils.rel \
        ./zout/cmddump.rel \
        ./zout/cmdload.rel \
        ./zout/cmddtime.rel \
        ./zout/cmdcfg.rel \
        ./zout/cmdpage.rel \
        ./zout/cmdexec.rel \
        ./zout/cmdhist.rel \
        ./zout/cmdregs.rel \
        ./zout/cmdsdimg.rel \
        ./zout/cmdsdboot.rel \
        ./zout/cmdsd.rel \
        ./zout/cmdtab.rel \
        ./zout/cmdsdmap.rel \
        ./zout/cmdsdwrite.rel \
        ./zout/cmdhw.rel \
        ./zout/cmdfill.rel \
        ./zout/cmdio.rel \
        ./zout/cmdmodify.rel \
        ./zout/cmdhelp.rel \
        ./zout/more.rel \
        -P C400 -D C000 \
        ../zlib/zout/libsio.rel \
        ../zios/zout/pcb.rel \
        ../zios/zout/nvram.rel \
        ../zios/zout/init.rel \
        ../zios/zout/mempage.rel \
        ../zios/zout/drive.rel \
        ../zlib/zout/libutils.rel \
        ../zlib/zout/libcmd.rel \
        ../zlib/zout/libconin.rel \
        ../zlib/zout/libconout.rel \
        ../zlib/zout/libspi.rel \
        ../zlib/zout/libsdc.rel \
        ../zlib/zout/libi2c.rel \
        ../zlib/zout/librtc.rel \
        ../zlib/zout/libmisc.rel \
        ../zios/zout/services.rel \
        ../zios/zout/devmap.rel \
        ../zios/zout/sdblk.rel \
        ../zios/zout/process.rel \
        ../zios/zout/run.rel \
        -P FE00 \
        ../zios/zout/appl.rel \

node ../tools/hextform --fix --move=C000,FFFF,4000 loader.tmp > loader.hex
rm ./loader.tmp
