ALL	: WL\cwd.exe WL\cwd.ovl

WL\cwd.exe	: cwd.asm macros.inc equates.inc
 ml -c -nologo -FlWL\ -FoWL\ cwd.asm

WL\cwd.ovl	: cwd-ovl.asm disas.inc generr.inc fpu.inc disaseq.inc ..\cw.inc ..\strucs.inc win.inc evaluate.inc
 ml /c /nologo /DENGLISH=1 /FlWL\ /FoWL\ cwd-ovl.asm
 wl32 /zu /m /sy WL\cwd-ovl,WL\cwd-ovl,WL\cwd-ovl
 del WL\cwd.ovl
 ren WL\cwd-ovl.exe cwd.ovl
