
 About

 This repository contains the CauseWay DOS Extender v5.0. The origin of the
 extender itself are the CauseWay source files supplied with Open Watcom ( v4.05 ).
 The other parts of the source are based on CauseWay v3.49 (or maybe v3.52 - the
 documentation is a bit unclear here). The extender and the debugger have been
 significantly modified - for details see files History.txt in directories CW5
 and CWD.

 The sources are in Masm v6 syntax. Tools that are used to create the
 binaries are Masm/JWasm and WL32. At least the extender itself can also be
 created with JWasm's -mz option, thus avoiding a separate link step. Finally,
 the binaries are compressed, using the CauseWay compressor CMC.

 The CauseWay linker WL32 (and the 8086 linker WarpLink) are in a separate
 repository.

 The main source parts are:

 CW32  : CauseWay Extender v5 ( origin: v4.05 - supplied with Open Watcom )
 CWC   : CauseWay File Compressor
 CWD   : CauseWay Debugger
 CWEM  : CauseWay EXE maker
 CWL   : (internal) CauseWay Linker - obsolete
 CWLIB : library modules to support CauseWay assembly programs

