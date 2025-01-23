
 About

 This repository contains the CauseWay DOS Extender v5.0. Its origin are
 the CauseWay source files supplied with Open Watcom ( v4.05 ). The extender
 and the debugger are significantly modified - for details see files
 History.txt in directories CW5 and CWD.
 
 The sources are in Masm v6 syntax. Tools that are used to create the
 binaries are Masm/JWasm and WL32. At least the extender itself can also be
 created with JWasm's -mz option, thus avoiding a separate link step. Finally,
 the binaries are compressed, using the CauseWay compressor CMC.

 The CauseWay linker WL32 (and the 8086 linker WarpLink) are in a separate
 repository.
 
 The main source parts are:

 CW    : CauseWay EXE maker
 CW5   : CauseWay Extender v5 ( origin: v4.05 - supplied with Open Watcom )
 CWD   : the CauseWay Debugger
 CWL   : (internal) CauseWay Linker - obsolete
 CWC   : CauseWay File Compressor
 CWLIB : library modules to support CauseWay assembly programs

