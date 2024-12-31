
 About

 This repository contains the CauseWay DOS Extender, both v3.xx and v4.0x
 (Open Watcom).
 
 The sources are in Masm v6 syntax. Some bugs have been fixed, in both
 the extender and the debugger. Also, the extender does now support the
 kernel debugger API of DebugR, so it's possible to debug it at privilege
 level 0 (using Deb386, or possibly Microsoft's WDeb386). Some optimizations
 have been done, data has been arranged to avoid unaligned access and
 binary size is significantly reduced.

 WarpLink and the CauseWay linker WL32 are in a separate repository.
 
 Tools that are used to create the binaries are Masm/JWasm and WL32. At
 least the extender itself can also be created with JWasm's -mz option,
 thus avoiding a separate link step. Finally, the binaries are compressed,
 using the CauseWay tool CMC.

 The main source parts are:

 CW    : CauseWay EXE maker
 CW349 : the CauseWay Extender v3.xx ( 3.49/3.52? )
 CW404 : the CauseWay Extender v4.0x ( supplied with Open Watcom )
 CWD   : the CauseWay Debugger
 CWL   : (internal) CauseWay Linker ( obsolete )
 CWC   : CauseWay File Compressor
 CWLIB : library modules to support CauseWay assembly programs
 LOADLE: CauseWay support for LE binary format (v3.xx)

 The CauseWay DOS Extender is in the Public Domain. It was a commercial
 product written mainly by Michael Devore in the early 1990s.

