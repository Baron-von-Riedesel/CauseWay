
 About

  cwStub2.exe may replace the standard cwstub.exe. Unlike cwstub.exe,
 which actually IS the CauseWay extender (compressed), cwstub2.exe is
 just bootstrap code that searches the (uncompressed) CauseWay extender
 CW32.EXE in the current directory and in the directories contained
 in the PATH environment variable.
  The advantage of using cwstub2.exe is that its size is less than 512
 bytes, while cwstub.exe is about 40 kB. The disadvantage is that the
 bound binary isn't self-contained anymore.
