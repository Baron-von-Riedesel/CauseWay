erase cwnoexvm.exe
nmake /a /f noexvm.mak all
copy /b cw32.exe+.\cw\cw.cw cwev.exe
ren cw32.exe cwnoexvm.exe
