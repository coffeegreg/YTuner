FOR %%i IN (..\src\sql\*.sql) DO 7z.exe a "..\res\%%~ni.zip" "%%i"
