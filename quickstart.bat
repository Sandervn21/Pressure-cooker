@echo off
rem One command to set up and run everything on Windows: double-click quickstart.bat or run it in a terminal.
rem Checks for R, then quickstart.R does the rest (packages, Quarto, data, rendering).
cd /d "%~dp0"

:find_r
set "RSCRIPT="
where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
rem R is usually not on the PATH on Windows, so also look in the default install folders
if not defined RSCRIPT for /d %%d in ("%ProgramFiles%\R\R-*" "%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%~d\bin\Rscript.exe" set "RSCRIPT=%%~d\bin\Rscript.exe"
if not defined RSCRIPT (
  echo R is not installed. Install R from https://cran.r-project.org/.
  echo Press any key when it is installed, or close this window to stop.
  pause >nul
  goto find_r
)

"%RSCRIPT%" quickstart.R
pause
