@echo off
REM =====================================================================
REM Shiny ML ^& Forecasting Lab - launcher (Windows)
REM =====================================================================
REM Usage:
REM   run.bat                 install (first time) + launch on 4848
REM   run.bat --port 5050     custom port
REM   run.bat --skip-install  do not check R packages
REM =====================================================================
setlocal EnableDelayedExpansion

set PORT=4848
set HOST=127.0.0.1
set SKIP_INSTALL=0

:parse_args
if "%~1"=="" goto args_done
if /I "%~1"=="--port" (
  set PORT=%~2
  shift & shift & goto parse_args
)
if /I "%~1"=="--host" (
  set HOST=%~2
  shift & shift & goto parse_args
)
if /I "%~1"=="--skip-install" (
  set SKIP_INSTALL=1
  shift & goto parse_args
)
echo Unknown option: %~1
exit /b 2
:args_done

REM ---- Resolve script directory ----
set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
cd /d "%SCRIPT_DIR%"
set SHINYML_HOME=%SCRIPT_DIR%

REM ---- Locate Rscript ----
where Rscript >nul 2>&1
if errorlevel 1 (
  echo ERROR: Rscript not found on PATH.
  echo Install R from https://cran.r-project.org/bin/windows/base/
  echo Then re-run this script from a NEW terminal so PATH refreshes.
  exit /b 1
)

for /f "delims=" %%v in ('Rscript -e "cat(paste(R.version$major, R.version$minor, sep='.'))"') do set R_VERSION=%%v
echo [run.bat] R %R_VERSION%

REM ---- Install dependencies (idempotent) ----
if "%SKIP_INSTALL%"=="0" (
  echo [run.bat] Verifying R packages... (skip with --skip-install)
  Rscript "%SCRIPT_DIR%\install_R.R"
  if errorlevel 1 exit /b 1
)

REM ---- Launch ----
echo [run.bat] Launching Shiny on http://%HOST%:%PORT%
Rscript -e "options(shiny.port=%PORT%, shiny.host='%HOST%'); Sys.setenv(SHINYML_HOME=normalizePath('%SCRIPT_DIR%')); shiny::runApp('%SCRIPT_DIR%', launch.browser = TRUE)"

endlocal
