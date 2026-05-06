# Thin wrapper: runs setup.R with Rscript and forwards arguments.
# Usage: .\setup.ps1            # full install
#        .\setup.ps1 -DryRun    # preview without installing (use --dry-run)
$ErrorActionPreference = "Stop"
Set-Location $PSScriptRoot
& Rscript setup.R @args
exit $LASTEXITCODE
