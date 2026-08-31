# ================================================================
#  Preparar commit - "ESW version 2"
#  Generado por Claude - revisa el script antes de correrlo
# ================================================================
#  Que hace (todo LOCAL, no toca GitHub, no hace commit ni push):
#   1) Verifica que la carpeta este dentro de un repo git
#   2) Chequea si hay archivos de data/ o _archivo_a_revisar/ que ya
#      estaban versionados de ANTES (importante: puede haber datos
#      sensibles ya commiteados) y los saca del tracking (git rm --cached)
#      -- esto NO borra nada del disco, solo deja de versionarlos de aca en mas
#   3) Hace "git add -A" (respeta el .gitignore)
#   4) Chequea si quedo algun archivo pesado (>90MB) adentro del commit
#      (GitHub rechaza archivos de mas de 100MB)
#   5) Te muestra el "git status" final para que lo revises vos
#
#  NO hace commit ni push. Vos decidis el mensaje del commit y cuando
#  subirlo, despues de revisar la salida de este script.
#
#  Como correrlo:
#   1) Abri PowerShell parado en la carpeta del proyecto (o dejá $Root)
#   2) .\preparar_commit.ps1
# ================================================================

$ErrorActionPreference = "Continue"

$Root = "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\Paper versión final - peer rev\ESW_HUD\ESW version 2"

if (-not (Test-Path -LiteralPath $Root)) {
    Write-Error "No encuentro la carpeta: $Root`nEdita la variable `$Root al principio del script y volve a correrlo."
    exit 1
}
Set-Location -LiteralPath $Root

# --- 0) git instalado? -------------------------------------------------
$null = git --version 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Error "No encuentro 'git' instalado o no esta en el PATH. Instala Git for Windows y volve a intentar."
    exit 1
}

# --- 1) Estamos dentro de un repo? --------------------------------------
$repoRoot = git rev-parse --show-toplevel 2>$null
if (-not $repoRoot) {
    Write-Error "Esta carpeta no parece estar dentro de un repositorio git (no encontre .git). Si todavia no lo inicializaste, corre 'git init' primero."
    exit 1
}
$repoRoot = $repoRoot -replace '/', '\'

Write-Host "================================================================"
Write-Host " Repo git: $repoRoot"
Write-Host " Branch actual: $(git branch --show-current)"
Write-Host " Remoto(s) configurado(s):"
git remote -v
Write-Host "================================================================`n"

# --- 2) Archivos de data/ o _archivo_a_revisar/ ya versionados de antes ---
Write-Host "--- Revisando si hay archivos de 'data/' o '_archivo_a_revisar/' ya commiteados de antes ---"
$trackedSensible = git ls-files | Where-Object { $_ -match '^(data/|_archivo_a_revisar/)' }

if ($trackedSensible) {
    Write-Host ""
    Write-Warning "Encontre archivos DENTRO de data/ o _archivo_a_revisar/ que ya estaban versionados en git de ANTES de este reordenamiento:"
    $trackedSensible | ForEach-Object { Write-Host "   $_" -ForegroundColor Yellow }
    Write-Host ""
    Write-Warning "Los voy a sacar del tracking (git rm --cached) para que de aca en mas NO se versionen mas. Esto NO borra los archivos de tu disco."
    Write-Warning "IMPORTANTE: si alguna vez corriste 'git push' con estos archivos adentro, pueden seguir estando en el historial y/o en GitHub aunque ahora los saque del tracking. Eso requiere una limpieza aparte (reescribir historial) - avisale a Claude si es tu caso, antes de asumir que ya estan a salvo."
    git rm -r --cached data 2>$null | Out-Null
    git rm -r --cached _archivo_a_revisar 2>$null | Out-Null
} else {
    Write-Host "  (ninguno estaba versionado de antes - bien)`n"
}

# --- 3) Staging -------------------------------------------------------------
Write-Host "`n--- Agregando cambios (git add -A) ---"
git add -A

# --- 4) Archivos pesados que hayan quedado en el commit ---------------------
Write-Host "`n--- Chequeando archivos grandes (>90MB) dentro del commit (GitHub rechaza >100MB) ---"
$staged = git diff --cached --name-only
$foundBig = $false
foreach ($f in $staged) {
    $full = Join-Path $repoRoot $f
    if (Test-Path -LiteralPath $full -PathType Leaf) {
        $size = (Get-Item -LiteralPath $full).Length
        if ($size -gt 90MB) {
            $foundBig = $true
            Write-Host ("   {0}  ({1:N1} MB)" -f $f, ($size/1MB)) -ForegroundColor Yellow
        }
    }
}
if (-not $foundBig) { Write-Host "  (ninguno - bien)" }

# --- 5) Status final para que el usuario revise ------------------------------
Write-Host "`n================================================================"
Write-Host " git status (esto es lo que se va a comitear si confirmas)"
Write-Host "================================================================"
git status

Write-Host "`n================================================================"
Write-Host " Revisa la lista de arriba con calma."
Write-Host " Si esta todo bien, comiteá vos con, por ejemplo:"
Write-Host '   git commit -m "Reorganizar estructura del repo: scripts por R/Stata, datos separados"'
Write-Host " No hice commit ni push - eso lo haces vos (o me pedis el mensaje y seguimos)."
Write-Host "================================================================"
