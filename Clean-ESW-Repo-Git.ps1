# Script para limpiar archivos del historial de Git usando git filter-branch
# Ejecutar desde la carpeta raiz del repositorio ESW_HUD

$RepoPath = Get-Location
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Limpieza de ESW_HUD - Removiendo archivos" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Verificar que estamos en un repositorio git
if (!(Test-Path ".git")) {
    Write-Host "ERROR: No se encontro la carpeta .git" -ForegroundColor Red
    Exit 1
}

Write-Host "Repositorio: $RepoPath" -ForegroundColor Green
Write-Host ""

# Archivos a remover
$files = @(
    "docs/BID_ESW_nota tecnica_v3_peer review.docx",
    "docs/Nota tecnica - HUD-HNP Draft.pdf",
    "docs/Nota tecnica - metodologia y resultado - draft- NGB .pdf",
    "docs/Nota tecnica.docx"
)

Write-Host "Archivos a remover:" -ForegroundColor Yellow
foreach ($file in $files) {
    Write-Host "  - $file"
}
Write-Host ""

# Backup
$timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
$backupPath = ".\.git_backup_$timestamp"
Write-Host "Creando backup..." -ForegroundColor Cyan
Copy-Item -Path ".git" -Destination $backupPath -Recurse -Force
Write-Host "Backup: $backupPath" -ForegroundColor Green
Write-Host ""

Write-Host "Ejecutando git filter-branch..." -ForegroundColor Cyan
Write-Host "(Puede tomar tiempo...)" -ForegroundColor Gray
Write-Host ""

# Construir el comando de git filter-branch
$rmCommand = ""
foreach ($file in $files) {
    $rmCommand += "rm -f '$file';"
}

# Ejecutar filter-branch
git filter-branch --force --index-filter "$rmCommand" --prune-empty --tag-name-filter cat -- --all

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "SUCCESS - Archivos removidos del historial!" -ForegroundColor Green
    Write-Host ""

    # Limpiar referencias
    Write-Host "Limpiando referencias locales..." -ForegroundColor Cyan
    Remove-Item -Path ".git\refs\original\" -Recurse -Force -ErrorAction SilentlyContinue
    git reflog expire --expire=now --all
    git gc --prune=now --aggressive
    Write-Host "Limpieza completada." -ForegroundColor Green
}
else {
    Write-Host ""
    Write-Host "ERROR - git filter-branch fallo" -ForegroundColor Red
    Exit 1
}

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "PROXIMO PASO: Push forzado" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Ejecuta estos comandos:" -ForegroundColor Yellow
Write-Host ""
Write-Host "  git push origin --force --all" -ForegroundColor Cyan
Write-Host "  git push origin --force --tags" -ForegroundColor Cyan
Write-Host ""
