# Script para limpiar archivos sensibles del historial de Git
# Ejecutar desde la carpeta raiz del repositorio ESW_HUD

$RepoPath = Get-Location
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Limpieza de ESW_HUD - Removiendo archivos del historial" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Verificar que estamos en un repositorio git
if (!(Test-Path ".git")) {
    Write-Host "ERROR: No se encontro la carpeta .git" -ForegroundColor Red
    Write-Host "Ejecuta este script en la carpeta raiz del repositorio." -ForegroundColor Red
    Exit 1
}

Write-Host "Repositorio encontrado: $RepoPath" -ForegroundColor Green
Write-Host ""

# Archivos a remover
$filesToRemove = @(
    "docs/BID_ESW_nota tecnica_v3_peer review.docx",
    "docs/Nota tecnica - HUD-HNP Draft.pdf",
    "docs/Nota tecnica - metodologia y resultado - draft- NGB .pdf",
    "docs/Nota tecnica.docx"
)

Write-Host "Archivos a remover:" -ForegroundColor Yellow
foreach ($file in $filesToRemove) {
    Write-Host "  - $file"
}
Write-Host ""

# Backup
$timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
$backupPath = ".\.git_backup_$timestamp"
Write-Host "Creando backup..." -ForegroundColor Cyan
Copy-Item -Path ".git" -Destination $backupPath -Recurse
Write-Host "Backup guardado: $backupPath" -ForegroundColor Green
Write-Host ""

# Instalar git-filter-repo si no existe
Write-Host "Verificando git-filter-repo..." -ForegroundColor Cyan
$check = python -m pip show git-filter-repo 2>&1 | Select-String "Name"
if ($null -eq $check) {
    Write-Host "Instalando git-filter-repo..." -ForegroundColor Yellow
    python -m pip install git-filter-repo --quiet
    Write-Host "Instalado." -ForegroundColor Green
}
else {
    Write-Host "Ya esta instalado." -ForegroundColor Green
}
Write-Host ""

# Crear archivo temporal con lista de archivos
$tempFile = [System.IO.Path]::GetTempFileName()
$filesToRemove | Out-File -FilePath $tempFile -Encoding UTF8

Write-Host "Ejecutando git filter-repo..." -ForegroundColor Cyan
Write-Host "(Esto puede tomar tiempo...)" -ForegroundColor Gray
Write-Host ""

# Ejecutar filter-repo
git filter-repo --invert-paths --paths-from-file $tempFile

if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "SUCCESS - Archivos removidos del historial!" -ForegroundColor Green
}
else {
    Write-Host ""
    Write-Host "ERROR - git filter-repo fallo" -ForegroundColor Red
    Remove-Item -Path $tempFile -Force
    Exit 1
}

Remove-Item -Path $tempFile -Force

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "PROXIMO PASO: Push forzado" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Ejecuta en PowerShell:" -ForegroundColor Yellow
Write-Host ""
Write-Host "  git push origin --force --all" -ForegroundColor Cyan
Write-Host "  git push origin --force --tags" -ForegroundColor Cyan
Write-Host ""
Write-Host "ADVERTENCIA: Esto sobrescribe el historial remoto!" -ForegroundColor Red
Write-Host ""
