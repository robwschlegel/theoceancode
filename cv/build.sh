#!/usr/bin/env bash
# Render cv/cv.html to static/files/cv.pdf (A4, must stay one page).
set -euo pipefail
cd "$(dirname "$0")"
google-chrome --headless=new --disable-gpu --no-pdf-header-footer \
  --print-to-pdf="../static/files/cv.pdf" "file://$PWD/cv.html" 2>/dev/null
pdfinfo ../static/files/cv.pdf | grep Pages
