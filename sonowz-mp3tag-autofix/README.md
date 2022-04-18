# MP3 Tag Autofix

- Unify artist names
- Change tag encodings to UTF-8

# How it works

1. Select working directory by executable argument (`./sonowz-mp3tag-autofix /path/to/mp3`)
2. Set encoding to UTF-8 (optional)
2. Searches Melon with song title & artist as keyword
3. Generates fixes of artist names with ad-hoc algorithm
4. Manually select fixes to ignore (interactive console: `Select indices of fix to exclude (leave empty to end selection):`)
5. Confirm updating tag to files
6. Writes fixed tags to files
