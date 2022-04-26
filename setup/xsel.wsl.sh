#!/bin/bash

# xsel simulation in WSL
# usage:
#     xsel -o
#     xsel -i [string]
for i in "$@"; do
  case "$i" in
    -o )
      # for paste
      powershell.exe Get-Clipboard | sed 's/\r$//' | head -c -1
      exit 0
      ;;
    -i )
      # for copy
      wsl_clipboard="/tmp/wsl_clipboard"
      distro="$(lsb_release -is)"
      wsl_root_folder="//wsl$/${distro}"

      cat > "${wsl_clipboard}"
      powershell.exe "Get-Content ${wsl_root_folder}${wsl_clipboard} | Set-Clipboard"
      exit 0
      ;;
  esac
done