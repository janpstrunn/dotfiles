#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__pass-backup.sh

pass_dir=${PASSWORD_STORE_DIR:-$HOME/.password-store}

function gpg_export() {
  read -p "Insert your email: " email
  gpg --export --armor "$email" >"$HOME/public.gpg"
  gpg --export-secret-keys --armor "$email" >"$HOME/private.gpg"
}

function gpg_import() {
  gpg --import "$HOME/public.gpg"
  gpg --import "$HOME/private.gpg"
}
function exportpass() {
  tar -czvf "$HOME/pass-bak.tar" "$pass_dir" "$HOME/private.gpg" "$HOME/public.gpg"
  openssl enc -aes-256-cbc -e -in "$HOME/pass-bak.tar" -out "$HOME/pass-bak.tar.enc" -iter 10000
  shred -u "$HOME/pass-bak.tar"
}

function importpass() {
  openssl enc -aes-256-cbc -d -in "$HOME/pass-bak.tar.enc" -out "$HOME/pass-bak.tar" -iter 10000
  tar -xzvf "$HOME/pass-bak.tar"
}

case "$1" in
"export")
  gpg_export
  exportpass
  ;;
"import")
  importpass
  gpg_import
  ;;
"export-gpg")
  gpg_export
  ;;
"import-gpg")
  gpg_import
  ;;
"")
  echo "Available options:"
  echo "export                    - Export gpg keys and pass, encrypts over openssl"
  echo "import                    - Import gpg keys and pass"
  echo "export-gpg                - Export only gpg keys"
  echo "import-gpg                - Import only gpg keys"
  ;;
esac
