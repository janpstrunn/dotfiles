#!/bin/env bash

# Variables

## Obsidian Path

ouroboros='/run/media/janpstrunn/Pandora/obsidian/OUROBOROS'

## External Drive Path

filen='/run/media/janpstrunn/Leviathan/Filen/'
ente='/run/media/janpstrunn/Leviathan/Ente/'
mega='/run/media/janpstrunn/Leviathan/MEGA/'

leviathan='/run/media/janpstrunn/Leviathan/'
pandora='/run/media/janpstrunn/Pandora/'

beelzebub='/run/media/janpstrunn/B3LZ33BUB/'
seth='/run/media/janpstrunn/S3TH'

# SQL

sqlupdate() {
  python $pandora/data/sql/sqlitedb.py
}

sqlfunction() {
  python $pandora/data/sql/sqlite-function.py
}

# Encryption

qlt() {
  gocryptfs $leviathan/Vault/ $HOME/Vault/
}

# Volume

changevol() {
  pactl set-sink-volume @DEFAULT_SINK@ +$1%
}

# Pacman

update() {
  sudo pacman -Syy
}

upgrade() {
  sudo pacman -Syu
}

remove() {
  sudo pacman -Rns
}

install() {
  sudo pacman -S
}

# Networking

nethunter() {
  nmap -v -sn 192.168.0.1/24 | grep -v "host down"
}

# Utilities

alias ls='ls --color=auto'
alias grep='grep --color-auto'
alias nv='nvim'

weather() {
  curl http://wttr.in
}

ctrash() {
  gio trash --empty
}

# Taskwarrior

tk() {
  task limit:3;
}

# Git

git-master() {
  python $HOME/git-master.py;
}

quickgit() {
  git add -A;
  git commit -a -m "$1";
  git push origin main;
}

taskgit() {
  git -C "$HOME/.task/" add -A;
  git -C "$HOME/.task/" commit -a -m 'Auto Update';
  git -C "$HOME/.task/" push origin main;

}

# Quick Capture

alias qc="echo '$1' >> $ouroboros/Ideas/Inbox.md"
alias qt="echo '- [ ] $1' >> $ouroboros/Ideas/Inbox.md"
alias qd="echo '**$(date "+%H:%M")** $1' >> $ouroboros/Ideas/Inbox.md"

# Dotfiles Sync

configsync() {
  rsync -av --delete $HOME $pandora/dotfiles/
}

# Syncing External Drives

syncbeelzebub() {
  rsync -av --delete $pandora $beelzebub/Pandora/;
  rsync -av --delete $filen $beelzebub/Filen/;
  rsync -av --delete $ente $beelzebub/Ente/;
}

syncseth() {
  rsync -av --delete $pandora $seth/Pandora/;
  rsync -av --delete $filen $seth/Filen/;
  rsync -av --delete $ente $seth/Ente/;
}

# Gitea

backupgitea() {
  sudo tar -czvf $leviathan/Filen/gitea-var.tar.gz /var/lib/gitea/;
  sudo tar -czvf $leviathan/Filen/gitea-etc.tar.gz /etc/gitea/;
}

# Experimental

function create_tmux_session() {
  local RESULT="$1"
  local SESSION_NAME=$(basename "$RESULT" | tr ' .:' '_')
  tmux new-session -d -s "$SESSION_NAME" -c "$RESULT"
  tmux attach -t "$SESSION_NAME"
}

function plink() {
  link=$(xclip -o -sel clipboard)
  desc="$*"
  command_name="xdg-open \\\"$link\\\""
  description="Link to $desc"
  tag="link"
  /usr/bin/expect <<EOF
    spawn pet new -t
    expect "Command>"
    send "${command_name}\r"
    expect "Description>"
    send "${description}\r"
    expect "Tag>"
    send "${tag}\r"
    expect eof
EOF
}

function repo() {
  export repo=$(fd . ${HOME}/dev --type=directory --max-depth 1 --color always | fzf --ansi --preview "onefetch /home/$USER/dev/{1}")

  [[ -n "$repo" ]] && cd $repo
}

function pkill() {
  ps aux | fzf --height 40% --layout=reverse --prompt="Select process to kill: " | awk '{print $2}' | xargs -r kill
}
