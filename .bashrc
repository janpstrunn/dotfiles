# ~/.bashrc
[[ $- != *i* ]] && return

HISTTIMEFORMAT="%F %T "

set -o vi

source $HOME/.commands.sh

export EDITOR=nvim
export VISUAL=nvim
export PATH=$HOME/Scripts/:$PATH

eval "$(fzf --bash)"
eval "$(starship init bash)"
