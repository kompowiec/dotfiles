if [ -f ~/.bash_aliases ]; then
.  ~/.bash_aliases
fi
export PATH="$HOME/.local/bin:$PATH"
export NNTPSERVER="news.eternal-september.org"
export TERMINAL=alacritty


export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups:erasedups  # Avoid duplicates
shopt -s histappend                      # Append to the history file, don't overwrite

