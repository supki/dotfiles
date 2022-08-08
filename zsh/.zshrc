declare _cfg_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
mkdir -p "$_cfg_cache_dir"
fpath=(~/.nix-profile/share/zsh/site-functions $fpath)

zstyle ':completion:*' auto-description '%B%d:%b'
zstyle ':completion:*' completer _expand _complete _ignored _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format '%B%d:%b'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/m/.zshrc'

autoload -Uz compinit
compinit -d "${_cfg_cache_dir}/zcompdump-${ZSH_VERSION}"
HISTFILE=${_cfg_cache_dir}/.zhistory # FIXME: maybe there's a better place for this file?
HISTSIZE=10000
SAVEHIST=10000
setopt autocd extendedglob
bindkey -e

alias g=git
alias ls='ls --color -1'
alias z=zellij

bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward
bindkey '\e' pound-insert
bindkey "^z" _fg

setopt autopushd histignorealldups histignorespace interactivecomments prompt_subst

autoload -Uz promptinit vcs_info
promptinit

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr 'M'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:git:*' formats "%F{1}%u%f%F{2}%c%f %F{218}%b%f"
zstyle ':vcs_info:svn:*' formats "%F{3}%b%f"
zstyle ':vcs_info:*' enable git svn

PROMPT="%M:%F{218}%~%f"$'$(signal_wrapper)'"$prompt_newline%(?,%F{green},%F{red})%#%f "
PROMPT2="%F{blue}%_%f> "
RPROMPT="%{"$'\e[1A'"%}"$'$(vcs_info_wrapper)'' ''$(nix_env_wrapper)'"%{"$'\e[B'"%"
WORDCHARS=${WORDCHARS//\//}

function signal_wrapper {
  declare val=$?
  if ((val > 128)); then
    echo " (\$? is %F{red}$(kill -l "$val")%f)"
  elif ((val > 0 )); then
    echo " (\$? is %F{red}${val}%f)"
  fi
}

function vcs_info_wrapper {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

function nix_env_wrapper {
  if [ -n "${IN_NIX_SHELL:-}" ]; then
    if [ "$name" = 'shell' ]; then
      echo -n "[no-project]"
    else
      echo -n "[${name}]"
    fi
  fi
}

function _fg {
  fg
}
zle -N _fg

function mkcdir {
  mkdir --parents "$1" && cd "$_"
}

function databagify {
  perl -p -e 's/\R/\\n/g' "$@"
}

function n {
  nix-shell --command "$*"
}

function e {
  if [ -f shell.nix ]; then
    n nvim "$@"
  else
    nvim "$@"
  fi
}

function stack-template {
  declare -r name=$1
  echo "${XDG_CONFIG_HOME:-$HOME/.config}/stack/${name}.hsfiles"
}

for ext in ~/.nix-profile/share/zsh/zsh-*/*.zsh~*plugin.zsh; do
  source "$ext"
done

unset _cfg_cache_dir

export LOCALE_ARCHIVE=/lib/locale/locale-archive # fix nix locale complaints
export CURL_HOME=${XDG_CONFIG_HOME:-$HOME/.config}/curl

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
