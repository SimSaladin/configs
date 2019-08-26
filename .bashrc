#!/bin/bash
# file: ~/.bashrc
# vim: fdm=marker fdl=0

# bash is weird, ~/.bashrc is only read when interactive and non-login;
# it reads ~/.bash_profile when shell is login. So we have:
# - login (interactive or not): bash_profile is sourced, bashrc is not (but
#   profile sources it explicitly anyways)
# - non-login & interactive: bashrc is sourced, bash_profile is not
# - non-login & non-interactive: if $BASH_ENV value points to a file, that is
#   sourced. (Example of a non-login non-interactive shell: ssh host bash -c echo)

# {{{1 Globals
if cmd=$(tty); then
  export GPG_TTY=$cmd
fi

if [[ -z $SSH_AUTH_SOCK ]]; then
  export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR:-/run/user/$UID}/gnupg/S.gpg-agent.ssh"
fi

export VAGRANT_DEFAULT_PROVIDER=libvirt

export ANSIBLE_SSH_EXECUTABLE=/usr/bin/ssh

[[ -r "$SECRETS" ]] && source "$SECRETS"

PATH="$HOME/bin:$HOME/.local/bin:$HOME/bin/lennartcl-gitl:/usr/lib/cw:$PATH"

# {{{2 XDG_*
if [[ "${XDG_DATA_DIRS:=/usr/local/share:/usr/share}" == "/usr/local/share:/usr/share" ]]; then
   export XDG_DATA_DIRS="$HOME/.nix-profile/share:${XDG_DATA_DIRS}"
fi
if [[ -z ${XDG_DATA_HOME:-} ]]; then
  export XDG_DATA_HOME="$HOME/.local/share"
fi
if [[ -z ${XDG_CONFIG_HOME:-} ]]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi
if [[ -z ${XDG_CACHE_HOME:-} ]]; then
  export XDG_CACHE_HOME="$HOME/.cache"
fi
if [[ -z ${XDG_MUSIC_DIR:-} && -d ~/music ]]; then
  export XDG_MUSIC_DIR=~/music
fi

# {{{2 less, PAGER, MANPAGER, EDITOR
if cmd=$(type -p vimpager 2>/dev/null); then
  export PAGER=$cmd
fi
if cmd=$(type -p vim 2>/dev/null); then
  EDITOR=$cmd
  MANPAGER=$cmd
  MANPAGER+=" -u ~/.vim/vimpagerrc -M +MANPAGER -"
  export EDITOR MANPAGER
fi

# {{{1 Shell setup

# {{{2 history
export HISTCONTROL="erasedups:ignoreboth"
export HISTFILESIZE=500000
export HISTSIZE=100000
export HISTIGNORE="&:[ ] *:exit"
shopt -s lithist    # multi-line commands with newlines instead of semicolons
shopt -s histappend # don't overwrite HISTFILE, append instead
shopt -s histverify # load history substitutions to readline buffer instead of parser direct (replaces current buffer contents)

# {{{2 glob, complete, correct
shopt -s no_empty_cmd_completion  # show all 5000 possibilities in PATH?
shopt -s autocd           # automatically cd to paths: $ /etc
shopt -s direxpand        # completing e.g. $HOME/<Tab> expands to canonical path
shopt -q progcomp \
  || complete -cf sudo    # If bash-completions is not provided by system, at least complete sudo
shopt -s cdspell dirspell # correct minor spelling mistakes for cd invocation and path expansion
shopt -s checkhash        # automatically rehash executables
shopt -s globstar         # enable ** and **/ recursive glob

# {{{2 prompt
if [[ ! -v __shell_prompt && -r "${SHELL_PROMPT:=$HOME/.shell_prompt.sh}" ]]; then
   source "$SHELL_PROMPT"
fi
if [[ -r "${DIRCOLORS:=$HOME/.dircolors}" ]] && _dircolors=$(type -P dircolors); then
   eval "$("$_dircolors" -b "$DIRCOLORS")"
fi

# {{{2 direnv hook
if script=$(command direnv hook bash 2>&-); then eval "$script"; fi

# {{{2 Shell completion (stack, pandoc, tmuxp)
if type -tf stack >/dev/null; then eval "$(stack --bash-completion-script stack)"; fi
if type -tf pandoc >/dev/null; then eval "$(pandoc --bash-completion)"; fi
if type -tf tmuxp >/dev/null; then eval "$(_TMUXP_COMPLETE=source tmuxp)"; fi

# {{{1 Aliases and functions

# {{{2 github & gitlab
gh(){
   path=${*: -1}
   if [[ $path != */* ]]
   then path=SimSaladin/$path
   fi
   cmd=${*: 1:$(( ${#@} - 1 ))}
   git ${cmd} https://github.com/${path}
}

# checkout GitLab merge-request
glmr(){
   git fetch origin merge-requests/"$1"/head:mr-"$1" && git checkout mr-"$1"
}

# {{{2 aliases: ls, find, du, grep, etc.
alias ls='ls -cb --color=auto --group-directories-first'
alias la='ls -A'
alias ll='ls -liZ --author'
alias lt='ll -t'
alias lal='ll -A'
alias lsdots='ls -d .*' # ls dot files only

alias findexts="find . -maxdepth 1 -type f -printf '%f\n' | sed 's|.*\.\([^.]*\)$|\1|' | sort -u"

alias mkdir='mkdir -p'
alias df='df -h'
alias dus='du -hd 1 | sort -h'

alias grep='grep --color=auto'
alias gr='grep --color=auto -inHPT1'  # GNU grep only
alias rawcode='grep -cv ^#\|^$'
alias ${PAGER:+less=}"$PAGER"

# {{{2
function mvto(){
        command mkdir -p "$2" && command git mv "$1" "$2"
}

function cd(){
   if [[ $1 == -- ]]; then shift; fi
   if [[ $# -gt 1 ]]; then echo "cd: too many arguments" >&2; exit 1; fi
   local - target=${1:-~} i
   case $target in
      - ) [[ -n ${DIRSTACK[1]} ]] && pushd +1 ;;
      + ) [[ -n ${DIRSTACK[1]} ]] && pushd -0 ;;
      ~+) return ;;
      ~-) target=$OLDPWD ;;&
      * ) target=$(realpath -s "$target")
         for i in "${!DIRSTACK[@]}"; do
            if [[ $target == "${DIRSTACK[$i]/#~/$HOME}" ]]; then
               pushd +"$i"
               return $?
            fi
         done
         pushd "$target"
         ;;
   esac >/dev/null
}

alias -- -='cd -'
alias -- +='cd +'

# {{{2 random
alias g=git
alias open='xdg-open'
alias ap='ansible-playbook'
alias tf='terraform'
alias assh='autossh -M 0'
alias ip='ip -c=auto'
alias myip='curl --ipv4 -sS https://tnx.nl/ip'
alias myip6='curl --ipv6 -sS http://icanhazip.com/'
alias birthdays="khal list -a birthdays -df '' -f '{calendar-color}{start}{reset} {title}' today 366d"

function path { set -- "${1:-PATH}"; echo -e "${!1//:/\\n}"; }
function _ { "$@" &>/dev/null & }

function pacman {
   if [[ " $* " =~ \ -(D|R|U|S|[RUS][^ hbpirlsk]*)\  &&
      ! " $*" =~ \ --(help|dbpath|root|info|list|search|check)([= ]) ]]
   then command sudo pacman "$@"
   else command pacman "$@"
   fi
}

function calc { awk 'BEGIN { print '"$*"' }'; }

function xkcd {
   wget "http://dynamic.xkcd.com/comic/random/" -qO- \
      | awk -F\" '/comics/ { print "http:"$2
                           ; print "http:"$2 > "/dev/stderr"
                           ; system("recode html..ascii <<< \"" $4 "\" >/dev/stderr")
                           ; exit }' \
      | wget -qi- -O- | display -
}

if cmd=$(type -P neomutt)       ; then alias mutt=$cmd; fi
if cmd=$(type -P time)          ; then alias time=$cmd; fi
if cmd=$(type -P udiskie-umount); then alias udu=$cmd; fi

# {{{2 pwgen
if   cmd=$(type -P pwgen)  ; then alias pwgen="$cmd -scnB 18"
elif cmd=$(type -P openssl); then alias pwgen="$cmd rand -base64"
fi

# {{{2 aliases: systemd{,-journal}
alias journalctl='journalctl -oshort-iso --no-hostname -b -q'
alias sc=systemctl
alias scuser="systemctl --user"
alias jc=journalctl
alias jcuser="journalctl --user"

# {{{2 aliases: mpc
if type -p mpc >/dev/null; then
   alias np='mpc --format "%title% - %artist% #[%album%#]" | head -n1'
   alias P='mpc --no-status toggle; np'
   alias n='mpc --no-status next; np'
   alias p='mpc --no-status prev; np'
   alias s='mpc --no-status stop; np'
fi

# {{{2 aliases: Input devices - These are for X220, IIRC
alias no_touch='xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0'
alias xi='xinput       set-prop 8 "Device Enabled" 1 && xinput set-prop 10 "Device Enabled" 1'
alias nxi='xinput      set-prop 8 "Device Enabled" 0 && xinput set-prop 10 "Device Enabled" 0'

# {{{1 motd

# Print the arch triangle on archlinux.
if cmd=$(type -P archey3); then "$cmd"; else cat /etc/motd || true; fi

# Show udisk info
if cmd=$(type -P udisksctl); then "$cmd" status; echo; fi

# Show last info
if cmd=$(type -P last); then "$cmd" -an 5 | head -n-1; fi

# Show stty deviations
stty | tail -n+2 | xargs printf "note: \033[0;36mstty %s\033[0m\n"

# Check if some random programs are available.
for prg in vim nix direnv shellcheck; do
  if ! type -t $prg >/dev/null; then echo "** program $prg not found **" >&2; fi
done
