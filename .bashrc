#!/usr/bin/env bash
# vim:noet:ts=4:
# shellcheck disable=SC1090,SC1091
# shellcheck disable=SC2139
# shellcheck disable=SC2015
# shellcheck disable=SC2155
#
# -l -i : .bash_profile | .bashrc | $BASH_ENV |
#  X  X :    X          |   X*    |           | * .bash_profile sources .bashrc
#  X    :    X          |   X*    |           | * .bash_profile sources .bashrc
#     X :               |   X     |           |
#       :               |         |  X*       | if $BASH_ENV exists; e.g. when `ssh host bash -c ...'


# POSIX-compatible string quoting
quote () { printf %s\\n "$1" | sed "s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/" ; }

# POSIX-compatible replacement for `[[ arg = a??* ]]'
fnmatch () { case $2 in $1) return 0 ;; esac; return 1; }

# sets [-v variable] [-s separator] [[-aA] value]
sets () {
	local OPTIND OPTARG opt
	local -n v || return $?
	local s=:
	local pre post
	while getopts v:s:a:A:p opt; do
		case $opt in
			v) v=$OPTARG ;;
			s) s=$OPTARG ;;
			a) pre=$OPTARG${pre:+$s}$pre ;;
			A) post=$post${post:+$s}$OPTARG ;;
			?) echo "Unrecognized option: $OPTARG" >&2; return 2 ;;
		esac
	done
	shift $((OPTIND - 1))
	if [ $# -gt $((OPTIND - 1)) ]; then
		printf "Too many arguments: %s (%s): %s\n" "$#" "$OPTIND" "$*" >&2
		return 2
	fi
	local value=$pre$s${v:-}$s$post
	local x=''
	local vnew=$s
	while [ "$value" != "$x" ]; do
		x=${value##*$s}
		vnew=${x:+$s}$x${vnew//$s$x$s/$s}
		value=${value%$s$x}
	done
	vnew=${vnew/%"$s"}
	vnew=${vnew/#"$s"}
	if [ -z "$pre$post" ]; then
		printf '%b\n' "${vnew//$s/\\n}"
	else
		v=$vnew
	fi
}

export PATH

sets -v PATH -a ~/bin/lennartcl-gitl -a ~/.local/bin -a ~/bin

export XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_RUNTIME_DIR XDG_DATA_DIRS XDG_MUSIC_DIR

: "${XDG_CONFIG_HOME:=$HOME/.config}"
: "${XDG_CACHE_HOME:=$HOME/.cache}"
: "${XDG_DATA_HOME:=$HOME/.local/share}"
: "${XDG_RUNTIME_DIR:=/run/user/$UID}"
: "${XDG_DATA_DIRS:=/usr/local/share:/usr/share}"
: "${XDG_MUSIC_DIR:=$HOME/music}"

sets -v XDG_DATA_DIRS -a ~/.nix-profile/share

export GPG_TTY=$(tty)
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
	export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi

export VAGRANT_DEFAULT_PROVIDER=${VAGRANT_DEFAULT_PROVIDER-libvirt}
export FFZF_DEFAULT_OPTS="--no-bold --color=16,hl:7,fg+:4,bg+:-1,gutter:0,hl+:14,info:11,border:6,prompt:-1,pointer:6,marker:5,spinner:4,header:12 --prompt=' '"

if [ -z "${LS_COLORS-}"	]; then
	eval "$(dircolors -b ~/.dircolors 2>/dev/null || dircolors)"
fi


if [ -n "${DISPLAY-}" ] && [ -z "${SSH_CONNECTION-}" ] && test -x /usr/lib/ssh/x11-ssh-askpass; then
	export SSH_ASKPASS=$_
else
	export SSH_ASKPASS=/usr/bin/systemd-ask-password
fi

if [ -z "${RANDFILE-}" ] && mkdir -p "$XDG_DATA_HOME/openssl" 2>&-; then
	export RANDFILE=$_/rnd
fi

if EDITOR=$(command -v vim);		then export EDITOR; fi
if PAGER=$(command -v vimpager);	then export PAGER; fi
if MANPAGER=$(command -v manpager); then export MANPAGER; fi
if CM_LAUNCHER=$(command -v rofi || command -v dmenu); then export CM_LAUNCHER; fi

alias path='sets -v PATH'

alias -- -='cd -'

alias _='<&- &>/dev/null'  # NOTE: "_ cmd... >&255" can restore shell sockets!
alias r='fc -s'

alias ls='ls -b --color=auto --group-directories-first'
alias la='ls -A'
alias ll='ls -li --author'
alias lla='ll -A'
alias llt='ll -t'
alias ll.='ll -d .*'
if id -Z >/dev/null 2>&1; then
	alias ll='ls -li --author -Z'
fi
if [[ $PAGER == *vimpager ]]; then
	alias less=vimpager
fi

alias mkdir='mkdir -p'

alias df='df -aTh'
alias dus='du -xh -d 1 | sort -h'

alias grep='grep --color=auto'
alias gr='grep --color=auto -inHPT1'  # GNU grep only

alias ip='ip -c=auto'
alias myip='curl --ipv4 -sS https://tnx.nl/ip'
alias myip6='curl --ipv6 -sS http://icanhazip.com'

alias g=git
alias gax='git annex'

alias open=xdg-open
alias ap=ansible-playbook
alias tf=terraform
alias sctl=systemctl
alias sctlu='systemctl --user'
alias journalctl='journalctl -oshort-iso --no-hostname -b -q'
alias jctl=journalctl
alias jctlu='journalctl --user'

alias pacman='sudo pacman'

alias findexts="find . -maxdepth 1 -type f -printf '%f\n' | sed 's|.*\.\([^.]*\)$|\1|' | sort -u"

alias birthdays="khal list -a birthdays -df '' -f '{calendar-color}{start}{reset} {title}' today 366d"

export MPC_FORMAT="%title% - %artist% #[%album%#]"

alias np='mpc status'
alias t='mpc toggle'
alias pnext='mpc next'
alias pprev='mpc prev'

if command -v neomutt >/dev/null; then alias mutt=neomutt; fi

if command -v pwgen >/dev/null;		then alias pwgen="pwgen -scnB 18"
elif command -v openssl >/dev/null; then alias pwgen="openssl rand -base64 18"
fi

alias fzfvi='fzf -m --bind "enter:execute(vim {})"'

alias vw='env -C ~/notes fzf +s --tac --tiebreak=end -q .wmd\$\ \!^diary/\ '

MAILCHECK=0
HISTCONTROL='erasedups:ignoreboth'
HISTIGNORE='&:[ ] *:exit'
HISTSIZE=100000
HISTFILESIZE=500000

set -b

shopt -s checkwinsize
shopt -s lithist histappend histverify
shopt -s autocd
shopt -s cdspell
shopt -s dirspell
shopt -s checkhash
shopt -s no_empty_cmd_completion
shopt -s extglob
shopt -s globstar # globstar: ** matches files + zero or more subdirectories
#shopt -s direxpand

shopt -q progcomp || complete -cf sudo

{
	complete -p git		|| source /usr/share/git/completion/git-completion.bash
	complete -p pandoc	|| eval "$(pandoc --bash-completion)"
	complete -p stack	|| eval "$(stack --bash-completion-script stack)"
	complete -p tmuxp	|| eval "$(_TMUXP_COMPLETE=source tmuxp)"
} >/dev/null 2>&1

awkexpr () { awk 'BEGIN { print '"$*"'}'; }

_prompt_hostcolor(){
	# select a c_host by hashing the hostname
	{ cat /etc/machine-id || hostname; } 2>/dev/null \
		| cksum | cut -f1 -d ' ' | { read -r X; printf '%s' "${@:$(((X%$#)+1)):1}"; }
}

_prompt(){

	tput_ps(){ printf '%s' '\[' "$(tput "$@")" '\]'; }

	local C R B Cw Ch C2 C3

	C=$(tput_ps setaf 11)
	R=$(tput_ps sgr 0)
	B=$(tput_ps bold)
	Cw=$(tput_ps setaf 6)
	C2=$(tput_ps setaf 10)
	C3=$(tput_ps setaf 4)
	Ch="\[\e[$(_prompt_hostcolor '0;32' '0;33' '0;34' '0;37' '0;91' '0;95' '0;96')m\]"

	local Cret="\[\e[0;\$((\$??91:32))m\]"
	local Ceuid="\[\e[0;\$((EUID?33:31))m\]"
	local line1 line2
	# shellcheck disable=SC2016
	line1="$C┌{$B\t$R$C}─{$Ceuid\u$C@$Ch\H$C:$Cw\w$C}─[$B$C2\\\${SHLVL}$R$C|$B\\\${\$}$R$C%$C3\j$C]"
	line2="$C├$B\!$R$C|$Cret\\\${?}$C┄$Ceuid\\\\\$$R "

	PS1="$line1\n$line2"
	PS2="$C│$R"
	PROMPT_COMMAND=$(cat <<-END
	case \$? in
		0) type -t __git_ps1 >/dev/null || __git_ps1(){ PS1=\$1\$2; } ;;&
		*) __git_ps1 "$line1" "\n$line2" " %s" ;;&
		0) test -x "\$(command -v direnv)" && eval "\$("\$_" export bash)" || : ;;&
	esac
	END
	)
}

case $- in
	*i*)
		export PS1 PS2 PROMPT_COMMAND
		export GIT_PS1_DESCRIBE_STYLE=branch
		export GIT_PS1_HIDE_IF_PWD_IGNORED=1
		export GIT_PS1_SHOWCOLORHINTS=1
		export GIT_PS1_SHOWDIRTYSTATE=1
		export GIT_PS1_SHOWSTASHSTATE=1
		export GIT_PS1_SHOWUNTRACKEDFILES=1
		export GIT_PS1_SHOWUPSTREAM="auto verbose name"
		type -t __git_ps1 >&2 || . /usr/share/git/completion/git-prompt.sh
		_prompt 2>/dev/null
		;;
esac
