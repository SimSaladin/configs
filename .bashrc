#!/usr/bin/env bash
# shellcheck shell=sh disable=SC1090,SC2015,SC2039,SC2139

# NOTE: shellcheck code explanations {{{1
#
# SC1090 "Can't follow non-constant source"
# SC2015 "A && B || C is not if-then-else"
# SC2039 "in POSIX 'shopt' is undefined"
# SC2139 "expands when defined, consider escaping"
#
# NOTE: .bashrc vs. .bash_profile vs. $BASH_ENV {{{1
#
# ~/.bash_profile: Sourced if shell is login (-l). *Sources ~/.bashrc.*
# ~/.bashrc:       Sourced if shell is interactive (-i) but /not/ login (-l).
# $BASH_ENV:       Sourced (if set) if not login (-l) and not interactive (-i).
#
# Example of non-interactive, non-login shell: `ssh ... bash -c "..."'
#
# NOTE: how to check if command exists in PATH {{{1
#
#       [ -x "$(command -v foo)" ]
#
# NOTE: related files {{{1
#
# - <url:~/.inputrc>
# - <url:~/.bash_profile>
# - <url:~/.bash_completion>
# - <url:~/.local/share/bash-completion/completions>
#
#}}}1

# Disable CTRL-S suspend (stty -ixon) {{{1
[ -t 0 ] && stty -ixon || :

# quote(): POSIX-compatible ${v@Q} (kind of) {{{1
quote () {
	printf %s\\n "$1" | sed "s/\n/\\\n/g;s/\x1B/\\\e/g;s/'/'\\\\''/g;1s/^/'/;\$s/\$/'/"
}

# fnmatch(): POSIX-compatible [[ arg = a??* ]] {{{1
fnmatch() {
	case $2 in $1) return 0 ;; esac
	return 1
}

# find_in(): IFS=: find_in "program" "$PATH" {{{1
find_in () {
	for p in $2; do
		if [ -e "$p/$1" ]; then
			printf "%s\n" "$p/$1"
			return
		fi
	done
	return 1
}

# is_{function,builtin}() - POSIX-compatible {{{1

# NOTE: POSIX 2013 alt.: $(command -V $1) = *function
is_function() { fnmatch '* function' "$(type -- "$1" 2>/dev/null)"; }

is_builtin () { fnmatch '* builtin' "$(type -- "$1" 2>/dev/null)"; }

# is_executable(): Like type -P "$1"  {{{1
is_executable() { IFS=: find_in "$1" "$PATH" >/dev/null; }

# curpos(): get current cursor position (not POSIX-compatible) {{{1

curpos()(
	# https://stackoverflow.com/questions/2575037/how-to-get-the-cursor-position-in-bash
	printf "\e[6n" >/dev/tty
	IFS='[;' read -r -s -dR _ ROW COL </dev/tty 2>/dev/tty
	printf "%i:%i\n" "$ROW" "$COL"
)

# path()                                                                 {{{1
path(){
	[ $# -gt 0 ] || set -- "$PATH"
	while [ $# -gt 0 ]; do
		IFS=:
		for p in $1; do
			printf "%s\n" "$p"
		done
		shift
	done
}

# clean_path()                                                           {{{1
clean_path(){
	_PATH=
	while [ $# -gt 0 ]; do
		IFS=:
		for p in $1; do
			case :$_PATH: in
				*:$p:*) : ;;
				*) _PATH=${_PATH:+$_PATH:}$p ;;
			esac
		done
		shift
	done
	printf "%s\n" "$_PATH"
}

# set and export PATH {{{1
PATH=$(clean_path ~/bin ~/.local/bin "${PATH:-/usr/bin}" ~/bin/lennartcl-gitl)
export PATH

# Set and export XDG_* variables {{{1
XDG_DATA_DIRS=$(clean_path ~/.nix-profile/share "${XDG_DATA_DIRS:=/usr/local/share:/usr/share}")

: "${XDG_CONFIG_HOME:=$HOME/.config}"
: "${XDG_CACHE_HOME:=$HOME/.cache}"
: "${XDG_DATA_HOME:=$HOME/.local/share}"
: "${XDG_RUNTIME_DIR:=/run/user/$(id -ru)}"
: "${XDG_MUSIC_DIR:=$HOME/music}"

export XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_RUNTIME_DIR XDG_DATA_DIRS XDG_MUSIC_DIR

# bash: history                                                          {{{1
HISTCONTROL=erasedups:ignoreboth # ignoreboth = ignorespace:ignoredups
HISTFILE=${XDG_CACHE_HOME}/bash_history # ~/.bash_history
HISTFILESIZE=-1
HISTIGNORE='&:[ ] *:exit'
HISTSIZE=100
HISTTIMEFORMAT=%s

if is_builtin shopt; then
	shopt -s lithist
	shopt -s histappend
	shopt -s histverify
fi

alias history='HISTTIMEFORMAT="[%F %T] " history'
alias h='history'

# bash: mail settings                                                    {{{1
unset MAILCHECK

# bash: job control settings {{{1
set -b # Report status of terminated background jobs immediately

# bash: cd and completion settings {{{1
if is_builtin shopt; then
	#shopt -s checkwinsize  # check window size & update LINES and COLUMNS (on by default)
	shopt -s autocd cdspell dirspell
	shopt -s checkhash
	shopt -s no_empty_cmd_completion
	shopt -s extglob
	shopt -s globstar
fi

if is_builtin complete; then
	shopt -q progcomp || complete -cf sudo || :
	! complete -p pandoc &>/dev/null && command -v pandoc >/dev/null && eval "$(pandoc --bash-completion)" || :
	! complete -p stack  &>/dev/null && command -v stack  >/dev/null && eval "$(stack --bash-completion-script stack)" || :
	! complete -p git    &>/dev/null && . "$(IFS=: find_in git/completion/git-completion.bash "$XDG_DATA_DIRS")" || :
fi

# set & export GPG_TTY, GPG_AGENT_INFO                                   {{{1
if [ -t 0 ]; then
	GPG_TTY=$(tty)
	export GPG_TTY
fi
# mutt might need this
export GPG_AGENT_INFO=${GPG_AGENT_INFO:-}

# set & export SSH_AUTH_SOCK                                             {{{1
if [ -z "${SSH_AUTH_SOCK-}" ] && command -v gpgconf >/dev/null; then
	SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
	export SSH_AUTH_SOCK
fi

# set dircolors ($LS_COLORS) if not set                                  {{{1
if [ -z "${LS_COLORS+1}" ]; then
	if [ -r ~/.dircolors ]; then
		eval "$(dircolors -b ~/.dircolors)"
	else
		eval "$(dircolors -b)"
	fi
fi

# openssl ($RANDFILE)                                                    {{{1
export RANDFILE=${RANDFILE-$XDG_DATA_HOME/openssl/rnd}

# clipmenu & rofi                                                        {{{1
if command -v rofi >/dev/null; then
	export CM_LAUNCHER=rofi
fi


# exec etc.                                                              {{{1
alias _='<&- &>/dev/null nohup'  # NOTE: "_ cmd... >&255" can restore shell sockets!
alias r='fc -s'

# cd                                                                     {{{1
alias -- -='cd -'
alias ...='../..'

# which                                                                  {{{1
alias which='(alias;declare -f)|which --read-alias --read-functions --show-tilde --show-dot'

# mkdir                                                                  {{{1
alias mkdir='mkdir -p'

# ls                                                                     {{{1
alias ls='ls -b --color=auto --group-directories-first'
alias ll='ls -lh'
alias la='ls -Ah'
alias lla='ls -lhA'
alias llt='ls -lht'
alias llZ='ls -lhZ --author'
alias ll.='ls -lhd .*'

# find, du, df, mount, etc.                                              {{{1
alias findexts="find . -maxdepth 1 -type f -printf '%f\n' | sed 's|.*\.\([^.]*\)$|\1|' | sort -u"

# df                                                                     {{{1
alias df='df -aTh'

# du                                                                     {{{1
alias dus='du -xh -d 1 | sort -h'

# mount                                                                  {{{1
alias mnt='mount | tabulate'

# EDITOR, PAGER, etc.                                                    {{{1
export EDITOR=vim
export PAGER=vimpager
export MANPAGER=vimpager

alias vimp='vimpager --no-passthrough'
alias vcat='vimcat -n -s'

alias less='vimpager -s -N'

# aliases alias                                                          {{{1
alias aliases='alias -p | vimcat -f sh'

# grep                                                                   {{{1
#export GREP_COLORS='ms=01;31:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'
if fnmatch '*GNU*' "$(command grep --version 2>/dev/null)"; then
	alias grep='grep --color=auto -inPH --label=/dev/stdin'
else
	alias grep='grep --color=auto'
fi

# systemd                                                                {{{1
alias sctl=systemctl
alias sctlu=systemctl\ --user

alias journalctl='journalctl -oshort-iso --no-hostname -b -q'
alias jctl=journalctl
alias jctlu='journalctl --user'

# archlinux                                                              {{{1
alias pacman='sudo pacman'

# git                                                                    {{{1
alias g='git'
alias gax='git annex'

# mpd/mpc                                                                {{{1
export MPC_FORMAT='%title% - %artist% #[%album%#]'

alias np='mpc status'
alias t='mpc toggle'
alias pnext='mpc next'
alias pprev='mpc prev'

# FZF <url:man:fzf.1>                                                    {{{1
export FZF_DEFAULT_OPTS="--no-bold --color=16,hl:7,fg+:4,bg+:-1,gutter:0,hl+:14,info:11,border:6,prompt:-1,pointer:6,marker:5,spinner:4,header:12 --prompt=' '"

alias fzfvi='fzf -m --bind "enter:execute(vim {})"'
alias vw='env -C ~/notes fzf +s --tac --tiebreak=end -q .wmd\$\ \!^diary/\ '

# vagrant, ansible, terraform                                            {{{1
# <url:https://www.vagrantup.com/docs/other/environmental-variables.html>
export VAGRANT_HOME=${VAGRANT_HOME-${XDG_DATA_HOME}/vagrant}
export VAGRANT_DEFAULT_PROVIDER=${VAGRANT_DEFAULT_PROVIDER-libvirt}

alias ap=ansible-playbook
alias tf=terraform

# ip (utils)                                                             {{{1
alias ip=ip\ -c=auto

# curl                                                                   {{{1
# - curl allows dy default HTTP,HTTPS,FTP and FTPS only since 7.65.2
# - FILE and SCP are disabled since 7.19.4
# - SMB and SMBS are disabled since 7.19.4
#
# --proto-default <protocols> tells the protocol to use when non is specified (since curl 7.45.0)
# --proto <protocols>         limits which protcols may be used in transit
# --proto-redir <protocols>   this option only decidedes which procotols are allowed as redirect targets {HTTP,HTTPS,FTP,FTPS by default)
export CURL_DEFAULT_OPTS="-j --http2 --keepalive-time 0 --proto-default https --proto -all,+https,+ftps --proto-redir -all,+https,+sftp --limit-rate 1G"

alias curl="curl $CURL_DEFAULT_OPTS"
alias myip6='curl https://tnx.nl/ip    -4fsS -M 1.2 --max-filesize 512 --noproxy --no-npn --no-keepalive -N'
alias myip6='curl http://icanhazip.com -6fsS -M 1.2 --max-filesize 512 --noproxy --no-pnn --no-keepalive -N'

# open (XDG)                                                             {{{1
alias open=xdg-open

# Haskell (GHC, stack)                                                   {{{1
alias ghci=stack\ ghci

# feh                                                                    {{{1
alias feh=feh\ --scale-down

# calendar etc.                                                          {{{1
alias birthdays="khal list -a birthdays -df '' -f '{calendar-color}{start}{reset} {title}' today 366d"

# pylint {{{1
# defaults to ~/.pylint.d
export PYLINTHOME=${XDG_DATA_HOME}/pylint

mutt() { #{{{1
	if is_executable neomutt; then
		command neomutt "$@"
	else
		command mutt "$@"
	fi
}

pwgen() { #{{{1
	if is_executable pwgen; then
		command pwgen -cnsB1 "${1:-18}"
	elif is_executable openssl; then
		command openssl rand -base64 "${1:-18}"
	else
		return 127
	fi
}

awkexpr() { #{{{1
	awk 'BEGIN { print '"$*"'}'
}

# alias complete {{{1
complete -F _complete_alias -- $(alias -p | sed -e 's/^alias \([^=]*\)=.*/\1/')

# prompt                                                                 {{{1

_prompt_hostcolor() {
	sum=$({ cat /etc/machine-id || hostname; } 2>/dev/null | cksum | cut -f1 -d\  )
	shift $(( (sum % $#) ))
	echo "$1"
}

__prompt_command () {
	[ -n "$HISTFILE" ] && history -a && history -c && history -r || :

	IFS=' ' \
		GIT_PS1_DESCRIBE_STYLE=branch \
		GIT_PS1_HIDE_IF_PWD_IGNORED=1 \
		GIT_PS1_SHOWCOLORHINTS=1 \
		GIT_PS1_SHOWDIRTYSTATE=1 \
		GIT_PS1_SHOWSTASHSTATE=1 \
		GIT_PS1_SHOWUNTRACKEDFILES=1 \
		GIT_PS1_SHOWUPSTREAM="auto verbose name" \
		__git_ps1 "${_1:-}" "${_2:-}" "${_ps1_git:- %s}"

	if [ "${_RET:-0}" -eq 0 ] && command -v direnv >/dev/null; then
		eval "$(direnv export bash)" || PS1="direnv: exit $?\n$PS1"
	fi

	# TODO
	#case "$(curpos)" in
	#	*:1) : ;;
	#	*:*) PS1=\\n$PS1 ;;
	#esac

	return "${_RET:-0}"
}

# shellcheck disable=SC2016,SC2154
__prompt() {
	_frame=11 # 11
	_success=2 # 2
	_failure=9 # 9
	_root=1 # 1
	_user=3 # 3
	_host=$(_prompt_hostcolor 2 3 4 13 14)

	set_reset=$(tput sgr0)
	set_frame=$(tput setaf $_frame)
	set_euid='\$(tput setaf $((EUID?'$_user':'$_root')))'
	set_code='\$(tput setaf $((_RET?'$_failure':'$_success')))'

	ps_reset="\[${set_reset}\]"
	ps_time="\[$(tput setaf 12)\]\t$ps_reset"
	ps_wdir="\[$(tput setaf  6)\]\w$ps_reset"
	ps_jobs="\[$(tput setaf  4)\]%\j$ps_reset"
	ps_host="\[$(tput setaf "$_host")\]\H$ps_reset"
	ps_euser="\[${set_euid}\]\u$ps_reset"
	ps_dsign="\[${set_euid}\]\\\$$ps_reset"
	ps_ecode="\[${set_code}\]\$(printf %3i \$_RET)$ps_reset"
	ps_cmd_id="\[$(tput setaf 11)\]\#$ps_reset"
	ps_sh_lvl="\[$(tput setaf 10)\]\$SHLVL$ps_reset"
	ps_sh_pid="\[$(tput setaf 10)\]\${\$}$ps_reset"

	PS0_1="${set_frame}command #\# started \D{at %T on %a %F %Z}$(tput sgr0)\n"

	PS1_1=$(printf %s     "$ps_reset" ' ' "┌┤$ps_cmd_id├┤$ps_time├┄┤$ps_euser@$ps_host:$ps_wdir├┄╎$ps_sh_pid╷$ps_sh_lvl╷$ps_jobs╎ $ps_ecode")
	PS1_2=$(printf %s \\n "$ps_reset"     "┆$ps_dsign" ' ')
	PS2_1=$(printf %s     "$ps_reset"     "┆$ps_reset")

	# PS0: only bash 4.4 and up
	PS0=$(quote "$PS0_1")
	PS1=$(quote "$PS1_1$PS1_2")
	PS2=$(quote "$PS2_1")

	PC="_RET=\$? _1=\"$PS1_1\" _2=\"$PS1_2\" __prompt_command 2>/dev/null"

	printf "export %s;\n" "PS0=$PS0" "PS1=$PS1" "PS2=$PS2" "PROMPT_COMMAND=$(quote "$PC")"
}

if fnmatch '*i*' "$-"; then
	. "$(IFS=: find_in git/completion/git-prompt.sh "$XDG_DATA_DIRS")" 2>/dev/null
	eval "$(__prompt)"
fi

# vim:noet:ts=4:fdm=marker:
