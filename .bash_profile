#!/usr/bin/env bash

# shellcheck disable=SC1090
[ ! -r ~/.bashrc ] || . ~/.bashrc

if [ -z "${DISPLAY-}" ] && [ "${XDG_VTNR-}" = 1 ] && ! systemctl -q --user is-active graphical-session.target; then
	systemctl --user import-environment CM_LAUNCHER
	systemctl --user import-environment NIX_PATH NIX_SSL_CERT_FILE NIX_REMOTE

	systemctl --user import-environment PATH XDG_VTNR
	systemctl --user set-environment DISPLAY=:0
	systemctl --user start graphical.target xorg@0.service

elif [ -t 1 ] && [ "${SHLVL-}" = 1 ] && [ -z "${TMUX-}" ]; then
	case $- in
		*i*)
			archey3
			paste <(printf "\e[1;33m%s\e[0m" who) <(who -uT | head -n10)
			paste <(printf "\e[1;33m%s\e[0m" disks) <(udiskie-info -a -o $'\e[0;33m{device_file}\e[0m \e[0;92m{id_type}\e[0m \e[0;36m"{id_label}"\e[0m {mount_paths}')
			paste <(printf "\e[1;33m%s\e[0m" tty) <(stty) <(tty)
			paste <(printf "\e[1;33m%s\e[0m" last) <(last -w -n5)
			;;
	esac 2>/dev/null || :
fi

