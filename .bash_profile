#!/usr/bin/env bash

# shellcheck disable=SC1090
[ ! -r ~/.bashrc ] || . ~/.bashrc

systemctl --user import-environment PATH SSH_ASKPASS CM_LAUNCHER NIX_PATH NIX_SSL_CERT_FILE NIX_REMOTE

my_motd () {
	[ -t 1 ] || return
	archey3
	paste <(printf "\e[1;33m%s\e[0m" who) <(who -uT | head -n10)
	paste <(printf "\e[1;33m%s\e[0m" disks) <(udiskie-info -a -o $'\e[0;33m{device_file}\e[0m \e[0;92m{id_type}\e[0m \e[0;36m"{id_label}"\e[0m {mount_paths}')
	paste <(printf "\e[1;33m%s\e[0m" tty) <(stty) <(tty)
	paste <(printf "\e[1;33m%s\e[0m" last) <(last -w -n5)
}

if [ -z "${DISPLAY-}" ] && [ "${XDG_VTNR-}" = 1 ] && ! systemctl -q --user is-active graphical.target; then

	systemctl --user import-environment XDG_VTNR
	systemctl --user start xorg@0.socket
	systemctl --user start xorg@0.service
	systemctl --user start graphical.target
fi

case $- in
	*i*) my_motd 2>/dev/null ;;
esac
