
# `~/.config/systemd/user/README.md`

> **graphical.target**
> ● ├─ ...
> ● └─basic.target

> **basic.target**
> ● ├─systemd-tmpfiles-setup.service
> ● ├─ ...
> ● ├─paths.target
> ● ├─sockets.target
> ● │ ├─dbus.socket
> ● │ └─...
> ● └─timers.target
> ●   ├─systemd-tmpfiles-clean.timer
> ●   └─...

# Xorg

Usage (`DISPLAY :0`):
    systemctl --user import-environment XDG_VTNR
    systemctl --user start xorg@0.socket

- `xorg@.service`
- `xorg@.socket`
