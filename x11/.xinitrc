wal -i "$(< "${HOME}/.cache/wal/wal")"
xrandr --dpi 192
xrdb -merge ~/.Xresources
exec xmonad
