wal -i "$(< "${HOME}/.cache/wal/wal")"
xrdb -merge ~/.Xresources
exec xmonad
