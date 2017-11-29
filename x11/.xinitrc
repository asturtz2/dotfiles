wal -i "$(< "${HOME}/.cache/wal/wal")"
xrdb -merge ~/.Xresources
mopidy &
exec xmonad
