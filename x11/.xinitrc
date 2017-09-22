wal -i "$(< "${HOME}/.cache/wal/wal")"
xrandr --dpi 192
xrdb -merge ~/.Xresources
/* reload polybar --reload main */
/* reload compton */
exec xmonad
