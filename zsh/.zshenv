alias paclog='aura -Li'
alias remove='sudo aura -R'
alias spotify='spotify --force-device-scale-factor=1.5'
alias qutebrowser='qutebrowser --backend webengine'
alias wal='wal -i ~/wallpapers/'
alias ag='ag --path-to-ignore ~/.ignore'
alias errlog='journalctl -p 3 -xb'


export EDITOR='vim'
export BROWSER='vimb'
export _JAVA_AWT_WM_NONREPARENTING=1

# export QT_AUTO_SCREEN_SCALE_FACTOR=1
if [ -e /home/alex/.nix-profile/etc/profile.d/nix.sh ]; then . /home/alex/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
