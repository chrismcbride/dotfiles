if [[ `xrandr -q | grep connected | wc -l` -eq "2" ]]; then
	xrandr --output DVI-0 --mode 1920x1080 --output HDMI-0 --mode 1920x1080 --left-of DVI-0
fi

xrdb ~/.Xresources

xsetroot -solid midnightblue
xsetroot -cursor_name left_ptr

xscreensaver -no-splash &
xmodmap /home/mcbride/.Xmodmap
lxpanel &

if [ -x /usr/bin/wicd-client ] ; then
   wicd-client --tray &
fi

if [ -x /usr/bin/volumeicon ] ; then
   volumeicon &
fi

if [ -x /bin/numlockx ] ; then
   numlockx &
fi

xmobar -x 1 /home/mcbride/.xmobarrc2 &

if [ -x /usr/bin/xfce4-power-manager ] ; then
	xfce4-power-manager
fi

eval `ssh-agent`

exec dbus-launch /usr/bin/xmonad
