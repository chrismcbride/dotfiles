xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 3 --transparent true --tint 0x191970 --height 12 &
 
xsetroot -solid midnightblue
xsetroot -cursor_name left_ptr

xscreensaver -no-splash &

if [ -x /usr/bin/wicd-client ] ; then
   wicd-client --tray &
fi
 
if [ -x /usr/bin/volumeicon ] ; then
   volumeicon &
fi

xmobar -x 1 /home/mcbride/.xmobarrc2 &

if [ -x /usr/bin/xfce4-power-manager ] ; then
	xfce4-power-manager
fi

exec ck-launch-session dbus-launch /usr/bin/xmonad
