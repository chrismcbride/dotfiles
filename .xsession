xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 1 --transparent true --tint 0x191970 --height 12 &
 
xsetroot -solid midnightblue
xsetroot -cursor_name left_ptr

xscreensaver -no-splash &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi
 
if [ -x /usr/bin/volumeicon ] ; then
   volumeicon &
fi

xmobar -x 1 /home/mcbride/.xmobarrc2 &

exec /home/mcbride/programs/bin/xmonad
