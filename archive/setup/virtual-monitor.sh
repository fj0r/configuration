gtf 1600 2560 60
#Modeline "1600x2560_60.00"  353.35  1600 1736 1912 2224  2560 2561 2564 2648  -HSync +Vsync
xrandr --newmode "1600x2560_60.00"  353.35  1600 1736 1912 2224  2560 2561 2564 2648  -HSync +Vsync
xrandr --addmode HDMI-1 1600x2560_60.00
xrandr --output HDMI-1 --mode 1600x2560_60.00 --left-of DP-1
x11vnc -clip 1280x800+0+0
