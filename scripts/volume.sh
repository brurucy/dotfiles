#!/run/current-system/sw/bin/fish
set -l current_volume (amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $2 }')
set -l current_volume_status (amixer get Master | tail -2 | grep -c '\[on\]')

if test $current_volume_status -eq "2"
    set icon ""
else
    set icon "ﱝ"
end

printf "%s %s" $icon $current_volume
