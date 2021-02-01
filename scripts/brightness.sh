#!/run/current-system/sw/bin/fish
set -l current_brightness (math (brightnessctl g) / 1500)
set -l current_brightness_percent (math "round ($current_brightness * 100)")

if test $current_brightness_percent -le "50"
   set icon ""
else if test $current_brightness_percent -le "99"
   set icon ""
else
   set icon ""
end

printf "%s %s%%" $icon $current_brightness_percent
