#!/run/current-system/sw/bin/fish
set -l battery_status (acpi -b | string match -r "(?<=:).*?(?=,)" | string trim)
set -l battery_percentage (acpi -b | string match -r "(?<=,).*?(?=%)" | string trim)

if test $battery_status = "Charging"
    #  notify-send $battery_status "now"
    set icon ""
else
    if test $battery_percentage -le "10"
        set icon ""
    else if test $battery_percentage -le "20"
        set icon ""
    else if test $battery_percentage -le "30"
        set icon ""
    else if test $battery_percentage -le "40"
        set icon ""
    else if test $battery_percentage -le "50"
        set icon ""
    else if test $battery_percentage -le "60"
        set icon ""
    else if test $battery_percentage -le "70"
        set icon ""
    else if test $battery_percentage -le "80"
        set icon ""
    else if test $battery_percentage -le "90"
        set icon ""
    else if test $battery_percentage -le "100"
        set icon ""
    end
end

printf "%s %s%%" $icon $battery_percentage
