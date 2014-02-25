#!/bin/zsh
# Author: Philipp Schmitt
# Dependencies: xrdr (https://github.com/pschmitt/xrdr)
# Version: 1.0

SCREENCOUNT=$(xrdr count)
CONKYRC=$XDG_CONFIG_HOME/conky/conkyrc
CONKYRC_SMALL=$XDG_CONFIG_HOME/conky/conkyrc_small

DZEN_TITLE="xmonad_rbar"
DZEN_TA="r" # Alignment
BG='#161616'
FG='#8c8b8e'
FONT="Tamsyn-16"

XPOS=430
YPOS=0
HEIGHT=20

ARGS=$(getopt -o "x:w:h:" -l "bg:,fg:,fn:" -n "$0" -- "$@")

[[ $? -ne 0 ]] && { echo "Woops, wrong params."; exit 1; }

eval set -- "$ARGS"

while true; do
    case "$1" in
        -x)
            XPOS=$2
            shift 2
            ;;
        -h)
            HEIGHT=$2
            shift 2
            ;;
        --fg)
            FG=$2
            shift 2
            ;;
        --bg)
            BG=$2
            shift 2
            ;;
        --fn)
            FONT=$2
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Internal error"
            exit 1
    esac
done

DZEN_TEMPLATE="dzen2 \
    -dock \
    -title-name \"$DZEN_TITLE\" \
    -u \
    -xs 1 \
    -x \"$XPOS\" \
    -y \"$YPOS\" \
    -h \"$HEIGHT\" \
    -ta \"$DZEN_TA\" \
    -bg \"$BG\" \
    -fg \"$FG\" \
    -fn \"$FONT\" \
    -e \"button3=\""

for screen in $(seq $SCREENCOUNT); do
    dzen_cmd=$(sed -e "s/\($DZEN_TITLE\)/\1_$screen/;\
                       s/\(-xs 1\)/-xs $screen/"\
               <<< "$DZEN_TEMPLATE")
    if xrdr vertical $screen; then
        if [[ -z $DZEN_SMALL_CMD ]]; then
            DZEN_SMALL_CMD="tee >($dzen_cmd)"
        else
            DZEN_SMALL_CMD="$DZEN_SMALL_CMD >($dzen_cmd)"
        fi
    else
        if [[ -z $DZEN_LARGE_CMD ]]; then
            DZEN_LARGE_CMD="tee >($dzen_cmd)"
        else
            DZEN_LARGE_CMD="$DZEN_LARGE_CMD >($dzen_cmd)"
        fi
    fi
done

if [[ -n $DZEN_SMALL_CMD ]]; then
    conky -c $CONKYRC_SMALL | eval $DZEN_SMALL_CMD &
fi
if [[ -n $DZEN_LARGE_CMD ]]; then
    conky -c $CONKYRC | eval $DZEN_LARGE_CMD &
fi

