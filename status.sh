#!/bin/zsh
# Author: Philipp Schmitt
# Dependencies: xrdr (https://github.com/pschmitt/bin)
# Version: 

SCREENCOUNT=$(xrdr count)
CONKYRC=$XDG_CONFIG_HOME/conky/conkyrc

DZEN_TITLE="xmonad_rbar"
DZEN_TA="r" # Alignment
BG='#161616'
FG='#8c8b8e'
FONT="Tamsyn-9"

XPOS=430
YPOS=0
HEIGHT=14

DZEN_CMD="tee >(dzen2 \
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
    -e \"button3=;onstart=lower\")"

case $SCREENCOUNT in
    3)
        DZEN_CMD="${DZEN_CMD} >(dzen2 \
            -dock \
            -title-name \"${DZEN_TITLE}_3\" \
            -u \
            -xs 3 \
            -x \"$XPOS\" \
            -y \"$YPOS\" \
            -h \"$HEIGHT\" \
            -ta \"$DZEN_TA\" \
            -bg \"$BG\" \
            -fg \"$FG\" \
            -fn \"$FONT\" \
            -e \"button3=;onstart=lower\")"
        ;&
    2)
        DZEN_CMD="${DZEN_CMD} >(dzen2 \
            -dock \
            -title-name \"${DZEN_TITLE}_2\" \
            -u \
            -xs 2 \
            -x \"$XPOS\" \
            -y \"$YPOS\" \
            -h \"$HEIGHT\" \
            -ta \"$DZEN_TA\" \
            -bg \"$BG\" \
            -fg \"$FG\" \
            -fn \"$FONT\" \
            -e \"button3=;onstart=lower\")"
        ;;
esac

conky -c $CONKYRC | eval $DZEN_CMD & # | tee > /var/log/statusbar.log

