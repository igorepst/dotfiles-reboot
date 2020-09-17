#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local THEME_DIR=~/.theme
    mkdir -p "${THEME_DIR}"
    local CUR_COLOR=paperColor.Xresources
    cat >"${THEME_DIR}/${CUR_COLOR}" <<"EOF"
! vim: filetype=xdefaults :

! xterm*background: #eeeeee
! xterm*foreground: #444444
! xterm*color0: #eeeeee
! xterm*color1: #af0000
! xterm*color2: #008700
! xterm*color3: #5f8700
! xterm*color4: #0087af
! xterm*color5: #878787
! xterm*color6: #005f87
! xterm*color7: #444444
! xterm*color8: #bcbcbc
! xterm*color9: #d70000
! xterm*color10: #d70087
! xterm*color11: #8700af
! xterm*color12: #d75f00
! xterm*color13: #d75f00
! xterm*color14: #005faf
! xterm*color15: #005f87
! *background: #eeeeee
! *foreground: #444444
! *color0: #eeeeee
! *color1: #af0000
! *color2: #008700
! *color3: #5f8700
! *color4: #0087af
! *color5: #878787
! *color6: #005f87
! *color7: #444444
! *color8: #bcbcbc
! *color9: #d70000
! *color10: #d70087
! *color11: #8700af
! *color12: #d75f00
! *color13: #d75f00
! *color14: #005faf
! *color15: #005f87

#define Ansi_0_Color     #1c1c1c
#define Ansi_8_Color     #1c1c1c
#define Ansi_1_Color     #af0000
#define Ansi_9_Color     #af0000
#define Ansi_2_Color     #008700
#define Ansi_10_Color    #008700
#define Ansi_3_Color     #d75f00
#define Ansi_11_Color    #d75f00
#define Ansi_4_Color     #005faf
#define Ansi_12_Color    #005faf
#define Ansi_5_Color     #d70087
#define Ansi_13_Color    #d70087
#define Ansi_6_Color     #0087af
#define Ansi_14_Color    #0087af
#define Ansi_7_Color     #e4e4e4
#define Ansi_15_Color    #e4e4e4
#define Foreground_Color #444444
#define Background_Color #eeeeee
#define Cursor_Color     #005f87
XTerm*color0      : Ansi_0_Color/*  normal black   */
XTerm*color1      : Ansi_1_Color/*  normal red     */
XTerm*color2      : Ansi_2_Color/*  normal green   */
XTerm*color3      : Ansi_3_Color/*  normal yellow  */
XTerm*color4      : Ansi_4_Color/*  normal blue    */
XTerm*color5      : Ansi_5_Color/*  normal magenta */
XTerm*color6      : Ansi_6_Color/*  normal cyan    */
XTerm*color7      : Ansi_7_Color/*  normal white   */
XTerm*color8      : Ansi_8_Color/*  bright black   */
XTerm*color9      : Ansi_9_Color/*  bright red     */
XTerm*color10     : Ansi_10_Color/* bright green   */
XTerm*color11     : Ansi_11_Color/* bright yellow  */
XTerm*color12     : Ansi_12_Color/* bright blue    */
XTerm*color13     : Ansi_13_Color/* bright magenta */
XTerm*color14     : Ansi_14_Color/* bright cyan    */
XTerm*color15     : Ansi_15_Color/* bright white   */
XTerm*colorUL     : Underline_Color
XTerm*colorRV     : Reverse_Color
XTerm*foreground  : Foreground_Color
XTerm*background  : Background_Color
XTerm*cursorColor : Cursor_Color
EOF
    ln -sf "${THEME_DIR}/${CUR_COLOR}" "${THEME_DIR}/currentTheme.Xresources"
}

doWork
