#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local THEME_DIR=~/.theme
    mkdir -p "${THEME_DIR}"
    local CUR_COLOR=paperColor.Xresources
    cat >"${THEME_DIR}/${CUR_COLOR}" <<"EOF"
! vim: filetype=xdefaults :

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
xterm*color0      : Ansi_0_Color/*  normal black   */
xterm*color1      : Ansi_1_Color/*  normal red     */
xterm*color2      : Ansi_2_Color/*  normal green   */
xterm*color3      : Ansi_3_Color/*  normal yellow  */
xterm*color4      : Ansi_4_Color/*  normal blue    */
xterm*color5      : Ansi_5_Color/*  normal magenta */
xterm*color6      : Ansi_6_Color/*  normal cyan    */
xterm*color7      : Ansi_7_Color/*  normal white   */
xterm*color8      : Ansi_8_Color/*  bright black   */
xterm*color9      : Ansi_9_Color/*  bright red     */
xterm*color10     : Ansi_10_Color/* bright green   */
xterm*color11     : Ansi_11_Color/* bright yellow  */
xterm*color12     : Ansi_12_Color/* bright blue    */
xterm*color13     : Ansi_13_Color/* bright magenta */
xterm*color14     : Ansi_14_Color/* bright cyan    */
xterm*color15     : Ansi_15_Color/* bright white   */
xterm*foreground  : Foreground_Color
xterm*background  : Background_Color
xterm*cursorColor : Cursor_Color
*color0      : Ansi_0_Color/*  normal black   */
*color1      : Ansi_1_Color/*  normal red     */
*color2      : Ansi_2_Color/*  normal green   */
*color3      : Ansi_3_Color/*  normal yellow  */
*color4      : Ansi_4_Color/*  normal blue    */
*color5      : Ansi_5_Color/*  normal magenta */
*color6      : Ansi_6_Color/*  normal cyan    */
*color7      : Ansi_7_Color/*  normal white   */
*color8      : Ansi_8_Color/*  bright black   */
*color9      : Ansi_9_Color/*  bright red     */
*color10     : Ansi_10_Color/* bright green   */
*color11     : Ansi_11_Color/* bright yellow  */
*color12     : Ansi_12_Color/* bright blue    */
*color13     : Ansi_13_Color/* bright magenta */
*color14     : Ansi_14_Color/* bright cyan    */
*color15     : Ansi_15_Color/* bright white   */
*foreground  : Foreground_Color
*background  : Background_Color
*cursorColor : Cursor_Color
EOF
    ln -sf "${THEME_DIR}/${CUR_COLOR}" "${THEME_DIR}/currentTheme.Xresources"
}

doWork
