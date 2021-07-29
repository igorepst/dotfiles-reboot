#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local THEME_DIR=~/.theme
    mkdir -p "${THEME_DIR}"
    local CUR_COLOR=hemisu.Xresources
    cat >"${THEME_DIR}/${CUR_COLOR}" <<"EOF"
! vim: filetype=xdefaults :

#define Ansi_0_Color     #777777
#define Ansi_8_Color     #999999
#define Ansi_1_Color     #ff0055
#define Ansi_9_Color     #d65e76
#define Ansi_2_Color     #82b414
#define Ansi_10_Color    #9dc700
#define Ansi_3_Color     #503d15
#define Ansi_11_Color    #957656
#define Ansi_4_Color     #00729b
#define Ansi_12_Color    #9eb3cd
#define Ansi_5_Color     #5c345f
#define Ansi_13_Color    #a284a5
#define Ansi_6_Color     #538192
#define Ansi_14_Color    #85b2ab
#define Ansi_7_Color     #999999
#define Ansi_15_Color    #bbbbbb
#define Foreground_Color #282828
#define Background_Color #fffffa
#define Cursor_Color     #fc971e
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
