import os
from os import environ
import subprocess
from libqtile import bar, layout, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy
from qtile_extras import widget

mod = "mod4"
terminal = environ.get("MYTERM")
wmname = "LG3D"

auto_fullscreen = True
bring_front_click = "floating_only"
cursor_warp = False
dgroups_key_binder = None
focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
dgroups_app_rules = []

groups = [
    Group("1", label = "⍺"),
    Group("2", label = "ϐ"),
]

keys = [
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod, "control"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "Tab", lazy.prev_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawn("rofiLauncher"), desc="Rofi"),
    Key([mod], "a", lazy.spawn("emacsclient -c"), desc="Emacs"),
    Key([mod], "e", lazy.spawn("tfm"), desc="File manager"),
    Key([mod], "t", lazy.group['scratch'].dropdown_toggle("term"), desc="Toggle scratchpad"),
]



for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

groups.append(
    ScratchPad("scratch", [
        DropDown("term", terminal, width=0.8, height=0.5, opacity=0.9,on_focus_lost_hide=True)]),)

layouts = [
    layout.Floating()
    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width = 4),
    # layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

colors = dict(
    foreground = '#2E3436',
    background = '#EEEEEC',
    inactive = '#999999'
)

widget_defaults = dict(
    font="DejaVuSansMono Nerd Font Mono",
    fontsize=14,
    padding=3,
    foreground = colors['foreground'],
    background = colors['background']
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        wallpaper='~/.config/qtile/theme/background.jpg',
        wallpaper_mode='fill',
        bottom=bar.Bar(
            [
                # widget.CurrentLayoutIcon(),
                widget.GroupBox(active = '#2E3436'),
                widget.TaskList(max_title_width = 400, icon_size = 20, padding_x = 5, padding_y = 2, margin_y = 2, fontsize = 16),
                widget.StatusNotifier(),
                widget.KeyboardLayout(configured_keyboards = ['us', 'ru', 'il']),
                # widget.Backlight(backlight_name = 'intel_backlight'),
                widget.UPowerWidget(font_colour = colors['foreground'], fill_normal = colors['foreground'], border_colour = colors['foreground']),
                widget.ALSAWidget(mode = 'icon', theme_path = '/usr/share/icons/breeze/status/22'),
                widget.WiFiIcon(interface = 'wlo1', active_colour = colors['foreground'], inactive_colour = colors['inactive']),
                widget.Clock(format="%a %d/%m,%H:%M"),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

# floating_layout = layout.Floating(
#     float_rules=[
#         *layout.Floating.default_float_rules,
#         Match(wm_class="ssh-askpass"),
#         Match(title="pinentry"),
#         Match(title="kitty-vifm"),
#     ]
# )

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~')
    subprocess.Popen([home + '/.config/qtile/autostart.sh'])

# @hook.subscribe.client_managed
# def clientManaged(c):
#     if c.get_wm_class()[0] == 'kitty-vifm':
#         # c.floating = True
#         c.maximized = True

# @hook.subscribe.client_new
# def clientNew(c):
#     if c.get_wm_class()[0] == 'kitty-vifm':
#         c.floating = True
        # c.maximized = True

# @hook.subscribe.client_focus
# def clientFocus(c):
#     c.cmd_bring_to_front()
