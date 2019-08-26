#
# File: ~/.config/qutebrowser/config.py
#
# Documentation: {{{1
#   <url:qute://help/configuring.html>
#   <url:qute://help/settings.html>
#   <url:qute://help/contributing.html>
#
# See also:
#   <url:/usr/lib/python3.7/site-packages/qutebrowser>
#
# Debugging:
#   --debug/-d
#           to turn on debug features.
#   --temp-basedir
#           might be useful with --debug.
#   :debug-{pyeval,console}
#   :debug-all-{objects,widgets}
#
# Command-line args:
#   --debug                     (enable debug mode)
#   --loglevel <LEVEL>          (see debug-log-level)
#   --logfilter <CAT>,!<CAT>…   (see debug-log-filter)
#   --loglines <NUM>            (see debug-log-capacity)
#   --qt-arg <k> <v>            (--qt-arg geometry 600x400)
#
# Logging:
#   <LEVEL>
#           (qutebrowser log levels)
#           vdebug, debug, info, warning, error, critical
#   <CAT>
#           statusbar, completion, init, url, destroy, modes, webview, misc,
#           mouse, procs, hints, keyboard, commands, signals, downloads, js,
#           qt, rfc6266, ipc, shlexer, save, message, config, sessions,
#           webelem, prompt, network, sql, greasemonkey, extensions
#   :messages [-p] [-t] [-w] <LEVEL>
#           Show log of past messages (subject to log_capacity())
#   qutebrowser.utils.log.<LOGGER>.<LEVEL>(<STRING>)
#   :message-{error,info,warning}
#           Generate a "message" type log item of said level, or
#           any logger any level from python.
#   qutebrowser.misc.utilcmds.log_capacity(<NUM>)
#   :set-log-capacity <NUM>
#           Change how many lines to store in RAM.
#   :debug-log-level (<LEVEL>)
#   :debug-log-filter (categories)
#           (debug mode only!) filter which messages/levels output to stdout
#           (i.e. not just internally).
#   :set content.javascript.log {...}
#           How to map javascript error levels to qutebrowser levels.
#           On QtWebKit, the unknown JS level is always used.
# }}}1
# pylint: disable=C0111  {{{1
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103
# }}}1

from qutebrowser.api import cmdutils
from qutebrowser.misc import utilcmds, objects
import subprocess

if 'set-log-capacity' not in objects.commands.keys():
    @cmdutils.register(name='set-log-capacity')
    def set_log_capacity(capacity=300):
        """Log lines to keep in RAM
        Set to 0, so that passwords from `qute-pass` aren't persisted.
        Note: also clears the log.
        """
        utilcmds.log_capacity(capacity)
    set_log_capacity(0)
    utilcmds.debug_log_level('error')

if 'pyeval' not in objects.commands.keys():
    @cmdutils.register(name='pyeval', maxsplit=0, no_cmd_split=True)
    def my_pyeval(s, file=False, quiet=True):
        """Evaluate a python string and display the results as a web page.

        Args:
            s: The string to evaluate.
            file: Interpret s as a path to file, also implies --quiet.
            quiet: Don't show the output in a new tab.
        """
        utilcmds.debug_pyeval(s, file, quiet)

# {{{1 aliases[…]
# Spellcheck https://www.wufoo.com/html5/spellcheck-attribute/
c.aliases['dictcli'] = 'spawn --output /usr/share/qutebrowser/scripts/dictcli.py'
c.aliases['open-mpv'] = 'spawn mpv "{url}"'
c.aliases['pass'] = 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-pass ' + '-d dmenu -m -U secret -u "username: (.+)" -P "password: (.+)"'

# {{{1 bind(…)
# See: :help :bindings.default
# modes: normal insert command caret hint passthrough prompt register yesno
# usage: bind --mode [mode] --default [key] [command]
config.bind('zl', 'pass')
config.bind('zul', 'pass --username-only')
config.bind('zpl', 'pass --password-only')
config.bind('zol', 'pass --otp-only')
config.bind('zv', 'open-mpv')

# {{{ misc.

c.window.hide_decoration = True
c.window.title_format = 'qb:{host}{title_sep}{current_title}'

# how to open links in existing instance, when link opened from outside that
# instance.
# Values: tab / tab-bg / tab-silent / tab-bg-silent / window
c.new_instance_open_target = "window"

# Which window to choose when opening links as new tabs. Only if
# new_instance_open_target is not "window"!
# Values: first-opened / last-opened / last-focused / last-visible
# new_instance_open_target_window = ""

# }}}
# {{{1 qt.*
# c.qt.highdpi = True
# Share process for buffers to same site
# c.qt.process_model = 'process-per-site'

# {{{1 messages.*
# set to positive number to clear messages older than that.
c.messages.timeout = 3000  # msecs

# {{{1 auto_save.*
# Always restore open sites when qutebrowser is reopened.
# Type: Bool
c.auto_save.session = True
c.auto_save.interval = 15000  # msec

c.confirm_quit = ["downloads"]

c.completion.height = '25%'
c.completion.shrink = True

c.history_gap_interval = 180  # mins

# keyhint.* ? XXX

# {{{1 session.*
c.session.lazy_restore = True

# {{{1 spellcheck.*
# note, spellcheck requires qtwebkit-plugins package installed.
c.spellcheck.languages = ['de-DE', 'en-GB', 'en-US']

# {{{1 editor.*
c.editor.command = [
    'urxvtc', '-name', 'info-terminal', '-geometry', '100x40',
    '-e', 'vim', '-f', '{file}', '-c', 'normal {line}G{column0}l']

# {{{1 input.insert_mode.*
c.input.insert_mode.auto_enter = True  # enter insert on editable focus
c.input.insert_mode.auto_leave = True  # exit insert on non-editable focus
c.input.insert_mode.auto_load = False  # focus editable elem after page load
c.input.insert_mode.leave_on_load = True
c.input.insert_mode.plugins = True  # focusing flash/plugins turns on insert

# {{{1 content.*
c.content.plugins = False
c.content.pdfjs = True

c.content.mute = True
c.content.notifications = False
config.set('content.mute', False, 'https://*.flowdock.com/*')
config.set('content.notifications', True, 'https://*.flowdock.com/*')

c.content.default_encoding = 'utf-8'
c.content.autoplay = False
c.content.local_content_can_access_remote_urls = True
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.content.webgl = True
c.content.ssl_strict = True
c.content.media_capture = 'ask'  # true / false / ask
# Allow websites register protocol handlers (navigator.registerProtocolHandler)
c.content.register_protocol_handler = False  # true / false / ask

# {{{2 content.headers.*
c.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36'
c.content.headers.accept_language = 'en-US,en;q=0.5'
c.content.headers.custom = {"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}

# {{{2 content.javascript.*
c.content.javascript.enabled = True
config.set('content.javascript.enabled', True, 'file://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

c.content.javascript.can_open_tabs_automatically = True

c.content.javascript.log['unknown'] = "debug"

# {{{1 url.*
c.url.default_page = 'https://start.duckduckgo.com/'
c.url.auto_search = 'dns'
c.url.open_base_url = True

# Maps a search engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
# Type: Dict
c.url.searchengines = {
    # wiki
    'wikipedia':    'https://en.wikipedia.org/w/index.php?search={}',
    'archwiki':     'https://wiki.archlinux.org/?search={}',
    'relexwiki':
        'https://wiki.relexsolutions.com/dosearchsite.action?queryString={}',
    # shop
    'ebay':         'https://www.ebay.com/sch/i.html?_nkw={}',
    'amazon.de':    'https://www.amazon.de/s?field-keywords={}',
    'mouser.at':    'https://www.mouser.at/Search/Refine?Keyword={}',
    # gtranslate
    'fien':         'https://translate.google.fi/#fi/en/{}',  # translate fi ->
    'fide':         'https://translate.google.fi/#fi/de/{}',
    'enfi':         'https://translate.google.fi/#en/fi/{}',  # translate en ->
    'ende':         'https://translate.google.fi/#en/de/{}',
    'defi':         'https://translate.google.fi/#de/fi/{}',  # translate de ->
    'deen':         'https://translate.google.fi/#de/en/{}',
    # other
    'googlemaps':   'https://www.google.fi/maps/search/{}',
    'stackage':     'https://www.stackage.org/lts/hoogle?q={}',
    'hackage':      'https://hackage.haskell.org/packages/search?terms={}',
    'dockerhub':    'https://hub.docker.com/search?q={}&type=image',
    'blockchain':   'https://www.blockchain.com/search?search={}',
    'lastfm':       'https://www.last.fm/search?q={}',
    'nyaasi-anime': 'https://nyaa.si/?f=0&c=1_2&q={}',
    'rutracker':    'https://rutracker.org/forum/tracker.php?nm={}',
    'imdb':         'https://www.imdb.com/find?s=all&q={}',
    'mal':          'https://myanimelist.net/search/all?q={}',
    'DEFAULT':      'https://duckduckgo.com/?q={}'
}

# {{{1 tabs.*
c.tabs.show = 'always'  # 'always' / 'never' / 'multiple' / 'switching'
c.tabs.position = 'bottom'
c.tabs.width = '15%'  # when vertical
c.tabs.padding = {'top': 1, 'bottom': 1, 'left': 1, 'right': 1}
c.tabs.background = True  # middleclick/ctrl+click opens tab in the background
c.tabs.last_close = 'default-page'
c.tabs.select_on_remove = 'prev'
c.tabs.favicons.scale = 1.0
c.tabs.favicons.show = 'always'  # always / never / pinned
c.tabs.show_switching_delay = 4500
c.tabs.new_position.related = 'prev'  # first / last / next / prev
c.tabs.new_position.unrelated = 'prev'
c.tabs.new_position.stacking = True
c.tabs.title.format = '{index}:{current_title}'
c.tabs.title.format_pinned = '{index}:{current_title}'
c.tabs.undo_stack_size = 10  # per window retained closed stacks

# {{{1 statusbar.*
c.statusbar.hide = False  # hide statusbar when there's nothing interesting.

# {{{1 downloads.*
c.downloads.location.directory = '~/Downloads'
c.downloads.location.suggestion = 'both'
# }}}1
# {{{1 colors.* (theme)
# {{{2 solarized_* color names
solarized_base03 = "#002b36"  # (bg)
solarized_base02 = "#073642"  # (bg)
solarized_base01 = "#586e75"  # (content)
solarized_base00 = "#657b83"  # (content)
solarized_base0 = "#839496"  # (content)
solarized_base1 = "#93a1a1"  # (content)
solarized_base2 = "#eee8d5"  # (bg)
solarized_base3 = "#fdf6e3"  # (bg)
solarized_red = "#dc322f"
solarized_orange = "#cb4b16"
solarized_yellow = "#b58900"
solarized_green = "#859900"
solarized_cyan = "#2aa198"
solarized_blue = "#268bd2"
solarized_violet = "#6c71c4"
solarized_magenta = "#d33682"
# }}}2
# {{{2 read xresources
def read_xresources(prefix):
    props = {}
    x = subprocess.run(['xrdb', '-query'], stdout=subprocess.PIPE)
    lines = x.stdout.decode().split('\n')
    for line in filter(lambda l : l.startswith(prefix), lines):
        prop, _, value = line.partition(':\t')
        props[prop] = value
    return props

xresources = read_xresources('*')
#c.colors.statusbar.normal.bg = xresources['*.background']
# }}}2

# {{{2 colors.completion.*
c.colors.completion.fg = solarized_base1
c.colors.completion.odd.bg = solarized_base02
c.colors.completion.even.bg = solarized_base03
c.colors.completion.category.fg = solarized_cyan
c.colors.completion.category.bg = solarized_base02
c.colors.completion.category.border.top = solarized_base02
c.colors.completion.category.border.bottom = solarized_base01
c.colors.completion.item.selected.fg = solarized_base01
c.colors.completion.item.selected.bg = solarized_base02
c.colors.completion.item.selected.border.top = solarized_green
c.colors.completion.item.selected.border.bottom = solarized_green
c.colors.completion.match.fg = solarized_green
c.colors.completion.scrollbar.fg = solarized_base1
c.colors.completion.scrollbar.bg = solarized_base02

# {{{2 colors.downloads.*
c.colors.downloads.bar.bg = solarized_base03
c.colors.downloads.start.fg = solarized_cyan
c.colors.downloads.start.bg = solarized_base02
c.colors.downloads.stop.fg = solarized_base01
c.colors.downloads.stop.bg = solarized_base02
c.colors.downloads.error.fg = solarized_red
c.colors.downloads.error.bg = solarized_base02

# {{{2 colors.hints.*
c.colors.hints.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(88, 110, 117, 0.8), stop:1 rgba(7, 54, 66, 0.8))'
c.colors.hints.fg = "#fff"  # solarized_base2
c.colors.hints.match.fg = solarized_green

# {{{2 colors.keyhint.*
c.colors.keyhint.bg = solarized_base03
c.colors.keyhint.fg = solarized_base1
c.colors.keyhint.suffix.fg = solarized_blue

# {{{2 colors.messages.*
c.colors.messages.error.fg = solarized_base3
c.colors.messages.error.bg = solarized_orange
c.colors.messages.error.border = solarized_red
c.colors.messages.warning.fg = solarized_base2
c.colors.messages.warning.bg = solarized_yellow
c.colors.messages.warning.border = solarized_violet
c.colors.messages.info.fg = solarized_base1
c.colors.messages.info.bg = solarized_base02
c.colors.messages.info.border = solarized_base00

# {{{2 colors.prompts.*
c.colors.prompts.fg = solarized_base0
c.colors.prompts.bg = solarized_base02
c.colors.prompts.border = solarized_cyan
c.colors.prompts.selected.bg = solarized_base01

# {{{2 colors.statusbar.*
c.colors.statusbar.normal.fg = solarized_green
c.colors.statusbar.normal.bg = solarized_base02
c.colors.statusbar.insert.fg = solarized_blue
c.colors.statusbar.insert.bg = solarized_base02
c.colors.statusbar.passthrough.fg = solarized_cyan
c.colors.statusbar.passthrough.bg = solarized_base02
c.colors.statusbar.private.fg = solarized_base00
c.colors.statusbar.private.bg = solarized_base02
c.colors.statusbar.command.fg = solarized_base1
c.colors.statusbar.command.bg = solarized_base02
c.colors.statusbar.command.private.fg = solarized_base1
c.colors.statusbar.command.private.bg = solarized_base02
c.colors.statusbar.caret.fg = solarized_violet
c.colors.statusbar.caret.bg = solarized_base02
c.colors.statusbar.caret.selection.fg = solarized_blue
c.colors.statusbar.caret.selection.bg = solarized_base02
c.colors.statusbar.progress.bg = solarized_cyan

# {{{2 colors.statusbar.url.*
c.colors.statusbar.url.fg = solarized_base1
c.colors.statusbar.url.error.fg = solarized_yellow
c.colors.statusbar.url.hover.fg = solarized_base1
c.colors.statusbar.url.success.http.fg = solarized_cyan
c.colors.statusbar.url.success.https.fg = solarized_green
c.colors.statusbar.url.warn.fg = solarized_violet

# {{{2 colors.tabs.*
c.colors.tabs.bar.bg = solarized_base03
c.colors.tabs.indicator.start = solarized_blue
c.colors.tabs.indicator.stop = solarized_cyan
c.colors.tabs.indicator.error = solarized_red
c.colors.tabs.odd.fg = solarized_base1
c.colors.tabs.odd.bg = solarized_base02
c.colors.tabs.even.fg = solarized_base1
c.colors.tabs.even.bg = solarized_base03
c.colors.tabs.selected.odd.fg = solarized_base03
c.colors.tabs.selected.odd.bg = solarized_base1
c.colors.tabs.selected.even.fg = solarized_base03
c.colors.tabs.selected.even.bg = solarized_base1
c.colors.tabs.pinned.odd.fg = solarized_base2
c.colors.tabs.pinned.odd.bg = solarized_base02
c.colors.tabs.pinned.even.fg = solarized_base2
c.colors.tabs.pinned.even.bg = solarized_base03
c.colors.tabs.pinned.selected.odd.fg = solarized_base03
c.colors.tabs.pinned.selected.odd.bg = solarized_base2
c.colors.tabs.pinned.selected.even.fg = solarized_base03
c.colors.tabs.pinned.selected.even.bg = solarized_base2

# {{{1 fonts.* (theme)
c.fonts.monospace = '"TerminessTTF Nerd Font", monospace'

c.fonts.keyhint = '12pt monospace'
c.fonts.messages.error = '12pt monospace'
c.fonts.messages.info = '12pt monospace'
c.fonts.messages.warning = '12pt monospace'

c.fonts.hints = 'bold 16px "xos4 Terminess Powerline"'
c.fonts.tabs = 'bold 18px "xos4 Terminess Powerline"'

# vim: ft=python fileencoding=utf-8 sts=4 sw=4 et
