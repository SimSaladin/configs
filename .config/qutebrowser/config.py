# ~/.config/qutebrowser/config.py
#
# <url:qute://help/>
# <url:/usr/lib/python3.7/site-packages/qutebrowser>
# <url:https://www.wufoo.com/html5/spellcheck-attribute/>

from qutebrowser.api import cmdutils
from qutebrowser.misc import utilcmds, objects

# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

TOR_PROXY = 'socks5://localhost:9050'

# command: pass                                                          {{{1
c.aliases['pass'] = 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-pass ' + '-d dmenu -m -U secret -u "username: (.+)" -P "(.+)"'
config.bind('zl', 'pass')
config.bind('zul', 'pass --username-only')
config.bind('zpl', 'pass --password-only')
config.bind('zol', 'pass --otp-only')

# command: open-mpv                                                      {{{1
c.aliases['open-mpv'] = 'spawn mpv "{url}"'
config.bind('zv', 'open-mpv')

# command: set-log-capacity                                              {{{1
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

# command: pyeval                                                        {{{1
if 'pyeval' not in objects.commands.keys():
    @cmdutils.register(name='pyeval', maxsplit=0, no_cmd_split=True)
    def pyeval(string, file=False, quiet=True):
        """Evaluate a python string and display the results as a web page.

        Args:
            string: The string to evaluate.
            file: Interpret s as a path to file, also implies --quiet.
            quiet: Don't show the output in a new tab.
        """
        utilcmds.debug_pyeval(string, file, quiet)

# command: dictcli                                                       {{{1
c.aliases['dictcli'] = 'spawn --output /usr/share/qutebrowser/scripts/dictcli.py'

# c.                                                                     {{{1

# how to open links in existing instance, when link opened from outside that
# instance.
# Values: tab / tab-bg / tab-silent / tab-bg-silent / window
c.new_instance_open_target = "window"

# Which window to choose when opening links as new tabs (when
# new_instance_open_target != "window").
# Values: first-opened / last-opened / last-focused / last-visible
# new_instance_open_target_window = ""

c.history_gap_interval = 180  # mins

c.confirm_quit = ["downloads"]

# c.qt.                                                                  {{{1

# c.qt.highdpi = True  # default False

# Share process for buffers to same site
# c.qt.process_model = 'process-per-site'

# c.window.                                                              {{{1

c.window.hide_decoration = True
c.window.title_format = 'qb:{host}{title_sep}{current_title}'

# c.editor.                                                              {{{1
c.editor.command = [
    'urxvtc', '-name', 'info-terminal', '-geometry', '100x40',
    '-e', 'vim', '-f', '{file}', '-c', 'normal {line}G{column0}l']

# c.messages.                                                            {{{1

# Set to positive number to clear messages older than that
c.messages.timeout = 3000  # msecs

# c.auto_save.                                                           {{{1

# Restore open sites when started.
c.auto_save.session = True  # bool
c.auto_save.interval = 15000  # msec

# c.completion.                                                          {{{1
c.completion.height = '95%'
c.completion.scrollbar.width = 0
c.completion.delay = 200  # msec

# c.session.                                                             {{{1
c.session.lazy_restore = True

# c.spellcheck.                                                          {{{1
c.spellcheck.languages = ['de-DE', 'en-GB', 'en-US']
# note, spellcheck requires qtwebkit-plugins package installed.

# c.input.insert_mode.                                                   {{{1
c.input.insert_mode.auto_enter = True  # enter insert on editable focus
c.input.insert_mode.auto_leave = True  # exit insert on non-editable focus
c.input.insert_mode.auto_load = False  # focus editable elem after page load
c.input.insert_mode.leave_on_load = True
c.input.insert_mode.plugins = True  # focusing flash/plugins turns on insert

# c.content.                                                             {{{1
c.content.ssl_strict = True
c.content.default_encoding = 'utf-8'
c.content.plugins = False
c.content.pdfjs = True
c.content.mute = True
c.content.autoplay = False
c.content.webgl = True
c.content.notifications = False
c.content.media_capture = 'ask'  # true / false / ask
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.content.local_content_can_access_remote_urls = True
# Allow websites register protocol handlers (navigator.registerProtocolHandler)
c.content.register_protocol_handler = False  # True / False / 'ask'

# site-specific
config.set('content.mute', False, 'https://*.flowdock.com/*')
config.set('content.notifications', True, 'https://*.flowdock.com/*')

# c.content.headers.                                                     {{{1
#c.content.headers.accept_language = 'en-US,en;q=0.5'
#c.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36'
#c.content.headers.custom = {"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}

# c.content.javascript.                                                  {{{1
c.content.javascript.enabled = True
c.content.javascript.can_open_tabs_automatically = True
c.content.javascript.log['unknown'] = "debug"

config.set('content.javascript.enabled', True, 'file://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

# c.url.                                                                 {{{1
c.url.default_page = 'https://start.duckduckgo.com/'
c.url.auto_search = 'dns'
c.url.open_base_url = True

# c.url.searchengines {}                                                 {{{1
# Maps a search engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
# Type: Dict
c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'ddg': 'https://duckduckgo.com/?q={}',
    'g': 'https://www.google.com/?q={}',
    'g.maps': 'https://www.google.com/maps/search/{}',
    # wiki
    'w.en': 'https://en.wikipedia.org/w/index.php?search={}',
    'w.fi': 'https://fi.wikipedia.org/w/index.php?search={}',
    'w.archlinux': 'https://wiki.archlinux.org/?search={}',
    'w.relexsolutions': 'https://wiki.relexsolutions.com/dosearchsite.action?queryString={}',
    # shops
    'ebay.com': 'https://www.ebay.com/sch/i.html?_nkw={}',
    'amazon.de': 'https://www.amazon.de/s?field-keywords={}',
    'mouser.at': 'https://www.mouser.at/Search/Refine?Keyword={}',
    # tech
    'blockchain': 'https://www.blockchain.com/search?search={}',
    'dockerhub': 'https://hub.docker.com/search?q={}&type=image',
    'ghc': 'https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/search.html?q={}',
    'hackage': 'https://hackage.haskell.org/packages/search?terms={}',
    'python': 'https://docs.python.org/3/search.html?q={}',
    'stackage': 'https://www.stackage.org/lts/hoogle?q={}',
    # forums
    'lastfm': 'https://www.last.fm/search?q={}',
    'nyaasi': 'https://nyaa.si/?f=0&c=1_2&q={}',
    'rutracker': 'https://rutracker.org/forum/tracker.php?nm={}',
    'imdb': 'https://www.imdb.com/find?s=all&q={}',
    'mal': 'https://myanimelist.net/search/all?q={}',
}

GOOGLE_TRANSLATE_URL = 'https://translate.google.com'
TRANSLATE_LANGUAGES = ['fi', 'en', 'de']

for f in TRANSLATE_LANGUAGES:
    for t in TRANSLATE_LANGUAGES:
        c.url.searchengines[f+t] = GOOGLE_TRANSLATE_URL + '/#'+f+'/'+t+'/{}'

# c.tabs.                                                                {{{1
c.tabs.position = 'left'
c.tabs.show = 'multiple'  # 'always' / 'never' / 'multiple' / 'switching'
c.tabs.show_switching_delay = 4500
c.tabs.width = '15%'
c.tabs.padding = {'top': 0, 'bottom': 0, 'left': 5, 'right': 0}
c.tabs.last_close = 'close'
# Values: blank / close / default-page / ignore / startpage
c.tabs.select_on_remove = 'prev'
# Size of per window retained closed stacks.
c.tabs.undo_stack_size = 10
# middleclick/ctrl+click opens tab in the background
c.tabs.background = True
c.tabs.new_position.stacking = True
c.tabs.new_position.related = 'next'  # first / last / next / prev
c.tabs.new_position.unrelated = 'last'
c.tabs.title.alignment = 'left'  # left / right / center
c.tabs.title.format = '{audio:^4}{index:>2}:{current_title}'
c.tabs.title.format_pinned = '{audio:^4}{index:>2}:{current_title}'
c.tabs.favicons.scale = 1.0
c.tabs.favicons.show = 'always'  # always / never / pinned

# c.statusbar.                                                           {{{1

# hide statusbar when there's nothing interesting.
c.statusbar.hide = False

# c.downloads.location.                                                  {{{1
c.downloads.location.directory = '~/Downloads'
c.downloads.location.suggestion = 'both'

# c.fonts.                                                               {{{1

FT_LOW_DPI = '12px "xos4 Terminess Powerline"'
FT_HIGH_DPI = 'bold 18px "xos4 Terminess Powerline"'

FONT_ACTIVE = FT_HIGH_DPI

# TODO different if high DPI
c.fonts.default_family = "bold 'xos4 Terminess Powerline'"
c.fonts.default_size = '18px'  # 10pt

c.fonts.hints = FONT_ACTIVE
c.fonts.keyhint = FONT_ACTIVE
c.fonts.messages.error = FONT_ACTIVE
c.fonts.messages.info = FONT_ACTIVE
c.fonts.messages.warning = FONT_ACTIVE
c.fonts.prompts = "default_size default_family"
c.fonts.statusbar = "default_size default_family"
c.fonts.tabs = FONT_ACTIVE

# c.fonts.web.cursive.standard
# c.fonts.web.fantasy.standard
# c.fonts.web.fixed.standard
# c.fonts.web.sans-serif.standard
# c.fonts.web.serif.standard
# c.fonts.web.family.standard

c.fonts.web.size.default = 16  # 16 (px)
c.fonts.web.size.default_fixed = 12  # 13 (px)

# c.colors.                                                              {{{1

# solarized_*                                                            {{{2
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

# colors.completion.*                                                    {{{2
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

# colors.downloads.*                                                     {{{2
c.colors.downloads.bar.bg = solarized_base03
c.colors.downloads.start.fg = solarized_cyan
c.colors.downloads.start.bg = solarized_base02
c.colors.downloads.stop.fg = solarized_base01
c.colors.downloads.stop.bg = solarized_base02
c.colors.downloads.error.fg = solarized_red
c.colors.downloads.error.bg = solarized_base02

# colors.hints.*                                                         {{{2
c.colors.hints.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgba(88, 110, 117, 0.8), stop:1 rgba(7, 54, 66, 0.8))'
c.colors.hints.fg = "#fff"  # solarized_base2
c.colors.hints.match.fg = solarized_green

# colors.keyhint.*                                                       {{{2
c.colors.keyhint.bg = solarized_base03
c.colors.keyhint.fg = solarized_base1
c.colors.keyhint.suffix.fg = solarized_blue

# colors.messages.*                                                      {{{2
c.colors.messages.error.fg = solarized_base3
c.colors.messages.error.bg = solarized_orange
c.colors.messages.error.border = solarized_red
c.colors.messages.warning.fg = solarized_base2
c.colors.messages.warning.bg = solarized_yellow
c.colors.messages.warning.border = solarized_violet
c.colors.messages.info.fg = solarized_base1
c.colors.messages.info.bg = solarized_base02
c.colors.messages.info.border = solarized_base00

# colors.prompts.*                                                       {{{2
c.colors.prompts.fg = solarized_base0
c.colors.prompts.bg = solarized_base02
c.colors.prompts.border = solarized_cyan
c.colors.prompts.selected.bg = solarized_base01

# colors.statusbar.*                                                     {{{2
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

# colors.statusbar.url.*                                                 {{{2
c.colors.statusbar.url.fg = solarized_base1
c.colors.statusbar.url.error.fg = solarized_yellow
c.colors.statusbar.url.hover.fg = solarized_base1
c.colors.statusbar.url.success.http.fg = solarized_cyan
c.colors.statusbar.url.success.https.fg = solarized_green
c.colors.statusbar.url.warn.fg = solarized_violet

# colors.tabs.*                                                          {{{2
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

# }}}1

# vim:ft=python:et:tw=79:sts=4:sw=4:
