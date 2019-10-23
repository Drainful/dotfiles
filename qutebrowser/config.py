# smooth scrolling
c.scrolling.smooth = False

# hide statusbar
c.statusbar.hide = True

c.qt.highdpi = True
c.zoom.default = 67

c.tabs.tabs_are_windows = True
c.tabs.show = "never"
c.window.title_format = "{perc}{title} -- {current_url}"

# search engine
c.url.searchengines = {"DEFAULT": "https://google.com/search?hl=en&q={}"}

# keybindings
config.bind('xm', 'spawn --userscript view_in_mpv')

# default page
c.url.default_page = "about:blank"

# command aliases
c.aliases['reader-mode'] = "spawn --userscript readability"
c.aliases['bitwarden'] = "spawn --userscript qute-bitwarden"
c.aliases['mpv'] = "spawn --userscript view_in_mpv"
