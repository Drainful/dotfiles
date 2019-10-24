# smooth scrolling
c.scrolling.smooth = False

# hide statusbar
c.statusbar.hide = True

c.qt.highdpi = True
c.zoom.default = 67

c.tabs.tabs_are_windows = True
c.tabs.show = "never"
c.window.title_format = "{perc}{current_title} -- {current_url}"

# url
c.url.searchengines = {"DEFAULT": "https://google.com/search?hl=en&q={}"}
c.url.start_pages = ["about:blank"]

# keybindings
config.bind('xM', 'spawn --userscript view_in_mpv')
config.bind('xm', 'hint links userscript view_in_mpv')
config.bind('Q', 'config-cycle scrolling.bar always never')

# default page
c.url.default_page = "about:blank"

# command aliases
c.aliases['reader-mode'] = "spawn --userscript readability"
c.aliases['bitwarden'] = "spawn --userscript qute-bitwarden"
c.aliases['mpv'] = "spawn --userscript view_in_mpv"

# ui
c.scrolling.bar = "never"

# style
c.content.user_stylesheets = "./user.css"
