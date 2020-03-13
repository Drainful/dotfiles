# blocked hosts
c.content.host_blocking.lists = ['https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/social/hosts']

# smooth scrolling
c.scrolling.smooth = False

# hide statusbar
c.statusbar.hide = False # todo: set true when browsing an image file?

c.qt.highdpi = True
# c.zoom.default = 67

c.new_instance_open_target = "window"
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
config.bind(',h', 'config-cycle -t -p content.host_blocking.enabled')
config.bind(',s', 'config-cycle -t -p statusbar.hide')

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

# external programs
c.editor.command = ["emacsclient", "{}"]
