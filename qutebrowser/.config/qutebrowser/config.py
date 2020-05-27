# blocked hosts
c.content.host_blocking.lists = ["https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"]
# privacy
c.content.javascript.enabled = False;
c.content.cookies.accept = "never";

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
c.url.searchengines = {"DEFAULT": "https://duckduckgo.com/?q={}",
                       "g": "https://google.com/search?hl=en&q={}",
                       "yt": "https://www.youtube.com/results?search_query={}"}
c.url.start_pages = ["about:blank"]

# keybindings
config.bind('xM', 'spawn --userscript view_in_mpv')
config.bind('xm', 'hint links userscript view_in_mpv')
config.bind('xm', 'hint links userscript view_in_mpv')
config.bind('Q', 'config-cycle scrolling.bar always never')
config.bind(',s', 'config-cycle -t -p content.javascript.enabled')
config.bind(',h', 'config-cycle -t -p content.host_blocking.enabled')
config.bind(',b', 'config-cycle -t -p statusbar.hide')
config.bind(',c', 'config-cycle -p content.cookies.accept never no-3rdparty')

# default page
c.url.default_page = "about:blank"

# command aliases
c.aliases['reader-mode'] = "spawn --userscript readability"
c.aliases['bitwarden'] = "spawn --userscript qute-bitwarden"
c.aliases['mpv'] = "spawn --userscript view_in_mpv"
c.aliases['pass'] = "spawn --userscript password_fill"

# ui
c.scrolling.bar = "never"

# style
c.content.user_stylesheets = "./user.css"

# external programs
c.editor.command = ["emacsclient", "{}"]

