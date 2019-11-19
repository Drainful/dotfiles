(use-modules (gnu)
             (gnu packages vim)
             (gnu packages version-control)
             (gnu packages curl)
             (gnu packages wget)
             (gnu packages video)
             (gnu packages compression)
             (gnu packages shells)
             (gnu packages admin)
             (gnu packages file)
             (gnu packages linux)
             (gnu packages readline)
             (gnu packages mpd)
             (nongnu packages compression)
             (lib))

(os-part '()
         (list neovim ; for when emacs is dead
               git
               curl
               wget
               youtube-dl
               ffmpeg
               neofetch
               file
               usbutils
               ;; udisks
               rlwrap
               aumix
               mpd

               ;; archives
               atool
               unrar
               unzip
               zip) 
         '())
