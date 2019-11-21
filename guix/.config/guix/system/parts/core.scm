(use-modules (gnu)
             (gnu packages vim)
             (gnu packages version-control)
             (gnu packages curl)
             (gnu packages wget)
             (gnu packages video)
             (gnu packages admin)
             (gnu packages code)
             (gnu packages file)
             (gnu packages linux)
             (gnu packages readline)
             (gnu packages package-management)
             (gnu packages mpd)
             (gnu packages compression)
             (gnu packages base)
             (gnu packages ssh)
             ;; (nongnu packages compression)
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
               rlwrap
               mpd
               stow
               openssh

               tree
               the-silver-searcher

               ;; archives
               atool
               ;; unrar
               unzip
               zip) 
         ;; (list (udisks-service)
         ;;       (bluetooth-service))
         '()
         )
