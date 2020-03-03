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
             (gnu packages admin)
             (gnu packages elf)
             (gnu packages readline)
             (gnu packages package-management)
             (gnu packages mpd)
             (gnu packages compression)
             (gnu packages base)
             (gnu packages ssh)
             (gnu packages busybox)
             (gnu packages shellutils)
             (gnu packages java)
             (gnu packages lisp)
             ;; (nongnu packages compression)
             (lib))

(os-part (list)
         (list neovim ; for when emacs is dead
               git
               curl
               wget
               youtube-dl
               ffmpeg
               neofetch
               file
               usbutils
               iptables
               netcat
               patchelf
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
               zip

               busybox ;; utilities

               direnv ;; directory-specific environment variables
               
               ;; used as an application, not for development
               openjdk12
               sbcl
               ) 
         (list))
