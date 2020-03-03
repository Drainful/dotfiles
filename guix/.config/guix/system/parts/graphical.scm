(use-modules (gnu)
             (gnu packages xorg)
             (gnu packages video)
             (gnu packages bittorrent)
             (gnu packages pulseaudio)
             (gnu packages wine)
             (nongnu packages wine)
             (gnu packages disk)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages xdisorg)
             (gnu packages imagemagick)
             ;; (gnu packages graphvis)
             (gnu packages gl)
             (gnu packages lxqt)
             (my-packages)
             (lib))

(os-part (list (load "./next.scm"))
         (list
          qutebrowser
          xcape
          xrdb
          xrandr

          mpv

          qbittorrent

          pavucontrol

          wine
          winetricks

          gparted

          ;; fonts
          font-gnu-freefont-ttf
          font-tex-gyre
          font-ubuntu
          font-dejavu
          font-terminus
          font-liberation
          font-inconsolata
          font-gnu-unifont
          font-adobe-source-han-sans
          
          fontconfig
          xfontsel
          redshift

          imagemagick
          xdpyinfo
          
          ;; graphvis
          mesa-utils

          screengrab)
         (list))
