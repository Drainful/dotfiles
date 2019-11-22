(use-modules (gnu)
             (gnu packages xorg)
             (gnu packages bittorrent)
             (gnu packages pulseaudio)
             (gnu packages wine)
             (nongnu packages wine)
             (gnu packages suckless)

             (gnu packages disk)

             (gnu packages gnome)

             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages xdisorg)

             (gnu packages imagemagick)
             (my-packages)
             (lib))
(os-part (list (load "./next.scm"))
         (list
          my-qutebrowser
          xcape
          xrdb
          mpv
          qbittorrent
          pavucontrol
          wine
          winetricks
          slock

          gparted

          gnome-icon-theme

          font-gnu-freefont-ttf
          font-tex-gyre
          font-ubuntu
          font-dejavu
          font-terminus
          font-liberation
          fontconfig
          xfontsel

          imagemagick
          xdpyinfo)
         (list))
