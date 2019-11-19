(use-modules (gnu)
             (gnu packages web-browsers)
             (gnu packages xdisorg)
             (gnu packages bittorrent)
             (gnu packages pulseaudio)
             (gnu packages wine)
             (nongnu packages wine)
             (gnu packages suckless)
             (lib))
(os-part '()
         (list ;; for user mounts
          qutebrowser next
          xcape
          mpv
          qbittorrent
          pavucontrol
          wine
          winetricks
          slock
          )
         '())
