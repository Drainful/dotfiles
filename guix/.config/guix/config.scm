(use-modules (gnu)
             (gnu system nss)
             (gnu packages vim)
             (gnu packages version-control)
             (gnu packages curl)
             (gnu packages wget)
             (gnu packages compression)
             (gnu packages shells)
             (gnu packages emacs)
             (gnu packages video)
             (gnu packages admin)
             (gnu packages file)
             (gnu packages linux)
             (gnu packages readline)
             (gnu packages wine)
             (gnu packages web-browsers)
             (gnu packages xdisorg)
             (gnu packages mpd)
             (gnu packages bittorrent)
             (gnu packages pulseaudio)
             (nongnu packages linux)
             (nongnu packages wine)
             (nongnu packages compression))

(use-service-modules desktop xorg)
(use-package-modules certs gnome)

(define packages
  (cons*

   ;;; core
   nss-certs
   gvfs ;; for user mounts
   neovim
   git 
   curl
   wget

   ;; archive managment
   atool
   unrar
   unzip
   zip
   
   youtube-dl
   ffmpeg
   neofetch
   file
   usbutils
   rlwrap
   wine
   winetricks
   xcape
   aumix
   mpd

   ;;; guistuff
   qutebrowser next
   mpv
   qbittorrent
   pavucontrol

   ;;; emacs
   emacs
   fish

   %base-packages))

(operating-system

 (kernel linux-5.3)
 (firmware (list linux-firmware))

 (host-name "guix-yoga")
 (timezone "America/Chicago")
 (locale "en_US.utf8")

 ;; Choose US English keyboard layout.  The "altgr-intl"
 ;; variant provides dead keys for accented characters.
 (keyboard-layout (keyboard-layout "us" "altgr-intl"))

 ;; Use the UEFI variant of GRUB with the EFI System
 ;; Partition mounted on /boot/efi.
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")
              (keyboard-layout keyboard-layout)))

 (file-systems (append
                (list (file-system
                       (device (file-system-label "guix-root"))
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (file-system-label "home"))
                       (mount-point "/home")
                       (type "ext4"))
                      (file-system
                       (device "/dev/nvme0n1p1")
                       (mount-point "/boot/efi")
                       (type "vfat")))
                %base-file-systems))

 (users (cons (user-account
               (name "adrian")
               (group "users")
               (supplementary-groups '("wheel" "netdev"
                                       "audio" "video"))
               (home-directory "/home/guix/adrian"))
              %base-user-accounts))

 ;; This is where we specify system-wide packages.
 (packages packages)

 ;; Add GNOME and Xfce---we can choose at the log-in screen
 ;; by clicking the gear.  Use the "desktop" services, which
 ;; include the X11 log-in service, networking with
 ;; NetworkManager, and more.
 (services (append (list (service gnome-desktop-service-type)
                         (set-xorg-configuration
                          (xorg-configuration
                           (keyboard-layout keyboard-layout))))
                   %desktop-services))

 ;; Allow resolution of '.local' host names with mDNS.
 (name-service-switch %mdns-host-lookup-nss))
