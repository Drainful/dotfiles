(define-module (my-packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  ;; #:use-module (nongnu packages qt)
  #:use-module (gnu packages image)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gcc)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix cvs-download)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages music)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages wordnet)
  #:use-module (gnu packages libusb)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public my-emacs-exwm
  (package
    (name "my-emacs-exwm")
    (version "0.23")
    (synopsis "Emacs X window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/exwm-"
                                  version ".tar"))
              (sha256
               (base32
                "05w1v3wrp1lzz20zd9lcvr5nhk809kgy6svvkbs15xhnr6x55ad5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-xelb" ,emacs-xelb)))
    (inputs
     `(("xhost" ,xhost)
       ("dbus" ,dbus)))
    ;; The following functions and variables needed by emacs-exwm are
    ;; not included in emacs-minimal:
    ;; scroll-bar-mode, fringe-mode
    ;; x-display-pixel-width, x-display-pixel-height
    (arguments
     `(#:emacs ,emacs
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-xsession
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (bin (string-append out "/bin"))
                    (exwm-executable (string-append bin "/exwm")))
               ;; Add a .desktop file to xsessions
               (mkdir-p xsessions)
               (mkdir-p bin)
               (with-output-to-file
                   (string-append xsessions "/exwm.desktop")
                 (lambda _
                   (format #t "[Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a~@
                     TryExec=~:*~a~@
                     Type=Application~%" ,name ,synopsis exwm-executable)))
               ;; Add a shell wrapper to bin
               (with-output-to-file exwm-executable
                 (lambda _
                   (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     [ -f \"$HOME/.xinitrc\" ] && . \"$HOME/.xinitrc\" ~@
                     ~a --daemon ~@
                     exec ~a --exit-with-session ~a --create-frame --eval '~s' \"$@\" ~%"
                           (string-append (assoc-ref inputs "bash") "/bin/sh")
                           (string-append (assoc-ref inputs "xhost") "/bin/xhost")
                           (string-append (assoc-ref inputs "emacs") "/bin/emacs")
                           (string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
                           (string-append (assoc-ref inputs "emacs") "/bin/emacsclient")
                           '(cond
                             ((file-exists-p "~/.exwm")
                              (load-file "~/.exwm"))
                             ((not (featurep 'exwm))
                              (require 'exwm)
                              (require 'exwm-config)
                              (exwm-config-default)
                              (message (concat "exwm configuration not found. "
                                               "Falling back to default configuration...")))))))
               (chmod exwm-executable 365)
               #t))))))
    (home-page "https://github.com/ch11ng/exwm")
    (description "EXWM is a full-featured tiling X window manager for Emacs
built on top of XELB.")
    (license license:gpl3+)))

(define-public my-libplist
  (package
    (name "libplist")
    (version "42bb64ba966082b440cb68cbdadf317f44710017")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libimobiledevice/libplist.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19yw80yblq29i2jx9yb7bx0lfychy9dncri3fk4as35kq5bf26i8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'configure-later
           ;; Don't run ./configure during bootstrap.
           (lambda _
             (setenv "NOCONFIGURE" "set")
             #t)))
       ;; Tests fail randomly when run in parallel because several of them write
       ;; and read to/from the same file--e.g., "4.plist" is accessed by
       ;; 'large.test' and 'largecmp.test'.
       #:parallel-tests? #f))
    (inputs
     `(("python" ,python)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython))) ; to build Python bindings
    (home-page "https://www.libimobiledevice.org/")
    (synopsis "C library to handle Apple Property List files")
    (description "This package provides a small portable C library to handle
Apple Property List files in binary or XML.")
    (license license:lgpl2.1+)))

(define-public my-libusbmuxd
  (package
    (name "libusbmuxd")
    (version "873252dc8b4e469c7dc692064ac616104fca5f65")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libimobiledevice/libusbmuxd.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qx3q0n1f2ajfm3vnairikayzln6iyb2y0i7sqfl8mj45ahl6wyj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("automake" ,automake)
       ("libplist" ,my-libplist)))
    (home-page "https://www.libimobiledevice.org/")
    (synopsis "Library to multiplex connections from and to iOS devices")
    (description "This package provides a client library to multiplex
connections from and to iOS devices by connecting to a socket provided by a
@code{usbmuxd} daemon.")
    (license license:lgpl2.1+)))

(define-public my-libimobiledevice
  (package
    (name "libimobiledevice")
    (version "61babf5f54e7734ebf3044af4c6294524d4b29b5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libimobiledevice/libimobiledevice.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02dnq6xza72li52kk4p2ak0gq2js3ssfp2fpjlgsv0bbn5mkg2hi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "PYTHON_LDFLAGS=-L"
                            (assoc-ref %build-inputs "python")
                            "/lib -lpython"
                            ,(version-major+minor (package-version python))
                            "m"))))
    (propagated-inputs
     `(("openssl" ,openssl-1.0)
       ("libplist" ,my-libplist)
       ("libusbmuxd" ,my-libusbmuxd)))
    (inputs
     `(("python" ,python)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)
       ("libtool" ,libtool)))
    (home-page "https://www.libimobiledevice.org/")
    (synopsis "Protocol library and tools to communicate with Apple devices")
    (description "libimobiledevice is a software library that talks the
protocols to support Apple devices.  It allows other software to easily access
the device's file system, retrieve information about the device and its
internals, backup/restore the device, manage installed applications, retrieve
address books, calendars, notes, and bookmarks, and (using libgpod) synchronize
music and video to the device.")
    (license license:lgpl2.1+)))

(define-public my-ifuse
  (package
    (name "ifuse")
    (version "e75d32c34d0e8b80320f0a007d5ecbb3f55ef7f0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libimobiledevice/ifuse.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b9w2i0sliswlkkb890l9i0rxrf631xywxf8ihygfmjdsfw47h1m"))))
    (inputs
     `(("fuse" ,my-fuse)
       ("libimobiledevice" ,my-libimobiledevice)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (home-page "https://www.libimobiledevice.org/")
    (synopsis "Mount iOS devices")
    (description "This package provides @command{ifuse}, a command to mount
iOS devices and access their contents.")
    (license license:lgpl2.1+)))

(define-public my-usbmuxd
  (package
    (name "usbmuxd")
    (version "9af2b12552693a47601347e1eafc1e94132d727e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libimobiledevice/usbmuxd.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m64idbnnl5kklxl1bypf7zmmq9hdrc6lcdwkz482519sgavsdwj"))))
    (inputs
     `(("libplist" ,my-libplist)
       ("libusb" ,libusb)
       ("libimobiledevice" ,my-libimobiledevice)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (home-page "https://www.libimobiledevice.org/")
    (synopsis "Multiplex connections over USB to an iOS device")
    (description "This package provides the @code{usbmuxd} daemon
which multiplexes connections over USB to an iOS device.  To
users, it means you can sync your music, contacts, photos, etc.
over USB.")
    (license license:gpl2+)))
