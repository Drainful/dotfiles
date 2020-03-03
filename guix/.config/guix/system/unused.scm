(define-public my-qtwebengine
  (package (inherit qtwebengine)
           (arguments
            `(#:phases
              (modify-phases %standard-phases
                             (add-before 'configure 'configure-qmake
                                         (lambda* (#:key inputs outputs #:allow-other-keys)
                                           (let* ((out (assoc-ref outputs "out"))
                                                  (qtbase (assoc-ref inputs "qtbase"))
                                                  (tmpdir (string-append (getenv "TMPDIR")))
                                                  (qmake (string-append tmpdir "/qmake"))
                                                  (qt.conf (string-append tmpdir "/qt.conf")))
                                             ;; Use qmake with a customized qt.conf to override install
                                             ;; paths to $out.
                                             (symlink (which "qmake") qmake)
                                             (setenv "CC" "gcc")
                                             (setenv "PATH" (string-append tmpdir ":" (getenv "PATH")))
                                             (with-output-to-file qt.conf
                                               (lambda ()
                                                 (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5
" out out qtbase)))
                                             #t)))
                             (replace 'configure
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        ;; Valid QT_BUILD_PARTS variables are:
                                        ;; libs tools tests examples demos docs translations
                                        (invoke "qmake" "QT_BUILD_PARTS = libs tools" "--"
                                                ;; "--webengine-printing-and-pdf=no"
                                                "--webengine-ffmpeg=system"
                                                "--webengine-icu=system"
                                                ;; "--webengine-pepper-plugins=no"
                                                )))
                             (add-before 'check 'set-display
                                         (lambda _
                                           ;; make Qt render "offscreen", required for tests
                                           (setenv "QT_QPA_PLATFORM" "offscreen")
                                           #t))
                             (add-after 'install-binaries 'install-qt.conf
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (let* ((out (assoc-ref outputs "out"))
                                                 (tmpdir (string-append (getenv "TMPDIR")))
                                                 (in.conf (string-append tmpdir "/qt.conf"))
                                                 (out.conf (string-append out "/lib/qt5/libexec/qt.conf")))
                                            (copy-file in.conf out.conf))
                                          #t)))))))

(define-public my-python-pyqt
  (package
    (inherit python-pyqt)
    (inputs
     `(("python" ,python-wrapper)
       ("my-qtwebengine" ,my-qtwebengine)
       ("qtbase" ,qtbase)
       ("qtconnectivity" ,qtconnectivity)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsensors" ,qtsensors)
       ("qtserialport" ,qtserialport)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebsockets" ,qtwebsockets)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sip (string-append out "/share/sip"))
                    (plugins (string-append out "/plugins"))
                    (designer (string-append plugins "/designer"))
                    (qml (string-append plugins "/PyQt5"))
                    (python (assoc-ref inputs "python"))
                    (python-version
                     (last (string-split python #\-)))
                    (python-major+minor
                     (string-join
                      (take (string-split python-version #\.) 2)
                      "."))
                    (lib (string-append out "/lib/python"
                                        python-major+minor
                                        "/site-packages"))
                    (stubs (string-append lib "/PyQt5")))
               (invoke "python" "configure.py"
                       "--confirm-license"
                       "--bindir" bin
                       "--destdir" lib
                       "--designer-plugindir" designer
                       "--qml-plugindir" qml
                                        ; Where to install the PEP 484 Type Hints stub
                                        ; files. Without this the stubs are tried to be
                                        ; installed into the python package's
                                        ; site-package directory, which is read-only.
                       "--stubsdir" stubs
                       "--sipdir" sip)))))))))

(define-public my-qutebrowser
  (package
    (name "qutebrowser")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/qutebrowser/"
                           "qutebrowser/releases/download/v" version "/"
                           "qutebrowser-" version ".tar.gz"))
       (sha256
        (base32
         "0ckffbw2zlg0afz4rgyywzdprnqs74va5qj0xqlaqc14ziiypxnw"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)))
    (inputs
     `(("my-qtwebengine" ,my-qtwebengine)
       ("my-python-pyqt" ,my-python-pyqt)
       ("python-setuptools" ,python-setuptools)
       ;; ("python-markupsafe" ,python-markupsafe)
       ("python-pypeg2" ,python-pypeg2)
       ("python-jinja2" ,python-jinja2)
       ("python-pygments" ,python-pygments)
       ("python-pyyaml" ,python-pyyaml)
       ("python-attrs" ,python-attrs)
       ("python-cssutils" ,python-cssutils)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (app (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor")))
               (invoke "a2x" "-f" "manpage" "doc/qutebrowser.1.asciidoc")
               (install-file "doc/qutebrowser.1"
                             (string-append out "/share/man/man1"))

               (for-each
                (lambda (i)
                  (let ((src  (format #f "icons/qutebrowser-~dx~d.png" i i))
                        (dest (format #f "~a/~dx~d/apps/qutebrowser.png"
                                      hicolor i i)))
                    (mkdir-p (dirname dest))
                    (copy-file src dest)))
                '(16 24 32 48 64 128 256 512))
               (install-file "icons/qutebrowser.svg"
                             (string-append hicolor "/scalable/apps"))
               
               ;; (substitute* "qutebrowser.desktop"
               ;;   (("Exec=qutebrowser")
               ;;    (string-append "Exec=" out "/bin/qutebrowser")))
               ;; (install-file "qutebrowser.desktop" app)
               #t))))))
    (home-page "https://qutebrowser.org/")
    (synopsis "Minimal, keyboard-focused, vim-like web browser")
    (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt5 and QtWebKit.")
    (license license:gpl3+)))
