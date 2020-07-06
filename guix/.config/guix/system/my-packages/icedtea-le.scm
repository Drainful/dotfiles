(define-module (my-packages icedtea)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages java)
  #:use-module (gnu packages certs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (ice-9 match)
  #:use-module (lib)


  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ghostscript) ;lcms
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux) ;alsa
  #:use-module (gnu packages maths)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages texinfo)
  #:use-module ((srfi srfi-1) #:select (fold alist-delete))
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match))


(define-public icedtea-le
  (package
    (inherit icedtea)
    (name "icedtea-le")
    (version (string-append (package-version icedtea) "-le"))
    (native-inputs (cons*
                    `("le-certs" ,le-certs)
                    (package-native-inputs icedtea)))
    (arguments
     (plist-set
      (package-arguments icedtea)
      #:phases
      `(modify-phases ,(plist-get (package-arguments icedtea) #:phases)
         (replace 'install-keystore
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((keystore  "cacerts")
                    (nss-certs-dir (string-append (assoc-ref inputs "nss-certs")
                                                  "/etc/ssl/certs"))
                    (le-certs-dir (string-append (assoc-ref inputs "le-certs")
                                                 "/etc/ssl/certs"))
                    (keytool   (string-append (assoc-ref outputs "jdk")
                                              "/bin/keytool")))
               (define (extract-cert file target)
                 (call-with-input-file file
                   (lambda (in)
                     (call-with-output-file target
                       (lambda (out)
                         (let loop ((line (read-line in 'concat))
                                    (copying? #f))
                           (cond
                            ((eof-object? line) #t)
                            ((string-prefix? "-----BEGIN" line)
                             (display line out)
                             (loop (read-line in 'concat) #t))
                            ((string-prefix? "-----END" line)
                             (display line out)
                             #t)
                            (else
                             (when copying? (display line out))
                             (loop (read-line in 'concat) copying?)))))))))
               (define (import-cert cert)
                 (format #t "Importing certificate ~a\n" (basename cert))
                 (let ((temp "tmpcert"))
                   (extract-cert cert temp)
                   (let ((port (open-pipe* OPEN_WRITE keytool
                                           "-import"
                                           "-alias" (basename cert)
                                           "-keystore" keystore
                                           "-storepass" "changeit"
                                           "-file" temp)))
                     (display "yes\n" port)
                     (when (not (zero? (status:exit-val (close-pipe port))))
                       (format #t "failed to import ~a\n" cert)))
                   (delete-file temp)))

               ;; This is necessary because the certificate directory contains
               ;; files with non-ASCII characters in their names.
               (setlocale LC_ALL "en_US.utf8")
               (setenv "LC_ALL" "en_US.utf8")

               (for-each import-cert (find-files nss-certs-dir "\\.pem$"))
               (for-each import-cert (find-files le-certs-dir "\\.pem$"))
               (mkdir-p (string-append (assoc-ref outputs "out")
                                       "/lib/security"))
               (mkdir-p (string-append (assoc-ref outputs "jdk")
                                       "/jre/lib/security"))

               ;; The cacerts files we are going to overwrite are chmod'ed as
               ;; read-only (444) in icedtea-8 (which derives from this
               ;; package).  We have to change this so we can overwrite them.
               (chmod (string-append (assoc-ref outputs "out")
                                     "/lib/security/" keystore) #o644)
               (chmod (string-append (assoc-ref outputs "jdk")
                                     "/jre/lib/security/" keystore) #o644)

               (install-file keystore
                             (string-append (assoc-ref outputs "out")
                                            "/lib/security"))
               (install-file keystore
                             (string-append (assoc-ref outputs "jdk")
                                            "/jre/lib/security"))))))))))
;; (define-public icedtea-8-3.16.0
;;   (package
;;     (inherit icedtea-8)
;;     (version "3.16.0")))


(define-public icedtea-8-3.16.0
  (let* ((version "3.16.0")
         (drop (lambda (name hash)
                 (origin
                   (method url-fetch)
                   (uri (string-append
                         "http://icedtea.classpath.org/download/drops"
                         "/icedtea8/" version "/" name ".tar.xz"))
                   (sha256 (base32 hash))))))
    (package (inherit icedtea-7)
             (version "3.16.0")
             (source (origin
                       (method url-fetch)
                       (uri (string-append
                             "http://icedtea.wildebeest.org/download/source/icedtea-"
                             version ".tar.xz"))
                       (sha256
                        (base32
                         "0015snd3d2jbcshda02mrf373kkvgdw0l8yff23002dgidkvfjxq"))
                       (modules '((guix build utils)))
                       (snippet
                        '(begin
                           (substitute* '("configure"
                                          "acinclude.m4")
                             ;; Do not embed build time
                             (("(DIST_ID=\"Custom build).*$" _ prefix)
                              (string-append prefix "\"\n"))
                             ;; Do not leak information about the build host
                             (("DIST_NAME=\"\\$build_os\"")
                              "DIST_NAME=\"guix\""))
                           #t))))
             (arguments
              `(#:imported-modules
                ((guix build ant-build-system)
                 (guix build syscalls)
                 ,@%gnu-build-system-modules)
                ,@(substitute-keyword-arguments (package-arguments icedtea-7)
                    ((#:modules modules)
                     `((guix build utils)
                       (guix build gnu-build-system)
                       ((guix build ant-build-system) #:prefix ant:)
                       (ice-9 match)
                       (ice-9 popen)
                       (srfi srfi-19)
                       (srfi srfi-26)))
                    ((#:configure-flags flags)
                     `(let ((jdk (assoc-ref %build-inputs "jdk")))
                        `( ;;"--disable-bootstrap"
                          "--enable-bootstrap"
                          "--enable-nss"
                          "--disable-downloading"
                          "--disable-system-pcsc"
                          "--disable-system-sctp"
                          "--disable-tests"  ;they are run in the check phase instead
                          "--with-openjdk-src-dir=./openjdk.src"
                          ,(string-append "--with-jdk-home=" jdk))))
                    ((#:phases phases)
                     `(modify-phases ,phases
                        (delete 'fix-x11-extension-include-path)
                        (delete 'patch-paths)
                        (delete 'set-additional-paths)
                        (delete 'patch-patches)
                        (delete 'patch-bitrot)
                        ;; Prevent the keytool from recording the current time when
                        ;; adding certificates at build time.
                        (add-after 'unpack 'patch-keystore
                          (lambda _
                            (substitute* "openjdk.src/jdk/src/share/classes/sun/security/provider/JavaKeyStore.java"
                              (("date = new Date\\(\\);")
                               "\
date = (System.getenv(\"SOURCE_DATE_EPOCH\") != null) ?\
new Date(Long.parseLong(System.getenv(\"SOURCE_DATE_EPOCH\"))) :\
new Date();"))
                            #t))
                        (add-after 'unpack 'patch-jni-libs
                          ;; Hardcode dynamically loaded libraries.
                          (lambda _
                            (let* ((library-path (search-path-as-string->list
                                                  (getenv "LIBRARY_PATH")))
                                   (find-library (lambda (name)
                                                   (search-path
                                                    library-path
                                                    (string-append "lib" name ".so")))))
                              (for-each
                               (lambda (file)
                                 (catch 'decoding-error
                                   (lambda ()
                                     (substitute* file
                                       (("VERSIONED_JNI_LIB_NAME\\(\"(.*)\", \"(.*)\"\\)"
                                         _ name version)
                                        (format #f "\"~a\""  (find-library name)))
                                       (("JNI_LIB_NAME\\(\"(.*)\"\\)" _ name)
                                        (format #f "\"~a\"" (find-library name)))))
                                   (lambda _
                                     ;; Those are safe to skip.
                                     (format (current-error-port)
                                             "warning: failed to substitute: ~a~%"
                                             file))))
                               (find-files "openjdk.src/jdk/src/solaris/native"
                                           "\\.c|\\.h"))
                              #t)))
                        (replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let ((doc (string-append (assoc-ref outputs "doc")
                                                      "/share/doc/icedtea"))
                                  (jre (assoc-ref outputs "out"))
                                  (jdk (assoc-ref outputs "jdk")))
                              (copy-recursively "openjdk.build/docs" doc)
                              (copy-recursively "openjdk.build/images/j2re-image" jre)
                              (copy-recursively "openjdk.build/images/j2sdk-image" jdk)
                              ;; Install the nss.cfg file to JRE to enable SSL/TLS
                              ;; support via NSS.
                              (copy-file (string-append jdk "/jre/lib/security/nss.cfg")
                                         (string-append jre "/lib/security/nss.cfg"))
                              #t)))
                        (add-after 'install 'strip-jar-timestamps
                          (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))))
             (native-inputs
              `(("jdk" ,icedtea-7 "jdk")
                ("openjdk-src"
                 ,(drop "openjdk"
                        "1mj6xgmw31i6qd30qi9dmv7160fbcfq5ikz1jwjihdg2793il19p"))
                ("aarch32-drop"
                 ,(drop "aarch32"
                        "1wb8k5zm40zld0986dvmlh5xh3gyixbg9h26sl662zy92amhmyyg"))
                ("corba-drop"
                 ,(drop "corba"
                        "11ma4zz0599cy70xd219v7a8vin7p96xrhhz3wsaw6cjhkzpagah"))
                ("jaxp-drop"
                 ,(drop "jaxp"
                        "14m1y0z0fbm5z5zjw3vnq85py8dma84bi3f9cw8rhdyc6skk8q4i"))
                ("jaxws-drop"
                 ,(drop "jaxws"
                        "09andnm6xaasnp963hgx42yiflifiljp9z7z85jrfyc5z8a5whmf"))
                ("jdk-drop"
                 ,(drop "jdk"
                        "0s6lcpc0zckz2fnq98aqf28nz9y3wbi41a3kyaqqa2abwbkm1zwl"))
                ("langtools-drop"
                 ,(drop "langtools"
                        "15wizy123vhk40chl1b4p552jf2pw2hdww0myf11qab425axz4nw"))
                ("hotspot-drop"
                 ,(origin
                    (method url-fetch)
                    (uri (string-append
                          "http://icedtea.classpath.org/download/drops"
                          "/icedtea8/" version "/hotspot.tar.xz"))
                    (sha256
                     (base32
                      "1ciz1w9j0kz7s1dxdhyqq71nla9icyz6qvn0b9z2zgkklqa98qmm"))
                    (patches (search-patches
                              "icedtea-7-hotspot-gcc-segfault-workaround.patch"))))
                ("nashorn-drop"
                 ,(drop "nashorn"
                        "19pzl3ppaw8j6r5cnyp8qiw3hxijh3hdc46l39g5yfhdl4pr4hpa"))
                ("shenandoah-drop"
                 ,(drop "shenandoah"
                        "0k33anxdzw1icn072wynfmmdjhsv50hay0j1sfkfxny12rb3vgdy"))
                ,@(fold alist-delete (package-native-inputs icedtea-7)
                        '("jdk" "openjdk-src" "corba-drop" "jaxp-drop" "jaxws-drop"
                          "jdk-drop" "langtools-drop" "hotspot-drop")))))))

(define-public java-junit
  (package
    (name "java-junit")
    (version "4.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/junit-team/junit/")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j8avi91px1z8rjc89cfikwrvfifdmmsarwiyrcnr59ynvpz0v8h"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled jar archives.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,openjdk12
       #:tests? #f ; no tests
       #:jar-name "junit.jar"))
    (inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "https://junit.org/junit4/")
    (synopsis "Test framework for Java")
    (description
     "JUnit is a simple framework to write repeatable tests for Java projects.
JUnit provides assertions for testing expected results, test fixtures for
sharing common test data, and test runners for running tests.")
    (license license:epl1.0)))

java-junit
