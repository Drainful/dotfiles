(require 'org)
(setq vc-follow-symlinks t) ;; follow the symlink without asking
(org-babel-load-file "~/.emacs.d/emacs.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   '("100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "99b2fdc7de612b74fcb76eb3a1962092cf729909223434f256c7007d490d787a" "3e2fd26606cba08448283cc16860c1deab138ede73c38c91fdaf4e5c60ece485" "07ed389142fef99649ebcfe1f835cf564fc40bb342d8d2f4e13f05302378a47a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "5e5345ea15d0c2234356bc5958a224776b83198f0c3df7155d1f7575405ce990" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "b563a87aa29096e0b2e38889f7a5e3babde9982262181b65de9ce8b78e9324d5" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default))
 '(dired-filter-stack nil)
 '(elfeed-feeds
   '("https://nitter.com/Dezznsfw/rss"
     ("https://nitter.com/Dezznsfw/media/rss" nitter-media nsfw)
     ("https://nitter.com/Dezznsfw/with_replies/rss" nitter nsfw)
     ("https://nitter.com/Dezznsfw/rss" nitter-no-replies nsfw)))
 '(evil-collection-company-use-tng nil)
 '(exwm-input-global-keys
   '(([8388722]
      . exwm-reset)
     ([f11]
      . exwm-layout-toggle-fullscreen)
     ([8388641]
      lambda
      (command)
      (interactive
       (list
        (read-shell-command "! ")))
      (setq me/exwm-always-home-dir t)
      (start-process-shell-command command nil command))
     ([8388672]
      lambda
      (command)
      (interactive
       (list
        (read-shell-command "@ ")))
      (setq me/exwm-always-home-dir nil)
      (start-process-shell-command command nil command))
     ([8388704]
      . helm-qutebrowser)
     ([8388657]
      . eshell-here)
     ([8388658]
      lambda nil
      (interactive)
      (find-file default-directory))
     ([8388659]
      lambda nil
      (interactive)
      (eshell-at-or-switch "~/"))
     ([s-f1]
      . previous-buffer)
     ([s-f2]
      . next-buffer)
     ([print]
      lambda nil
      (interactive)
      (let
          ((command "screengrab"))
        (start-process-shell-command command nil command)))
     ([XF86MonBrightnessUp]
      . desktop-brightness-increment-large)
     ([XF86MonBrightnessDown]
      . desktop-brightness-decrement-large)
     ([S-XF86MonBrightnessUp]
      . desktop-brightness-increment-normal)
     ([S-XF86MonBrightnessDown]
      . desktop-brightness-decrement-normal)
     ([C-XF86MonBrightnessUp]
      . desktop-brightness-increment-small)
     ([C-XF86MonBrightnessDown]
      . desktop-brightness-decrement-small)
     ([XF86AudioRaiseVolume]
      . desktop-volume-increment-large)
     ([XF86AudioLowerVolume]
      . desktop-volume-decrement-large)
     ([S-XF86AudioRaiseVolume]
      . desktop-volume-increment-normal)
     ([S-XF86AudioLowerVolume]
      . desktop-volume-decrement-normal)
     ([C-XF86AudioRaiseVolume]
      . desktop-volume-increment-small)
     ([C-XF86AudioLowerVolume]
      . desktop-volume-decrement-small)
     ([XF86AudioMute]
      . pulseaudio-control-toggle-current-sink-mute)
     ([s-f12]
      . ttp)
     ([s-f11]
      lambda nil
      (interactive)
      (start-process-shell-command "toggle redshift" nil "pkill -USR1 '^redshift$'"))
     ([8388640]
      . exlaunch)
     ([s-return]
      . helm-winconf)
     ([8388726]
      . evil-window-vsplit)
     ([8388723]
      . evil-window-split)
     ([8388712]
      . windmove-left)
     ([8388714]
      . windmove-down)
     ([8388715]
      . windmove-up)
     ([8388716]
      . windmove-right)
     ([s-right]
      . windows-right)
     ([s-down]
      . evil-quit)
     ([s-up]
      . delete-other-windows)
     ([s-left]
      . windows-left)
     ([8388719]
      . desktop-environment-lock-screen)
     ([8388700]
      . symon-mode)
     ([8388655]
      . exwm-floating-toggle-floating)
     ([s-tab]
      . evil-buffer)
     ([8388656]
      . org-agenda)
     ([8388725]
      . hydra-winner/winner-undo)
     ([8388730]
      . helm-M-x)
     ([8388667]
      . evil-ex)
     ([8388706]
      . me/helm-buffer)
     ([8388710]
      . helm-find-files)
     ([8388727]
      . actually-kill-this-buffer)
     ([8388721]
      . evil-delete-buffer)
     ([8388705]
      . bury-evil-buffer)
     ([8388673]
      . unbury-interesting-buffer)
     ([8388711]
      . guix)
     ([8388679]
      . keyboard-quit)))
 '(fci-rule-color "#5B6268" t)
 '(helm-completion-style 'emacs)
 '(helm-source-names-using-follow
   '("Imenu in variables.lisp" "Imenu in progn.lisp" "Imenu" "describe-variable" "describe-function" "Emacs buffers" "qute-browse" "EXWM buffers"))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(line-spacing 0.2)
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   '("/home/guix/adrian/Documents/notes/Appointments.org" "/home/guix/adrian/Documents/notes/Ideas.org" "/home/guix/adrian/Documents/notes/Notes.org" "/home/guix/adrian/Documents/notes/Remember.org" "/home/guix/adrian/Documents/notes/Remember.sync-conflict-20200413-080211-M5AHMQ4.org" "/home/guix/adrian/Documents/notes/Tasks.org" "/home/guix/adrian/Documents/notes/gpt-inspirations.org" "/home/guix/adrian/Documents/notes/small-number-of-vaguely-lisp-adjacent-thoughts.org"))
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item)))
 '(package-selected-packages
   '(smalltalk-mode typescript-mode exwm-mff transmission envrc evil-space unicode-fonts flycheck-meghanada meghanda pinentry olivetti auto-package-update symon pdf-tools purp-theme minimal-theme tango-plus-theme espresso-theme sly love-minor-mode rust-mode tuareg lua-mode aumix-mode fancy-battery nameless dmenu badwolf-theme dired-atool exwm-firefox-core exwm-firefox-evil helm-exwm desktop-environment forth-mode skeletor direnv 0blayout sml-mode evil-collection toc-org nix-buffer evil-goggles modern-cpp-font-lock which-key emacs-which-key comint exwm-systemtray poet-theme exwm emmet-mode multi-term helm-fzf theme-changer circe shen-emacs shen-mode fzf sql-indent evil-surround lispyville lispy ivy company-lsp lsp-ui cquery markdown-mode sublimity-scroll evil-snipe snipe evil-easymotion evil-search-highlight-persisist color-theme-approximate emacs-dashboard cider macrostep quelpa ac-slime julia-repl company-nixos-options load-theme-buffer-local doom-themes company clojure-mode general sly-quicklisp flycheck-pos-tip rainbowdelimiters rainbow-delimiters mic-paren evil-vimish-fold rainbow-delimeters evil-cleverparens darkroom elm-mode flycheck-elm haskell-mode nix-mode helm-projectile flycheck restart-emacs projectile delight evil use-package))
 '(pdf-view-midnight-colors (cons "#F8F8F0" "#5a5475"))
 '(rustic-ansi-faces
   ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.mail.yahoo.com")
 '(smtpmail-smtp-service 587)
 '(symon-mode nil)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
