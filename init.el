;; -*- lexical-binding: t; -*-

;; simple convenient functions (complex ones are stored in ./lisp)

(defun my/open-init-file ()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

;; layout

(setq display-line-numbers-type 'relative) ;;;relative line number

(global-display-line-numbers-mode)

(toggle-frame-maximized)

(menu-bar-mode -1)

(tool-bar-mode -1)

(set-face-attribute 'default nil :height 150) ;;;set the default font size

(scroll-bar-mode -1)

(pixel-scroll-precision-mode 1) ;;smooth scrolling

;; globally bind keys

(define-key (current-global-map) (kbd "<f2>") #'my/open-init-file)

;; vanllia emacs variables' setting

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
	("https" . "127.0.0.1:7890")
	("no_proxy" . "0.0.0.0")))

(setq backup-inhibited t) ;;disable automatically backup files suffixed with "~"

(setq initial-scratch-message ";;I❤️Emacs.\n\n")

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
	    (let ((local-map (current-local-map)))
	      (define-key local-map (kbd "C-x C-s") (lambda ()
						      (interactive)
						      (message "*Scratch* buffer should not be saved!"))))))

;; builtin and local packages
;;org-mode
(use-package org
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-image-actual-width nil)
  (defvar my/new-article-directory "~/rlog/")
  (defvar my/new-article-author "Peerin")
  (defun my/new-article (path)
    "scaffold a new article."
    (interactive "F")
    (if (file-directory-p path)
	(message "cannot specify a directory!")
      (find-file path)
      (insert (concat "#+title: " (file-name-base path) "\n"
		      "#+author: " my/new-article-author "\n"
		      "#+date: " "<" (current-time-string) ">\n"
		      "\n")))))

(require 'yiyan-frame-title)

(require 'simpler)

(progn
  ;;open local emacs server
  (server-force-delete)
  (server-start))

;;awesome-tray(local)
(progn
  (require 'awesome-tray)
  (make-thread
   (lambda ()
     (sleep-for 3)
     (awesome-tray-mode 1))
   "enable awesome-tray-mode"))

;; third-party packages
;;slime
(progn
  (setq inferior-lisp-program "/usr/bin/sbcl"))
;;google-translate
(progn
  (require 'google-translate)
  (require 'google-translate-default-ui)
  ;;(global-set-key "\C-ct" 'google-translate-at-point)
  ;;(global-set-key "\C-cT" 'google-translate-query-translate)
  ;;(require 'google-translate-smooth-ui)
  ;;(global-set-key "\C-ct" 'google-translate-smooth-translate)
  (setq google-translate-default-target-language "zh-CN")
  (setq google-translate-default-source-language "auto"))
;;iedit
(progn
  (require 'iedit))
;;tempel
(progn
  (global-set-key (kbd "C-c SPC") #'tempel-complete)
  (global-set-key (kbd "C-c i") #'tempel-insert))
;;tempel-collection
(progn
  (require 'tempel-collection))
;;vertico
(progn
  (vertico-mode 1))
;;corfu
(progn
  (global-corfu-mode 1)
  (setq corfu-auto t))
;;eglot
(progn
  (require 'eglot)
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure)
  (add-hook 'python-mode-hook #'eglot-ensure))
;;eat
(progn
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))
;;beacon
(use-package beacon
  :init (beacon-mode 1))
;;goggles
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing
;;elfeed
(use-package elfeed
  :bind (("C-c f" . elfeed))
  :config
  (setq elfeed-feeds
	'("https://manateelazycat.github.io/feed.xml"
	  "http://feeds.feedburner.com/ruanyifeng"
	  "https://www.solidot.org/index.rss"
	  "https://rsshub.app/luogu/daily"
	  "http://www.ruanyifeng.com/blog/atom.xml"
	  "https://linux.cn/rss.xml"
	  ;;"https://hnrss.org/frontpage"
	  "https://hackernewsrss.com/feed.xml"
	  "https://planet.emacslife.com/atom.xml"
	  "https://irreal.org/blog/?feed=rss2"
	  ))
  )
;;geiser
(use-package geiser
  )
;;geiser-guile
(use-package geiser-guile
  :after geiser)
;;org2ctex
;; delay the loading of the package to the first use of org-export-dispatch
(add-hook 'org-mode-hook (lambda ()
			   (advice-add 'org-export-dispatch :before
				       (lambda (r)
					 (use-package org2ctex
					   :config (org2ctex-toggle t))))))
;;fanyi
(use-package fanyi
  :ensure t
  :bind (("C-c T" . fanyi-dwim)
	 ("C-c t" . fanyi-dwim2))
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))
;;telega
(use-package telega
  :config
  (setq telega-server-libs-prefix "/home/user/Documents/aur-builds/telegram-tdlib-git/pkg/telegram-tdlib-git/usr")
  (setq telega-proxies
	'((:server "127.0.0.1" :port 7890 :enable t :type (:@type "proxyTypeSocks5")))))
;;nov
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(material))
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940"
     default))
 '(erc-modules
   '(autojoin button completion fill imenu irccontrols list match menu
	      move-to-prompt netsplit networks noncommands readonly
	      ring sasl stamp track))
 '(erc-sasl-auth-source-function 'erc-sasl-auth-source-password-as-host)
 '(erc-sasl-user "peerin")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(lua-mode nov posframe xpm ement telega fanyi org2ctex geiser-guile
	      geiser material-theme elfeed goggles beacon vue-mode
	      zig-mode eat tempel-collection tempel corfu eglot
	      vertico iedit cmake-mode google-translate slime
	      elvish-mode go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
