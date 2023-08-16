;;; yiyan-frame-title.el --- Append yiyan to frame title.  -*- lexical-binding:t -*-

;; Author: Peerin
;; Version: 0.01
;; Package-Requires: ()
;; Keywords: 
;; URL: 

;;; Commentary:

;; 

(defgroup yiyan-frame-title
  nil
  "Append yiyan to frame title.")

(defcustom yiyan-frame-title-format
  "❤️%b - GNU Emacs❤️"
  "Basic frame title format string same to `frame-title-format'.
It will be the former part of the frame title."
  :group 'yiyan-frame-title)

(defun yiyan-frame-title-update ()
  "Retrieve yiyan from the Internet asynchronously, update and show it after."
  (interactive)
  (url-retrieve "http://v1.hitokoto.cn?encode=text"
		(lambda (status)
		  (if (not (null  (plist-get status :error)))
		      (message (plist-get status :error))
		    (search-forward-regexp "^$")
		    (forward-char)
		    (setq frame-title-format
			  (concat yiyan-frame-title-format
				  " | "
				  (decode-coding-string (buffer-substring (point) (buffer-end 1)) 'prefer-utf-8)))
		    (force-window-update)))))

;;run after requiring this package
(yiyan-frame-title-update)

(provide 'yiyan-frame-title)
