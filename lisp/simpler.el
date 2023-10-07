;;; simpler.el --- simpler emacs for emacs newhands  -*- lexical-binding:t -*-

;; Author: Peerin
;; Version: 0.01
;; Package-Requires: ()
;; Keywords: 
;; URL: 

;;; Commentary:

;; 

(defun simpler-mode--outlook ()
  (delete-other-windows)
  (menu-bar-mode (if (null simpler-mode)
		     -1
		   1))
  (scroll-bar-mode (if (null simpler-mode)
		       -1
		     1))
  )

(defun simpler-mode--build ()
  (interactive)
  (let* ((source-file-complete-name (buffer-file-name))
	 (source-file-extension (file-name-extension source-file-complete-name))
	 (source-file-name (concat
			    (file-name-base source-file-complete-name)
			    "." source-file-extension))
	 (output-file-name (file-name-base source-file-complete-name))
	 (output-file-complete-name (concat
				     (file-name-directory source-file-complete-name)
				     (file-name-base source-file-complete-name)))
	 (compiler (cond ((string= source-file-extension "c")
			  '("gcc" "clang"))
			 ((or
			   (string= source-file-extension "cpp")
			   (string= source-file-extension "cxx")
			   (string= source-file-extension "cc"))
			  '("g++" "clang++"))))
	 (command (concat (car compiler) " " source-file-complete-name " -o " output-file-complete-name))
	 (output (shell-command-to-string command)))
    (message output)
    ))

(defun simpler-mode--run ()
  (interactive)
  (let* ((current-source-file (buffer-file-name))
	 (executable-file-complete-name (concat (file-name-directory current-source-file)
						(file-name-base current-source-file))))
    (if (not (file-exists-p executable-file-complete-name))
	(message "executable file not found!")
      (let ((default-directory (file-name-directory current-source-file)))
	(start-process-shell-command "simpler-mode--run terminal" nil (concat "gnome-terminal -- sh -c \"" executable-file-complete-name " ; read -n1 -r -p \'Press any key to continue...\'\""))))))

(defun simpler-mode--build-and-run ()
  (interactive)
  (simpler-mode--build)
  (simpler-mode--run))

(defun simpler-mode--menu ()
  (define-key global-map [menu-bar simpler]
	      (cons "Simpler" (make-sparse-keymap "simpler"))
	      (if (null simpler-mode)
		  t
		nil))
  (define-key global-map
	      [menu-bar simpler build]
	      '("Build" . simpler-mode--build)
	      (if (null simpler-mode)
		  t
		nil))
  (define-key global-map
	      [menu-bar simpler run]
	      '("Run" . simpler-mode--run)
	      (if (null simpler-mode)
		  t
		nil))
    (define-key global-map
	      [menu-bar simpler build-and-run]
	      '("Build and Run" . simpler-mode--build-and-run)
	      (if (null simpler-mode)
		  t
		nil)))

(define-minor-mode simpler-mode nil
  :global t
  :lighter " Simpler"
  :keymap '(([?\C-s] . save-buffer)
	    ([?\C-c] . kill-ring-save)
	    ([?\C-x] . kill-region)
	    ([?\C-v] . yank)
	    ([?\C-z] . undo)
	    ([?\C-f] . isearch-forward)
	    ([?\C-a] . mark-whole-buffer))
  (simpler-mode--outlook)
  (simpler-mode--menu))

(provide 'simpler)
