;;; init.el --- Jeremy's Emacs Configuration
;;;

;;; Code:

;;
;; Private settings such as email addresses, (gulp) clear-text passwords, ...
;;

(load "~/.emacs.d/private.el")

;;

(defun load-my-init ()
  "Load my custom emacs init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(add-to-list 'load-path "~/.emacs.d")

(defalias 'yes-or-no-p 'y-or-n-p)

(if (eq system-type "windows-nt")
    (setq default-directory "C:/Development/Projects"))
(setq global-font-lock-mode t)
(setq generic-define-unix-modes t)
(setq column-number-mode t)
(setq inhibit-splash-screen t)
(show-paren-mode 1)
(tool-bar-mode 0)

;;; (ido-mode 1) ; First few days use just couldn't get use to it.
(setq show-trailing-whitespace t)
(setq indicate-empty-lines t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

(global-hl-line-mode t)
(set-face-background 'hl-line "#eff")

;;; Automatic indentation

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun indent-buffer ()
  "Indent the current buffer"
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(defun tab-to-spaces ()
  "Convert tab to spaces"
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     html-mode       rhtml-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))


;;; Backup

(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.backups"))
 delete-old-versions t
 kept-new-versions 25
 kept-old-version 2
 version-control t)

(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)

;;(defun cleanup-backup-directory ()
;;  (interactive)
;;  (message "Deleting old backup files...")
;;  (let ((week (* 60 60 24 7))
;;	(current (float-time (current-time))))
;;    (dolist (file (directory-files "~/.backups" t))
;;      (when (and (backup-file-name-p file)
;;		 (> (- current (float-time (fifth (file-attributes file))))
;;		    week))
;;	(message file)
;;	(delete-file file)))))

;; ERROR
;;
;; fifth is not defined in the above function.
;;
;; (cleanup-backup-directory)

;;; Modes

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook (lambda () (rinari-launch)))

(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)
