;;; init.el --- Jeremy's Emacs Configuration
;;;

;;; Code:

;;
;; Private settings such as email addresses, (gulp) clear-text passwords, ...
;;

;(load "~/.emacs.d/private.el")

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

;;; Scrolling
(set-scroll-bar-mode 'right)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
		(lambda () (interactive)
		  (condition-case nil (scroll-up)
		    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
		(lambda () (interactive)
		  (condition-case nil (scroll-down)
		    (beginning-of-buffer (goto-char (point-min))))))

;;; (ido-mode 1) ; First few days use just couldn't get use to it.
(setq show-trailing-whitespace t)
(setq indicate-empty-lines t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

(global-hl-line-mode t)
(set-face-background 'hl-line "#eff")

(delete-selection-mode)

(defun get-line (num)
  "Return the string content of line `num' relative to the current line"
  (save-excursion
    (next-line num)
    (set-mark-command nil)
    (end-of-line)
    (buffer-substring-no-properties (mark) (point))))

;; Duplicate what is the same in the previous two lines
(defun duplicate-similar ()
  "Duplicate what is the same in the previous two lines from the current cursor column"
  (interactive)
  (let ((line1 (get-line -2))
	(line2 (get-line -1)))
  (setq end-index (compare-strings line1 0 nil
				   line2 0 nil))
  (cond ((symbolp end-index)
	 (setq end-index (length line1)))
	((> 0 end-index)
	 (setq end-index (- (- end-index) 1)))
	((< 0 end-index)
	 (setq end-index (- end-index 1))))
  (insert-string (substring line1 0 end-index))))

;; Redefine the Home/End keys to (nearly) the same as visual studio behaviour
;; special home and end by Shan-leung Maverick WOO

(defun My-smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'My-smart-home)
	   (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text))
  )

(defun My-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'My-smart-end)
           (= (line-end-position) (point)))
      (end-of-line-text)
    (end-of-line))
  )

(defun end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
	(backward-char 1))
      (unless (= (point) bol)
	(forward-char 1) (skip-chars-backward " \t\n"))))
  )

(defun dos-to-unix ()
  "Convert the current buffer from DOS format to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix)
  (save-buffer))

(defun unix-to-dos ()
  "Convert the current buffer from UNIX format to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos)
  (save-buffer))

;;; Automatic indentation

(define-key global-map (kbd "RET") 'newline-and-indent)
(setq indent-tabs-mode nil)
(setq tab-width 4)

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

;; Line handling
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'find-file-check-line-endings)

(defun dos-file-endings-p ()
  (string-match "dos" (symbol-name buffer-file-coding-system)))

(defun find-file-check-line-endings ()
  (when (dos-file-endings-p)
    (setq show-trailing-whitespace t)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))

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

(require 'tea-time)
(require 'task-timer)

(require 'go-mode-load)
(add-hook 'go-mode-hook
	  '(lambda ()
	     (setq tab-width 3)))

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
;;(add-hook 'rhtml-mode-hook (lambda () (rinari-launch)))

(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
(semantic-mode 1)
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(require 'ecb-autoloads)
(setq ecb-options-version "2.40")
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
(setq ecb-layout-name "leftright1")
(setq ecb-expand-methods-switch-off-auto-expand nil)

;; Key bindings
(global-set-key [home] 'My-smart-home)
(global-set-key [end] 'My-smart-end)
(global-set-key (kbd "<ESC>SPC") 'duplicate-similar)
(define-key global-map "\C-ctt" 'tea-time)
(define-key global-map "\C-ctb" 'task-timer-begin)
(define-key global-map "\C-cts" 'task-timer-status)

(custom-set-variables
 '(ecb-layout-window-sizes (quote (("leftright1" (ecb-directories-buffer-name 0.22127659574468084 . 0.3709677419354839) (ecb-sources-buffer-name 0.22127659574468084 . 0.3064516129032258) (ecb-history-buffer-name 0.22127659574468084 . 0.3064516129032258) (ecb-methods-buffer-name 0.2 . 0.9838709677419355)) ("left5" (ecb-directories-buffer-name 0.18220338983050846 . 0.29310344827586204) (ecb-sources-buffer-name 0.18220338983050846 . 0.3448275862068966) (ecb-history-buffer-name 0.18220338983050846 . 0.3448275862068966)) ("left8" (ecb-directories-buffer-name 0.2711864406779661 . 0.29310344827586204) (ecb-sources-buffer-name 0.2711864406779661 . 0.2413793103448276) (ecb-methods-buffer-name 0.2711864406779661 . 0.27586206896551724) (ecb-history-buffer-name 0.2711864406779661 . 0.1724137931034483)))))
 '(show-trailing-whitespace t))
(custom-set-faces)
