
(add-to-list 'load-path "~/.emacs.d/")

;;; Global stuff
(if (not (string-match "XEmacs\\|Lucid" emacs-version))
		(global-font-lock-mode 1)
	(font-lock-mode t))

(font-lock-mode)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

; Get rid of the new emacs bug involving the cursor
(setq-default column-number-mode t)

;; Turn off startup message
(setq inhibit-startup-message t)

;; Goto line
(global-set-key "\M-g" 'goto-line)

;; Don't echo passwords in shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Turn "yes or no" into "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Go to shell buffer
(defun start-shell ()
	(interactive)
	(shell)
	(load-library "ansi-color")
	(ansi-color-for-comint-mode-on)
	(goto-char (point-max))) 

;; Replace Meta 's' with our custom start-shell keybinding
(global-unset-key "\M-s")
(global-set-key "\M-s" 'start-shell)

;; Cycle buffer
(load "~/.emacs.d/cycle-buffer/load")

;; Long lines mode provides soft word wrapping instead of the slash c\rap
(autoload 'longlines-mode "longlines.el" "Minor mode for automatically wrapping long lines." t)

;;;; Languages ;;;;

;; Ruby mode
(load "~/.emacs.d/ruby/load")

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Python mode
(load "~/.emacs.d/python/load")

;; Javascript mode
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;; Haml mode
(autoload 'haml-mode "~/.emacs.d/haml-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; Sass mode
(autoload 'sass-mode "~/.emacs.d/sass-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Chuck mode
(autoload 'chuck-mode "~/.emacs.d/chuck-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.ck$" . chuck-mode))

;; Php mode
(autoload 'php-mode "~/.emacs.d/php-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))

;; Arc mode
(autoload 'arc-mode "~/.emacs.d/arc-mode.el" nil t)
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))

;; Nginx mode :)
(autoload 'nginx-mode "~/.emacs.d/nginx-mode.el" nil t)
(add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))

;; CSS Mode

;; First remove css-mode if it already exists
(rassq-delete-all 'css-mode auto-mode-alist)

(autoload 'css-mode "~/.emacs.d/css-mode-simple.el" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; Treetop mode
;; (autoload 'treetop-mode "treetop" nil t)
;; (add-to-list 'auto-mode-alist '("\\.treetop$" . treetop-mode))

;; Haskell Mode

(load "~/.emacs.d/haskell-mode-2.7.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq auto-mode-alist (cons '("\\.hs\\'" . haskell-mode) auto-mode-alist))


;; Add HTML & RHTML mode
(setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html.erb$" . html-mode) auto-mode-alist))


;; Autosave 'autosave~' and Backup '#backup#' files go into /tmp 
(load "~/.emacs.d/autosave")

;; Grep commands
(autoload 'grep "igrep" "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'egrep "igrep" "*Run `egrep`..." t)
(autoload 'fgrep "igrep" "*Run `fgrep`..." t)
(autoload 'agrep "igrep" "*Run `agrep`..." t)

(autoload 'grep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'egrep-find "igrep" "*Run `egrep` via `find`..." t)
(autoload 'fgrep-find "igrep" "*Run `fgrep` via `find`..." t)
(autoload 'agrep-find "igrep" "*Run `agrep` via `find`..." t)

(defun other-window-backward ()
	"Select the previous window"
	(interactive)
	(other-window -1))

;; Change C-p & C-o to cycle between windows
;;; Change these to F9 & F10 at some point
(global-unset-key "\C-p")
(global-unset-key "\C-o")

(global-set-key "\C-p" 'other-window)
(global-set-key "\C-o" 'other-window-backward)


;; Split the screen to my personal taste
(defun my-split-window ()
	(interactive)
	(split-window-vertically)
	(split-window-horizontally)
	(enlarge-window 10))

(defun my-split-window-startup ()
	(my-split-window)
	(other-window 2)
	(start-shell)
	(switch-to-buffer "*shell*")
	(other-window 1))

(my-split-window-startup)

;; Create a hot key to switch back, if necessary
(global-unset-key "\C-a")
(global-set-key "\C-as" 'my-split-window)

(global-set-key "\C-ar" 'align-regexp)
(global-set-key "\C-q" 'set-mark-command)

;; For some reason, can't get regexp search to work, so mapping it to C-S-s
(global-set-key "\C-S" 'isearch-forward-regexp)


;; Disable Ctrl-Z (background this process in unix)
(global-unset-key "\C-z")

;; Instead, let \C-z kill emacs
(global-set-key "\C-z" 'kill-emacs)

;; Copy text on insert
(global-set-key [insertchar] 'ignore)

(global-set-key [insert] 'ignore)
;; Disable the insert key.. 
(global-set-key [(insert)] 'ignore)

;; Create a hot key for replace-string
(global-set-key "\C-xr" 'replace-string)

;; Set C-xg to replace-regexp
(global-set-key "\C-xg" 'replace-regexp)

;; For Cygwin : Let C-x C-c save buffers and kill emacs 
;; Don't know why cygwin interprets C-x C-c as C-x C-g
(global-set-key "\C-x \C-g" 'save-buffers-kill-emacs)


(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)


;; Turn off annoying "Kill active processes?" question
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
	"Prevent annoying \"Active processes exist\" query when you quit Emacs."
	(flet ((process-list ())) ad-do-it))


;; adding autotest integration
(require 'toggle )
(require 'autotest)

;; Indent a region
(global-set-key "\C-t" 'indent-region)


(load "~/.emacs.d/git-status-message.el")

;; I think I got this from Yegge..
(defun rename-file-and-buffer (new-name)
	"Renames both current buffer and file it's visiting to NEW-NAME." 
	(interactive "sNew name: ")
	
	(let ((name (buffer-name))
				(filename (buffer-file-name)))
		(if (not filename)
				(message "Buffer '%s' is not visiting a file!" name)
			(if (get-buffer new-name)
					(message "A buffer named '%s' already exists!" new-name)
				(progn  (rename-file name new-name 1)
								(rename-buffer new-name)
								(set-visited-file-name new-name)
								(set-buffer-modified-p nil))))))

;; Rename files
(global-set-key "\C-cr" 'rename-file-and-buffer)


;; Scroll one line at a time
(setq scroll-step 1)

;; Putty
(define-key global-map [select] 'end-of-line)
