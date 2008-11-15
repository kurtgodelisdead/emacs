
(load "ruby/ruby-mode")

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.treetop$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "ruby/inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "ruby/inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(add-hook 'ruby-mode-hook
					'(lambda ()
						 (inf-ruby-keys)))
;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "ruby/rubydb3x" "Ruby debugger" t)
;; uncomment the next line if you want syntax highlighting
;; (add-hook 'ruby-mode-hook 'turn-on-font-lock)

(defun ruby-eval-buffer ()
	"Evaluate the buffer with ruby."
	(interactive)
	(shell-command-on-region (point-min) (point-max) "ruby"))

(defun my-ruby-mode-hook ()
	(define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(global-set-key "\C-f" 'comment-region)


