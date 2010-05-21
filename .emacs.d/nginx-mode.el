

(defvar nginx-mode-syntax-table
  (let ((nginx-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" nginx-mode-syntax-table)
		(modify-syntax-entry ?# "<" nginx-mode-syntax-table)
		(modify-syntax-entry ?\n ">" nginx-mode-syntax-table)
		nginx-mode-syntax-table)
	"Syntax mode for Nginx")


(defconst nginx-font-lock-keywords-1
  (list
   '("\\<\\(events\\|server\\|upstream\\|if\\|location\\|http\\)\\>" . font-lock-keyword-face))
  "Minimal highlighting expressions for Nginx mode")

(defvar nginx-font-lock-keywords nginx-font-lock-keywords-1
	"Default highlighting expressions for Nginx mode")


(defun nginx-indent-line ()
  "Indent current line as Nginx code"
  (interactive)
  (beginning-of-line)

	(if (bobp) ; Check for rule 1: The beginning of the buffer is never indented
			(indent-line-to 0)
		
		(let ((not-indented t) cur-indent)			
			(if (looking-at "^[ \t]*}") ; Check for rule 2: If we're closing a block
					(progn
						(save-excursion
							(forward-line -1)
							(setq cur-indent (- (current-indentation) default-tab-width)))
						(if (< cur-indent 0)
								(setq cur-indent 0)))
				
        (save-excursion 
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}") ; Check for rule 3
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
							
																				; Check for rule 4
              (if (looking-at "^.*{$")
                  (progn
                    (setq cur-indent (+ (current-indentation) default-tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
			
			(if cur-indent
          (indent-line-to cur-indent)
				(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation


(defun nginx-mode ()
  "Major mode for editing Nginx config files"
  (interactive)
  (kill-all-local-variables)
	(set-syntax-table nginx-mode-syntax-table)

	(set (make-local-variable 'font-lock-defaults) '(nginx-font-lock-keywords))
	(set (make-local-variable 'indent-line-function) 'nginx-indent-line)
	(setq major-mode 'nginx-mode)
	(setq mode-name "Nginx")
	(run-hooks 'nginx-mode-hook))
	

(provide 'nginx-mode)

