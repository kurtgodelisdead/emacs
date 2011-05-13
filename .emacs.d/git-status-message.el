;; appt mode apparently lets you get reminders.. Lets try this
(display-time)
(require 'appt)
(appt-activate 1)

(defun nilp (a)
  (eq nil a))
  
(defun git-command (dir opts)
  (shell-command-to-string (format "cd %s; git ls-files %s" dir opts)))

(defun git-uncommitted (dir)
  (git-command dir "--modified"))

(defun git-untracked (dir)
  (git-command dir "--other --exclude-standard"))
    
(defun find-next (source substr start)
  (let ((str source))
    (let ((len (length str)))    
      (let ((l (- len 1)))
        (search substr str :start2 start :end2 l)))))

(defun count-substring (str substr)
  (let ((start 0) (count 0) (next 0))
    (while next
      (setq count (1+ count))
      ;; (message (format "Count is now %d" count))
      (setq next (find-next str substr start))
      (if next
          (setq start (+ next 1))))
    count))

(defun git-status-message ()
  (interactive)
  (let ((cgrr (current-git-repo-root)))
    (let ((uncommitted (count-substring (git-uncommitted cgrr) "\n"))
          (untracked   (count-substring (git-untracked   cgrr) "\n")))
      ;; You are in the directory %s. 

      (format "You have %d uncommitted files and %d untracked files. (%s)" 
              uncommitted untracked cgrr))))

;; (git-status-message)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun current-directory-path ()
  (interactive)
  (file-name-directory (or load-file-name buffer-file-name)))

(defun current-git-repo-root () 
  (interactive)
  (let ((cdp (current-directory-path)))
    (chomp (concat cdp (shell-command-to-string (format "cd %s; git rev-parse --show-cdup" cdp))))))


;; time-list = (seconds minutes hour day month year dow dst zone)
(defun one-minute-from-now ()
  (let ((time-list (decode-time (current-time))))
    (let ((seconds        (car time-list))
          (minutes        (car (cdr time-list)))
          (rest-time-list (cdr (cdr time-list))))
      (apply 'encode-time
             (cons seconds (cons (+ minutes 1) rest-time-list))))))

(defun git-commit-alert ()
  (interactive)
  (appt-add (format-time-string "%H:%M%P" (one-minute-from-now)) (git-status-message)))

;; run-at-time + appt mode + git mode should give me
;; the ability to be reminded to commit
(run-at-time "0 seconds" (* 60 5) 'git-commit-alert)


