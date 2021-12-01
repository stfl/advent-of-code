(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

(setq-local input (-map #'string-to-number (read-lines "input.txt")))

(defun count-increased (list)
  (-count (lambda (a) (< (car a) (car (cdr a))))
           (-partition-in-steps 2 1 list)))

(count-increased input)

(count-increased (-map #'-sum (-partition-in-steps 3 1 input))) ; part 2
