(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

(setq-local input (-map #'split-string (read-lines "input.txt")))

(-group-by #'car input)
