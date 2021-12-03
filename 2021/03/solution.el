(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

(setq-local input (read-lines "input.txt"))
;; (setq-local input (read-lines "example.txt"))

(setq-local most-common (-map (-compose
                               (lambda (col-sum) (> col-sum 0))
                               '-sum)
                              (-unzip (-map (lambda (string)
                                              (-map (lambda (el) (- el 48.5))
                                                    (append string nil))) input))))

(defun to-lsb (acc is-1)
  (logior acc (if is-1 1 0)))

(setq-local gamma (-reduce-from (lambda (acc bit)
                                  (to-lsb (lsh acc 1) bit))
                                (to-lsb 0 (car most-common))
                                (cdr most-common)))

(setq-local least-common (-map 'null most-common))

(setq-local epsilon (-reduce-from (lambda (acc bit)
                                    (to-lsb (lsh acc 1) bit))
                                  (to-lsb 0 (car least-common))
                                  (cdr least-common)))

(message "Part1: %d" (* epsilon gamma))
