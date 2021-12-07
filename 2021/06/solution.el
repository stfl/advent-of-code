(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

;; (setq-local input (-map #'string-to-number (split-string (car (read-lines "example.txt")) ",")))
(setq-local input (-map #'string-to-number (split-string (car (read-lines "input.txt")) ",")))

(defun give-birth (input)
  (append input (-repeat (-count (-rpartial #'< 0) input) 8)))

(defun next-generation (input)
  (-map-when (-rpartial #'< 0)
             (lambda (_) 6)
             (give-birth (-map #'1- input))))

(defun run-generations (input num)
  (let ((generation input))
    (dotimes (_ num)
      (setq generation (next-generation generation)))
    (length generation)))

;; this takes long
;; (message "Part1: %d" (run-generations input 80))

;; this takes FOREVER
;; (message "Part2: %d" (run-generations input 256))

;; Another try

(defun calc-counts (input)
  (-map (lambda (i)
          (length (-elem-indices i input )))
        (number-sequence 0 8)))

(defun next-counts-gen (counts)
  (-update-at 6 (-partial #'+ (car counts))
              (-rotate -1 counts)))

(defun run-count-gens (input num)
  (let ((generation (calc-counts input)))
    (dotimes (_ num)
      (setq generation (next-counts-gen generation)))
    (-sum generation)))

;; This is fast

(message "Part1: %d" (run-count-gens input 80))
(message "Part2: %d" (run-count-gens input 256))
