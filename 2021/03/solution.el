(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

(setq-local input (read-lines "input.txt"))
;; (setq-local input (read-lines "example.txt"))

(defun parse-to-bit (input)
  (-map (lambda (string)
          (-map (lambda (el) (- el 48.5))
                (append string nil))) input))

(defun gen-most-common (input)
  (-map (-compose
         (lambda (col-sum) (>= col-sum 0))
         '-sum
         (lambda (col)  ;; we can't do -sum of a cons-pair
           (if (-cons-pair? col)
               (-cons-to-list col)
             col)))
        (-unzip input)))

(defun gen-least-common (most-common) (-map 'null most-common))

(setq-local most-common (gen-most-common (parse-to-bit input)))

(defun to-lsb (acc is-1)
  (logior acc (if is-1 1 0)))

(defun bit-list-to-int (bit-list)
  (-reduce-from (lambda (acc bit)
                  (to-lsb (lsh acc 1) bit))
                (to-lsb 0 (car bit-list))
                (cdr bit-list)))

(setq-local gamma (bit-list-to-int most-common))
(setq-local epsilon (bit-list-to-int (gen-least-common most-common)))

(message "Part1: %d" (* epsilon gamma))

;; Part 2

(defun filter-by-bit-index (rows most-common idx)
  (-filter (lambda (row)
             (if (nth idx most-common)
                 (> (nth idx row) 0)
               (< (nth idx row) 0)))
           rows))

(defun find-rating (bit-input find-most)
  (let ((idx 0)
        (remaining bit-input))
    (while (> (length remaining) 1)
      (let ((most-common (gen-most-common remaining)))
        (setq remaining (filter-by-bit-index remaining
                                             (if find-most
                                                 most-common
                                               (gen-least-common most-common))
                                             idx)))
      (cl-incf idx))
    remaining))

(setq-local oxygen
            (bit-list-to-int
             (-map
              (lambda (col-sum) (>= col-sum 0))
              (car (find-rating (parse-to-bit input) t)))))

(setq-local co2
            (bit-list-to-int
             (-map
              (lambda (col-sum) (>= col-sum 0))
              (car (find-rating (parse-to-bit input) nil)))))

(message "Part2: %d" (* oxygen co2))
