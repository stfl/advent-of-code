(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

;; (setq-local input (read-lines "example.txt"))
(setq-local input (read-lines "input.txt"))

(setq-local input
            (-tree-map (-rpartial #'- 48)
                       (-map (-compose (-rpartial #'append nil))
                             input)))

(setq-local width (length (car input)))

(defun bound-input-y (input)
  (let* ((width (length (car input)))
         (bound-list (make-list width 10)))
    (-snoc (cons bound-list input) bound-list)))

(defun low-point? (points)
  (let ((p (nth 2 points))
        (min-others (-min (-remove-at 2 points))))
    (when (< p min-others) p)))

(defun find-lows (input)
  (-map (lambda (rows)
          (-map
           (-compose #'low-point?
                     #'-flatten)
           (-zip (nth 0 rows)
                 (-partition-in-steps 3 1 (-snoc (cons 10 (nth 1 rows)) 10))
                 (nth 2 rows))))
        (-partition-in-steps 3 1 (bound-input-y input))))

(defun risk-score (input)
  (->> (find-lows input)
       (-flatten)
       (-map #'1+)
       (-sum)))

(message "Part1: %d" (risk-score input))
