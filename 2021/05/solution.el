;; -*- max-specpdl-size: 200000; max-lisp-eval-depth: 20000; -*-

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

;; (setq lines (read-lines "example.txt"))
(setq lines (read-lines "input.txt"))

(setq-local input
            (-tree-map #'string-to-number
                       (-tree-map (-rpartial #'split-string ",")
                                  (-map (-rpartial #'split-string " -> ")
                                        lines))))

(defun get-xs (line)
  (list (car (car line))
        (car (nth 1 line))))

(defun get-ys (line)
  (list (nth 1 (nth 0 line))
        (nth 1 (nth 1 line))))

(defun straight-line? (line)
  (-let (((x1 x2) (get-xs line))
         ((y1 y2) (get-ys line)))
    (or (= x1 x2) (= y1 y2))))

(defun straight-line-steps (line)
  (-let (((xfrom yfrom xto yto) (-flatten line)))
    (when (cond ((< xfrom xto) (cl-incf (elt (elt line 0) 0)))
                ((< yfrom yto) (cl-incf (elt (elt line 0) 1)))
                ((> xfrom xto) (cl-decf (elt (elt line 0) 0)))
                ((> yfrom yto) (cl-decf (elt (elt line 0) 1)))
                (t nil)))
    (cons `(,xfrom ,yfrom) (straight-line-steps line))))

(defun count-overlapping-straigth (lines)
  (->> lines
       (-map 'straight-line-steps)
       (-flatten-n 1)
       (-group-by 'identity)
       (-map (-compose 'length 'cdr))
       (-filter (-rpartial '> 1))
       (length)))


(message "Part1: %d" (count-overlapping-straigth (-filter 'straight-line? input)))
