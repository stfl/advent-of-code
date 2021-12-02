(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

;; (setq-local input (-map #'split-string (read-lines "example.txt")))
(setq-local input (-map #'split-string (read-lines "input.txt")))

;; Part 1
(defun movements (list)
  (-map (-compose 'string-to-number 'car 'cdr) list))

(defun sum-movements (list)
  (-sum (movements list)))

(setq-local grouped (-group-by #'car input))

(setq-local depth (- (sum-movements (cdr (assoc "down" grouped)))
                     (sum-movements (cdr (assoc "up" grouped)))))

(setq-local forward (sum-movements (cdr (assoc "forward" grouped))))

(setq-local sum-forward-depth (* depth forward))

(message "Part 1: %d" sum-forward-depth)

;; Part 2

(defun gen-dir-list (el)
  (let ((direction (car el))
        (num (string-to-number (car (cdr el)))))
    ; (forward direction)
    (cond ((string= direction "down") `(nil ,num))
          ((string= direction "up") `(nil ,(- num)))
          ((string= direction "forward") `(,num nil)))))

(setq zipped (-unzip (-map 'gen-dir-list input))
      forward-list (car zipped)
      depth-list (car (cdr zipped)))

(setq aim-acc (-running-sum (-replace nil 0 depth-list)))

(setq depth-aimed
      (-sum (-map (lambda (el)
                    (if (-cons-pair? el) (* (car el) (cdr el)) 0))
                  (-zip aim-acc forward-list))))


(setq prod-aimed (* depth-aimed (-sum (-replace nil 0 forward-list))))

(message "Part 2: %d" prod-aimed)

;; using -reduce-from

(-product
 (cdr
  (-reduce-from
   (lambda (acc el)
     (let ((direction (car el))
           (num (string-to-number (elt el 1))))
       (cond ((string= direction "down") (cl-incf (elt acc 0) num))
             ((string= direction "up") (cl-decf (elt acc 0) num))
             ((string= direction "forward")
              (cl-incf (elt acc 1) (* num (elt acc 0)))
              (cl-incf (elt acc 2) num))
             )
       acc))
   '(0 0 0) ;; (aim depth forward)
   input)))
