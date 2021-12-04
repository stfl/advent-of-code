(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(require 'dash)

;; (setq-local input (read-lines "example.txt"))
(setq-local input (read-lines "input.txt"))

(setq-local in-list (split-string (car input) ","))

(setq-local boards
            (-map 'split-string
                  (-filter (-not 'string-empty-p)
                           (cdr input))))

(setq-local found nil)

(defun mark-until-found (in-list boards)
  (let* ((marked boards)
         (f (catch 'found
                (dolist (in in-list)
                  (setq marked (-map (-partial '-remove-item in) marked))
                  (let ((complete-index (-find-indices 'not marked)))
                    (when complete-index
                      (throw 'found `(,(car complete-index) . ,in))))))))
    `(,f ,marked)))

(defun score (in-list boards)
            (let* ((res (mark-until-found in-list boards))
                   (board-start (* (/ (car (car res)) 5) 5))
                   (board (-slice (nth 1 res) board-start (+ board-start 5)))
                   (last-in (string-to-number (cdr (car res)))))
              (* last-in (-sum
                          (-map 'string-to-number (-flatten board))))))

(message "Part1: %s" (score in-list boards))


;; (setq-local complete-board
;;             (floor (/
;;                     ()
;;             (car ))
;;             4))
