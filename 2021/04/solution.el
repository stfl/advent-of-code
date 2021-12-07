;; -*- max-lisp-eval-depth: 2000; -*-

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

(defun board-complete? (board)
  (-first (-partial #'-all? 'null)
          (append board (-unzip board))))

(defun mark-until-found (in-list boards)
  (let* ((marked boards)
         (f (catch 'found
              (dolist (in in-list)
                (setq marked (-map (-partial '-replace in 'nil) marked))
                (let ((completed-board (-first 'board-complete? (-partition 5 marked))))
                  (when completed-board
                    (throw 'found `(,in . ,completed-board))))))))
    f))

(defun score (in-list boards)
  (-let (((last-in . board) (mark-until-found in-list boards)))
    (* (string-to-number last-in)
       (-sum (-map 'string-to-number (-flatten board))))))

(message "Part1: %s" (score in-list boards))

;; Part 2

;; without catch throw

(setq boards (-partition 5 boards))

(defun mark-boards (boards in)
  (-tree-map (lambda (el) (unless (string= el in) el)) boards))

(defun play-bingo (boards in-list)
  (when (and in-list boards)
    (-let (((completed-board remaining)
            (-separate 'board-complete? (mark-boards boards (car in-list)))))
      (cons completed-board
            (play-bingo remaining (cdr in-list))))))

(defun bingo-score (in board)
  (when board
    (* (string-to-number in)
       (-sum (-map 'string-to-number (-flatten board))))))

(defun bingo-scores (boards in-list)
  (-map (-applify #'bingo-score)
        (-zip-lists in-list
                    (play-bingo boards in-list))))

;; (message "Part1: %d" (-first (-not 'null) (bingo-scores boards in-list)))
(message "Part2: %d" (-last (-not 'null) (bingo-scores boards in-list)))
