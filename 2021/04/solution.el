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
(setq-local boards (-partition 5 (-map 'split-string (cdr input))))

(defun board-complete? (board)
  (-first (-partial #'-all? 'null)
          (append board (-unzip board))))

(defun mark-boards (boards in)
  (-tree-map (lambda (el) (unless (string= el in) el)) boards))

(defun play-bingo (boards in-list)
  (when (and in-list boards)
    (-let (((completed-board remaining)
            (-separate 'board-complete? (mark-boards boards (car in-list)))))
      (cons completed-board
            (play-bingo remaining (cdr in-list))))))

(defun bingo-score (board in)
  (when board
    (* (string-to-number in)
       (-sum (-map 'string-to-number (-flatten board))))))

(defun bingo-scores (boards in-list)
  (-map (-applify #'bingo-score)
        (-zip-lists (play-bingo boards in-list)
                    in-list)))

(message "Part1: %d" (-first (-not 'null) (bingo-scores boards in-list)))
(message "Part2: %d" (-last (-not 'null) (bingo-scores boards in-list)))
