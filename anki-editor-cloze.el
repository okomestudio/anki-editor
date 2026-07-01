;;; anki-editor-cloze.el --- Minor mode for Cloze deletion  -*- lexical-binding: t; -*-

;;; Code:

(defface anki-editor-cloze-payload
  '((t :foreground "#696969" ;; :underline (:style dashes :color "#00bfff")
       ))
  "Face used for Anki Cloze payload.")

(defvar anki-editor-cloze-underline-colors '("#00bfff" "red" "blue" "green"))

(defface anki-editor-cloze-label
  '((t :foreground "#00bfff" :height 0.75))
  "Face used for Anki Cloze payload.")

(defface anki-editor-cloze-closing
  '((t :foreground "#00bfff" ))
  "Face used for Anki Cloze payload.")

(defconst anki-editor-cloze-opening-regexp "\\({{\\)\\(c[0-9]+\\)\\(::\\)"
  "Regexp matching the start of a cloze deletion tag.")

(defconst anki-editor-cloze-closing-regexp "}}"
  "Regexp matching the end of a cloze deletion tag.")

(defconst anki-editor-cloze-newline-regexp "\n"
  "Regexp matching a newline character.")

(defconst anki-editor-cloze-tag-regexp
  (string-join `(,anki-editor-cloze-closing-regexp
                 ,anki-editor-cloze-opening-regexp)
               "\\|")
  "Combined regexp to locate any cloze structural token.")

(defconst anki-editor-cloze-tag-multiline-regexp
  (string-join `(,anki-editor-cloze-tag-regexp
                 ,anki-editor-cloze-newline-regexp)
               "\\|")
  "Combined regexp to locate any cloze structural token or a newline.")

(defconst anki-editor-cloze-search-limit 3000
  "Character count to limit search.")

(defun anki-editor-cloze-start-position ()
  "Return the starting position of the cloze block at point.
Returns nil if the point is outside a cloze block."
  (save-excursion
    (let ((limit (max (point-min) (- (point) anki-editor-cloze-search-limit)))
          (closed-tags-count 0)
          (start-pos nil))
      (while (and (not start-pos)
                  (re-search-backward anki-editor-cloze-tag-regexp limit t))
        (if (string-match-p anki-editor-cloze-closing-regexp (match-string 0))
            (setq closed-tags-count (1+ closed-tags-count))
          (if (> closed-tags-count 0)
              (setq closed-tags-count (1- closed-tags-count))
            (setq start-pos (match-beginning 0)))))
      start-pos)))

(defun anki-editor-cloze-inside-p ()
  "Return non-nil if point is currently inside a cloze deletion block."
  (not (null (anki-editor-cloze-start-position))))

(defun anki-editor-cloze-end-position ()
  "Return the buffer position of the absolute end of the current cloze block.
Returns nil if the block is unbalanced or unclosed before the buffer limit."
  (let ((start (anki-editor-cloze-start-position)))
    (when start
      (save-excursion
        (goto-char start)
        (when (looking-at anki-editor-cloze-opening-regexp)
          (goto-char (match-end 0)))
        (let ((depth 1)
              (limit (min (point-max) (+ (point) anki-editor-cloze-search-limit))))
          (while (and (> depth 0)
                      (re-search-forward anki-editor-cloze-tag-regexp limit t))
            (if (string-match-p anki-editor-cloze-closing-regexp (match-string 0))
                (setq depth (1- depth)) ; stepped out of a layer
              (setq depth (1+ depth)))) ; stepped into a nested layer
          (when (= depth 0)
            (point)))))))

(defun anki-editor-cloze-extend-region ()
  "Extend font-lock boundaries if they intersect a cloze block.
Add this to the hook `font-lock-extend-region-functions'."
  (let ((changed nil))
    ;; If `font-lock-beg' is inside a multiline block, move it back:
    (save-excursion
      (goto-char font-lock-beg)
      (when (and (not (bobp)) (anki-editor-cloze-inside-p))
        (setq font-lock-beg (anki-editor-cloze-start-position)
              changed t)))
    ;; If `font-lock-end' cuts off a block, push it forward:
    (save-excursion
      (goto-char font-lock-end)
      (when (and (not (eobp)) (anki-editor-cloze-inside-p))
        (setq font-lock-end (anki-editor-cloze-end-position)
              changed t)))
    changed))

(defun anki-editor-cloze-find-balanced-end (limit)
  "Scan forward from point to find the matching '}}', accounting for nesting.
Returns the buffer position of the final '}}', or nil if unbalanced before LIMIT."
  (let ((depth 1) multiline found)
    (catch :exit
      (while-let
          ((matched
            (and (> depth 0)
                 (re-search-forward anki-editor-cloze-tag-multiline-regexp limit t)
                 (match-string 0))))
        (cond ((string-match-p anki-editor-cloze-opening-regexp matched)
               (setq depth (1+ depth)))
              ((string-match-p anki-editor-cloze-closing-regexp matched)
               (setq depth (1- depth)))
              ((string-match-p anki-editor-cloze-newline-regexp matched)
               (setq multiline t))
              (t (error "Unexpected situation!")))
        (when (= depth 0)
          (setq found (point))
          (throw :exit (cons found multiline)))))))

(defvar anki-editor-nested--last nil)
(make-local-variable 'anki-editor-nested--last)

(defun anki-editor-cloze-nested-matcher (limit)
  "Font-lock matcher that handles nested cloze syntax safely."
  (when (re-search-forward anki-editor-cloze-opening-regexp limit t)
    (let* ((opening (cons (match-beginning 1) (match-end 1)))
           (label (cons (match-beginning 2) (match-end 2)))
           (label-num (string-to-number
                       (substring (buffer-substring-no-properties
                                   (car label) (cdr label))
                                  1)))
           (colons (cons (match-beginning 3) (match-end 3)))
           (payload-beg (point))) ; right after double-colon
      (save-excursion
        (when-let* ((rv (anki-editor-cloze-find-balanced-end limit)))
          (let* ((pt (car rv))
                 (ml (cdr rv))
                 (payload (cons payload-beg (- pt 2)))
                 (closing (cons (- pt 2) pt))
                 (props-invisible '( face font-lock-comment-face
                                     invisible anki-editor-cloze-hide
                                     rear-nonsticky (invisible) ))
                 (props-opening
                  (if ml
                      '( face font-lock-comment-face
                         display "⦃"
                         rear-nonsticky (display) )
                    props-invisible))
                 (props-label
                  `( face anki-editor-cloze-label
                     display ,(if ml nil '(raise 0.45))
                     rear-nonsticky (display) ))
                 (props-colon
                  (if ml
                      props-invisible
                    props-invisible))
                 (props-payload
                  `( face ,(if ml
                               nil
                             `( :inherit anki-editor-cloze-payload
                                :underline
                                ( :style dashes
                                  :color ,(let* ((i (mod (1- label-num)
                                                         (length anki-editor-cloze-underline-colors))))
                                            (nth i anki-editor-cloze-underline-colors)) ) ))
                     cursor-face
                     ( :underline
                       ( :color ,(let* ((i (mod (1- label-num)
                                                (length anki-editor-cloze-underline-colors))))
                                   (nth i anki-editor-cloze-underline-colors)) ) )))
                 (props-closing
                  (if ml
                      '( face font-lock-comment-face
                         display "⦄"
                         rear-nonsticky (display) )
                    props-invisible)))
            (add-text-properties (car opening) (cdr opening) props-opening)
            (add-text-properties (car label) (cdr label) props-label)
            (add-text-properties (car colons) (cdr colons) props-colon)
            (add-text-properties (car payload) (cdr payload) props-payload)
            (add-text-properties (car closing) (cdr closing) props-closing)
            (setq-local anki-editor-nested--last (cons (car opening) pt))
            t))))))

;;;###autoload
(define-minor-mode anki-editor-cloze-mode
  "Minor mode to visually conceal cloze deletion."
  :init-value nil
  :lighter " Cloze"
  :group 'anki-editor
  (if anki-editor-cloze-mode
      (progn
        ;; (cursor-sensor-mode 1)
        (cursor-face-highlight-mode 1)
        ;; (cursor-face-highlight-mode 1)
        (add-hook 'font-lock-extend-region-functions #'anki-editor-cloze-extend-region nil t)
        (add-to-invisibility-spec 'anki-editor-cloze-hide)
        (font-lock-add-keywords nil '((anki-editor-cloze-nested-matcher)))
        (font-lock-flush))
    (remove-from-invisibility-spec 'anki-editor-cloze-hide)
    (font-lock-remove-keywords nil '((anki-editor-cloze-nested-matcher)))
    (remove-hook 'font-lock-extend-region-functions #'anki-editor-cloze-extend-region t)
    (font-lock-flush)))

(provide 'anki-editor-cloze)

;;; anki-editor-cloze.el ends here
