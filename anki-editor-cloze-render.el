;;; anki-editor-cloze-render.el --- Minor mode for Cloze deletion  -*- lexical-binding: t; -*-

;;; Code:

;; TODO(2026-06-29): Since Anki verion 2.1.56, nested cloze deletions are
;; supported. Consider support here.

(defface anki-editor-cloze-payload
  '((t :foreground "#696969" :underline (:style dashes :color "#00bfff" :position 4)))
  "Face used for Anki Cloze payload.")

(defface anki-editor-cloze-label
  '((t :foreground "#00bfff" :height 0.75))
  "Face used for Anki Cloze payload.")

(defface anki-editor-cloze-closing
  '((t :foreground "#00bfff" ))
  "Face used for Anki Cloze payload.")

(defun anki-editor-find-balanced-cloze-end (limit)
  "Scan forward from point to find the matching '}}', accounting for nesting.
Returns the buffer position of the final '}}', or nil if unbalanced before LIMIT."
  (let ((depth 1) multiline found)
    (catch :exit
      (while-let ((matched (and (> depth 0)
                                (re-search-forward "{{\\|}}\\|\n" limit t)
                                (match-string 0))))
        (cond ((string= matched "{{") (setq depth (1+ depth)))
              ((string= matched "}}") (setq depth (1- depth)))
              ((string= matched "\n") (setq multiline t))
              (t (error "Unexpected situation!")))
        (when (= depth 0)
          (setq found (point))
          (throw :exit (cons found multiline)))))))

(defvar anki-editor-nested--last nil)
(make-local-variable 'anki-editor-nested--last)

(defun anki-editor-nested-cloze-matcher (limit)
  "Font-lock matcher that handles nested cloze syntax safely."
  (when (re-search-forward "\\({{\\)\\(c[0-9]+\\)\\(::\\)" limit t)
    (let ((opening (cons (match-beginning 1) (match-end 1)))
          (label (cons (match-beginning 2) (match-end 2)))
          (colons (cons (match-beginning 3) (match-end 3)))
          (payload-beg (point))) ; right after double-colon
      (save-excursion
        (when-let* ((rv (anki-editor-find-balanced-cloze-end limit)))
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
                             'anki-editor-cloze-payload) ))
                 (props-closing
                  (if ml
                      '( face font-lock-comment-face
                         display "⦄"
                         rear-nonsticky (display) )
                    props-invisible
                    ;; '( face anki-editor-cloze-closing
                    ;;    display ("┊" :width 0.2 )
                    ;;    rear-nonsticky (display) )
                    )))
            (add-text-properties (car opening) (cdr opening) props-opening)
            (add-text-properties (car label) (cdr label) props-label)
            (add-text-properties (car colons) (cdr colons) props-colon)
            (add-text-properties (car payload) (cdr payload) props-payload)
            (add-text-properties (car closing) (cdr closing) props-closing)
            (setq-local anki-editor-nested--last (cons (car opening) pt))
            t))))))

;;;###autoload
(define-minor-mode anki-editor-cloze-render-mode
  "Minor mode to visually conceal cloze deletion wrappers while keeping contents editable."
  :init-value nil
  :lighter " Cloze"
  :group 'org-appearance
  (if anki-editor-cloze-render-mode
      (progn
        (setq-local font-lock-multiline t)
        (add-to-invisibility-spec 'anki-editor-cloze-hide)
        (add-to-invisibility-spec '(anki-editor-cloze-hide-ellipsis . t))
        (font-lock-add-keywords nil '((anki-editor-nested-cloze-matcher)))
        (font-lock-flush))
    (remove-from-invisibility-spec '(anki-editor-cloze-hide-ellipsis . t))
    (remove-from-invisibility-spec 'anki-editor-cloze-hide)
    (kill-local-variable 'font-lock-multiline)
    (font-lock-remove-keywords nil '((anki-editor-nested-cloze-matcher)))
    (font-lock-flush)))

(provide 'anki-editor-cloze-render)

;;; anki-editor-cloze-render.el ends here
