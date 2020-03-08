;;; nextjournal stuff
(defun nextjournal/add-nextjournal-cljs-repl-type ()
  (when (not (seq-some (lambda (entry) (eq 'nextjournal (car entry))) cider-cljs-repl-types))
    (add-to-list 'cider-cljs-repl-types '(nextjournal "(do (require 'com.nextjournal.journal.repl) (com.nextjournal.journal.repl/wait-for-figwheel) (com.nextjournal.journal.repl/editor-repl))" nil))))

(with-eval-after-load 'cider
  (progn
    (add-to-list 'clojure-align-binding-forms "p/let")

    ;; automatically reuse cider repl buffers without prompting
    (defadvice cider--choose-reusable-repl-buffer (around auto-confirm compile activate)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest args) t))
                ((symbol-function 'completing-read) (lambda (prompt collection &rest args) (car collection))))
        ad-do-it))

    (nextjournal/add-nextjournal-cljs-repl-type)))

(provide 'nextjournal-init)
