((nil . ((fill-column . 78)
         (tab-width   .  8)))
 (scheme-mode . ((indent-tabs-mode . nil)
                 (eval . (setq scheme-font-lock-keywords-2
                               (cons
                                '("(\\(define-json-mapping\\)"
                                  .
                                  ((1 font-lock-keyword-face)))
                                scheme-font-lock-keywords-2)))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil))))
