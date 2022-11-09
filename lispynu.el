(require 'lispy)
;; maybe lispy is not necessary

(defun lispynu-sort ()
  (interactive)
  (let ((list (read (thing-at-point 'sexp))))
    (if (listp list)
        (progn
          (delete-region (beginning-of-thing 'sexp)
                         (end-of-thing 'sexp))
          (when (eq (car list) 'quote)
            (setf list (cadr list))
            (insert "'"))
          (let ((symbol-list-p (and (listp list) (-all? 'symbolp list)))
                (string-list-p (and (listp list) (-all? 'stringp list))))
            (if (or symbol-list-p string-list-p)
                (progn
                  (prin1 (sort list
                               (if symbol-list-p
                                   (lambda (x y) (string< (symbol-name x)
                                                     (symbol-name y)))
                                 (if string-list-p
                                     #'string<
                                   (error "You shouldn't get here."))))
                         (current-buffer))
                  (lispy-multiline))
              (message "Only works on lists of symbols or strings."))))
      (message "Only works on lists."))))

(lispy-define-key lispy-mode-map (kbd "Q") #'lispynu-sort)

(defmacro lispy--show-inline (expr)
  "Display the result of EXPR inline. This is implemented as a macro as EXPR must be evaluated after the program traverses to the appropriate location."
  `(save-excursion
     (lispy--back-to-paren)
     (unless (and (prog1 (lispy--cleanup-overlay)
                    (when (window-minibuffer-p)
                      (window-resize (selected-window) -1)))
                  (= lispy-hint-pos (point)))
       (cond ((memq major-mode lispy-elisp-modes)
              (let ((sym (intern-soft (lispy--current-function))))
                (cond ((fboundp sym)
                       (setq lispy-hint-pos (point))
                       (lispy--show (eval ,expr))
                       (other-window 1)))))

             ((eq major-mode 'lisp-mode)
              (require 'le-lisp)
              (setq lispy-hint-pos (point))
              (lispy--show (eval ,expr))
              (other-window 1))

             (t (error "%s isn't supported currently" major-mode))))))
(defun lispy-eval-inline ()
  "Evaluate the first sexp around point inline."
  (interactive)
  (lispy--show-inline (lispy--eval nil)))

(defun lispy--find-source ()
  "Finds source of the symbol under point."
  (let ((this-buffer (current-buffer)))
    (save-window-excursion
      (save-excursion
        (if (eq major-mode 'lisp-mode)
            (sly-edit-definition-no-confirm)
          (xref-find-definitions (thing-at-point 'symbol)))
        (lispy-move-beginning-of-line)
        (prog1
            (if (eq major-mode 'c-mode)
                (thing-at-point 'paragraph)
              (thing-at-point 'sexp))
          (unless (eq this-buffer (current-buffer))
            (kill-this-buffer)))))))

(defun lispy-source-inline ()
  "Displays source of the symbol under point inline."
  (interactive)
  (lispy--show-inline
   (progn
     (forward-char)
     (lispy--find-source))))

(define-prefix-command 'lispy-command-map)
(global-set-key (kbd "C-c C-l") 'lispy-command-map)
(general-def
  :keymaps 'lispy-mode-map
  ;; "C-3" 'lispy-source-inline
  "M-3" 'xref-find-definitions)
(general-def
  :keymaps 'emacs-lisp-mode-map
  "C-(" 'xref-find-definitions-other-window)
(general-def
  :prefix "C-c C-l"
  :keymaps 'lispy-mode-map
  "e" 'lispy-eval)
;; (define-key emacs-lisp-mode-map (kbd "C-(") (lambda ()
;;                                               (interactive)
;;                                               (consult-line (thing-at-point 'symbol))))
;; (define-key lpy-mode-map (kbd "C-c C-l e") 'lispy-eval)
;; (define-key lpy-mode-map (kbd "C-c C-l C-e") 'lispy-eval)
(defun %defun-or-macro-p (sexp)
  "Checks if SEXP is defun or macro."
  (cl-member (car sexp)
             '(defun defmacro cl-defun cl-defmacro defgeneric defvar* defun* defmacro*)
             :test #'equal))

(defun %get-operator-and-operands (sexp)
  "Get operator and operands in SEXP. In a defun or macro like (defun square (x) (* x x)), square is the operator and '(x) is the operands. In a function call like (+ 1 2), + is the operator and '(1 2) is the operands."
  (if (%defun-or-macro-p sexp)
      (values (second sexp) (third sexp))
    (values (first sexp) (rest sexp))))

(defun %parse-args (operands)
  "Parse arguments for OPERANDS."
  (remove-if #'listp
             (cl-loop for o in operands
                      unless (member o '(&optional))
                      collect o)))

(defvar l1-examples
  '((expr . (expt 2 3))
    (xs . '(a b c d))
    (sexp . '(a b c d))
    (prompt . "lorem ipsum: ")
    (string . "\"lorem ipsum\"")
    (fn . '+)
    (lang . 'emacs-lisp)
    ("str" . "\"lorem ipsum\"")
    ("s" . "\"lorem ipsum\"")
    ("trigger-string" . "\"lorem ipsum\"")))

(defvar l2-examples
  (-union l1-examples
          '(("n" . "3")
            ("a" . "0")
            ("b" . "1")
            ("i" . "3")
            (list . '(a b c d))
            (elem . 'b)
            ("x" . 4)
            ("y" . 5)
            ("value" . "'f")
            (alist . '((a . b) (c . d)))
            ("key" . 'a)
            ("unit" . "h"))))

(defun %subst-examples (operands examples)
  "Substitute OPERANDS with corresponding EXAMPLES, where EXAMPLES is an association list of key and example. If operand in OPERANDS is equal to key, then the operand is replaced with example."
  (cl-loop for operand in operands
           collect (cl-loop named inner for (key . example) in examples
                            if (and (not (listp operand)) (string-equal operand key))
                            return example
                            finally return operand)))

(defun %end-of-current-list ()
  "Move to end of sentence with right paren. end-of-current-list is not a perfect name."
  (while (save-excursion
           (backward-char)
           (not (string= (thing-at-point 'char) ")")))
    (forward-sentence)))

(defun lispynu-try ()
  "Try to copy the current function or expression"
  (interactive)
  (let ((sexp (read (thing-at-point 'sexp))))
    (cl-multiple-value-bind (operator operands) (%get-operator-and-operands sexp)
	  (%end-of-current-list)
	  (lispy-newline-and-indent-plain)
	  (insert (format "%S"
					  (cons
					   operator
					   (if (%defun-or-macro-p sexp)
						   (%subst-examples (%parse-args operands) l1-examples)
                         (%subst-examples operands l2-examples))))))))

(lispy-define-key lispy-mode-map "T" 'lispynu-try)

(defun lispynu-setf-var-from-minibuffer ()
  ;; a very dangerous function
  (interactive)
  (eval `(setf ,(intern (read-from-minibuffer "setf (?): " "temp"))
               ,(read (current-word)))))

(defun lispynu-isetf ()
  ;; defines a temporary variable from the minibuffer, looks at sexp under cursor
  (interactive)
  (let* ((expr-str (thing-at-point 'sexp))
         (name (read-from-minibuffer (cl-format nil "setf (?): ")))
         (result (eval (read expr-str))))
    (eval
     `(setf ,(intern name) ,result))
    (if (> (length (prin1-to-string result)) 80)
        (message "(v) %s: %s" name expr-str)
      (message "(v) %s: %s => %s" name expr-str result))
    ))

(defun lispynu-idefun ()
  ;; defines a temporary function from the minibuffer, looks at sexp under cursor
  (interactive)
  (let* ((expr-str (thing-at-point 'sexp))
         (name (read-from-minibuffer (cl-format nil "defun (?): ") "test")))
    (eval
     `(defun ,(intern name) ()
        (interactive)
        (message "%s" ,(read expr-str))))
    (message "(f) %s: %s" name expr-str)))


(defun lispynu-isetf-inverse ()
  ;; defines a temporary variable from the minibuffer, looks at sexp under cursor
  (interactive)
  (let* ((name (current-word))
         (expr (read-minibuffer (format "setf %s: " (current-word))))
         (result (eval expr))
         (result-str (prin1-to-string result)))
    (eval
     `(setf ,(intern name) ,result))
    (if (or (> (length result-str) 80)
            (= expr result))
        (message "(v) %s: %s" name expr)
      (message "(v) %s: %s => %s" name expr result))
    ))

(lispy-define-key lispy-mode-map "S" 'lispynu-isetf)
(lispy-define-key lispy-mode-map "D" 'lispynu-idefun)
(lispy-define-key lispy-mode-map "V" 'lispynu-isetf-inverse)
