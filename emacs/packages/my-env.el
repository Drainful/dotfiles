;; My environment variables

(require 'helper-functions)

(defmacro set-environment-variable (variable value)
  `(setenv ,(symbol-name variable) ,value))

(define-multi-macro setenv++ set-environment-variable 2)

(provide 'my-env)
