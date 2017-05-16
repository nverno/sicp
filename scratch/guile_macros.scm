;; to get nice indentation, need to use
;; (put 'my-when 'scheme-indent-function 1)
;; or the the like -- no declare syntax
(define-syntax my-when
  (syntax-rules ()
    ((my-when condition exp ...)
     (if condition
         (begin exp ...)))))

(my-when #t
  (write "hi"))
