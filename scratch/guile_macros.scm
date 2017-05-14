(define-syntax my-when
  (syntax-rules ()
    ((my-when condition exp ...)
     (if condition
         (begin exp ...)))))

(my-when #t
         (write "hi"))
