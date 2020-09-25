(define (er-macro-transformer proc)
  (lambda (stx)


    (proc UNWRAPPED (lambda (x)
                      (syntax->datum #'a x))
          free-identifier=?)


    )


  )
