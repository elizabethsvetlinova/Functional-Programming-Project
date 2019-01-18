#lang racket
(define d '("/"
            "/"
            (
             ("/etc" "etc" ())
             ("/opt" "opt"
                     (
                      ("/opt/tectia" "tectia" ())
                      ))
             ("/u" "u"
                   (
                    ("/u/smith" "smith" ())
                     ("/u/walsh" "walsh"
                                 (
                                  ("/u/walsh/test" "test"
                                                   (
                                                    ("/u/walsh/test/file.txt" "file.txt" "some text")
                                                    ))
                                  ))
                      
                     ))

             )))

(define full-path car)
(define name cadr)
(define content caddr)

(define (get-full-path-foreach-content dir)
  (map full-path (content dir)))

(define (get-names-foreach-content dir)
  (map name (content dir)))

(define (make-full-path dir n)
  (cond ((equal? (full-path dir) "/") (string-append (full-path dir) n))
         (else (string-append (full-path dir) "/" n))))

(define (add-content dir n c) ;n-name c-content
  (list (full-path dir)
        (name dir)
        (if (null? (content dir))
            (list (make-full-path dir n)
                     n
                     c)
        (list (content dir)
              (list (make-full-path dir n)
                     n
                     c)))))


(define (is-folder? el)
  (and (not ( null? (full-path el)))
       (not( null? (name el)))
       (not( null? (content el)))
       (and (list? (content el)))))

(define (is-file? el)
  (and (not ( null? (full-path el)))
       (not( null? (name el)))
       (not( null? (content el)))
       (and (string? (content el)))))


(define p "/u/walsh")


(define (split-input input)
  (if (equal? (string-ref input 0) #\/) 
     (cons "/" (string-split	 input "/" #:trim? #t 	#:repeat? #t)	)
      (string-split	 input "/" #:trim? #t  #:repeat? #t)
       ))

;(get-child-by-name d "u")
;'("/u"
  ;"u"
  ;(("/u/smith" "smith" ())
   ;("/u/walsh" "walsh" (("/u/walsh/test" "test" (("/u/walsh/test.txt" "file.txt" "some text")))))))
(define (get-child-by-name dir n)
(if (not(null?
         (filter (lambda (x)(if (equal? (name x) n) x #f)) (content dir)))  )
    (car (filter (lambda (x)(if (equal? (name x) n) x #f)) (content dir)))
    #f))

;(get-childs-by-path d '("u" "walsh"))
;'("/u/walsh" "walsh" (("/u/walsh/test" "test" (("/u/walsh/test" "file.txt" "some text")))))
(define (get-childs-by-path dir list)
  (cond ((null? dir) #f)
         ((null? list) dir)
         ((equal? dir #f) #f)
         ((null? (cdr list)) (get-child-by-name dir (car list))) 
         (else (get-childs-by-path (get-child-by-name dir (car list)) (cdr list)))))


;(get-by-path d "/u/walsh")
;'("/u/walsh" "walsh" (("/u/walsh/test" "test" (("/u/walsh/test" "file.txt" "some text")))))
(define (get-by-path dir p)
  (cond ((equal? (name dir)(car (split-input p)))
         (get-childs-by-path dir  (cdr (split-input p))))
        (else #f)))

(define (get-by-listed-path dir list)
  (cond ((equal? (name dir)(car list))
         (get-childs-by-path dir  (cdr list)))
        (else #f)))

                     
  
(define c (cadr (content d)))

(define (get-whithout-last list)
  (cond ((null? (cdr list)) '())
        (else (cons (car list) (get-whithout-last (cdr list))))))

;(get-parent d '("/opt/tectia" "tectia" ()))
;'("/opt" "opt" (("/opt/tectia" "tectia" ())))
(define (get-parent dir current )
  (get-by-listed-path dir (get-whithout-last (split-input (full-path current)))))


;(define (cd dir current-dir input-list)
;(cond ((null? dir) #f)
;((null? (cdr input-list))
;(if (equaal
(define (up-or-down dir current next)
  (cond ((equal? next ".."); we need to get the parrent
         (get-parent dir current )) 
        ((equal? (string-ref next 0) #\/) ;we need to start from the root
         (get-childs-by-path dir next))
        (else (get-child-by-name current next))))


(define (cd-list dir current input-list)
  (let ([next-folder (car input-list)] )
  (cond ((equal? next-folder "..") (get-parent dir current ))
        ((equal? (string-ref next-folder 0) #\/)
         (get-by-listed-path dir input-list))
        (else (get-childs-by-path current input-list)))))
         
;> (cd d c "/u/walsh/test/file.txt")
;#f
;> (cd d c "/u/walsh/test/")
;'("/u/walsh/test" "test" (("/u/walsh/test/file.txt" "file.txt" "some text")))
;> (cd d c "../u")
 
(define (cd dir current input)
(let ([result (cd-list dir current (split-input input))])
  (if (is-file? result) #f result)))















