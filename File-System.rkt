#lang racket

(define full-path car)   ;Full path to folder/file
(define name cadr)       ;Name of the folder/file
(define content caddr)   ;Content of the folder/file


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
(define p "/u/walsh")
(define c (cadr (content d)))



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
(define (get-from-root-listed-path root-dir path-list)
  (cond ((equal? (name root-dir)(car path-list))
         (get-childs-by-path root-dir  (cdr path-list)))
        (else #f)))

(define (get-by-listed-path dir path-list)
  (cond ((equal? (name dir)(car path-list))
         (get-childs-by-path dir  (cdr path-list)))
        (else #f)))

                     
  
(define (get-whithout-last list)
 ; (cond ((null? (cdr list)) '())
  ;      (else (cons (car list) (get-whithout-last (cdr list))))))
  (reverse (list-tail (reverse list) (- (length list) (- (length list) 1)))))
  
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
 
;(define (cd dir current input)
;(let ([result (cd-list dir current (split-input input))])
 ; (if (is-file? result) #f result)))


;(get-by-path d (up-or-down2 d c (split-input "../u")) (split-input "walsh/../walsh/test/../test/file.txt"))
;(get-by-path d (up-or-down2 d c (split-input "../u")) (split-input "/"))
;(get-by-path d d (split-input "/"))
(define (get-by-path root-dir current listed-path)
    (cond ((null? listed-path) current)
          ((equal? #f current) #f)
          ((equal? (car listed-path) ".."); we need to get the parrent
           (get-by-path root-dir (get-parent root-dir current ) (cdr listed-path)))
          ((equal? (string-ref (car listed-path) 0) #\/) ;we need to start from the root
           (get-by-path root-dir root-dir (cdr listed-path) ))
          (else (get-by-path root-dir (get-child-by-name current (car listed-path)) (cdr listed-path)))))



(define root-dir d)
(define current-dir root-dir)

(define (cd-final input-path)
  (let [(result (get-by-path root-dir root-dir (split-input input-path)))]
  (if (is-folder? result) result #f)))


(define (ls . path)
  (if (null? path)
      (get-names-foreach-content current-dir)
      (get-names-foreach-content
       (get-by-path root-dir current-dir (split-input (car path))))))

  












