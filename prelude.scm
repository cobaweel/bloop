#!/usr/bin/scsh -s
!#

(define = equal?)
(define (_show x)
  (cond ((eq? x #f) (display "NO"))
        ((eq? x #t) (display "YES"))
        (else (display x)))
  (newline))

(define REMAINDER remainder)
(define (AND x y) (and x y))
(define (OR x y) (or x y))

(define-syntax _defproc
  (syntax-rules ()
    ((_defproc (nargl ...) (cells ...) s ...)
     (define (nargl ...)
       (let (cells ...)
         s ...)))))

(define-syntax _block
  (syntax-rules ()
    ((_block escape s ...)
     (call-with-current-continuation
      (lambda (escape) s ...)))))

(define-syntax _bloop
  (syntax-rules ()
    ((_bloop escape n s ...)
     (call-with-current-continuation
      (lambda (escape)
        (let loop ((i n))
          (if (> i 0)
              (begin s ...
                     (loop (- i 1))))))))))

(define-syntax _muloop
  (syntax-rules ()
    ((_muloop escape s ...)
     (call-with-current-continuation
      (lambda (escape)
        (let loop () s ... (loop)))))))

(define-syntax _when
  (syntax-rules ()
    ((_when c s ...)
     (if c (begin s ...)))))

