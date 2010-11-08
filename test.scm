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

(_defproc 
 (TWO-TO-THE-THREE-TO-THE N) 
 ((_output 0) (_cell0 0) (_cell1 0)) 
 (_block 
  _quit-1 
  (_block 
   _quit0 
   (set! _cell0 1) 
   (_bloop _abort1 N (_block _quit1 (set! _cell0 (* 3 _cell0)) _output)) 
   (set! _cell1 1) 
   (_bloop _abort2 _cell0 (_block _quit2 (set! _cell1 (* 2 _cell1)) _output)) 
   (set! _output _cell1) 
   _output) 
  _output))
(_show "TWO-TO-THE-THREE-TO-THE[1]")
(_show (TWO-TO-THE-THREE-TO-THE 1))
(_show "TWO-TO-THE-THREE-TO-THE[2]")
(_show (TWO-TO-THE-THREE-TO-THE 2))
(_show "TWO-TO-THE-THREE-TO-THE[3]")
(_show (TWO-TO-THE-THREE-TO-THE 3))
(_defproc 
 (MINUS M N) 
 ((_output 0)) 
 (_block 
  _quit-1 
  (_block 
   _quit0 
   (_when (< M N) (_quit0)) 
   (_bloop 
    _abort1 
    (+ M 1) 
    (_block 
     _quit1 
     (_when (= (+ _output N) M) (_abort1)) 
     (set! _output (+ _output 1)) 
     _output)) 
   _output) 
  _output))
(_show "MINUS[5,1]")
(_show (MINUS 5 1))
(_show "MINUS[43,1]")
(_show (MINUS 43 1))
(_show "MINUS[43,20]")
(_show (MINUS 43 20))
(_defproc 
 (PRIME? N) 
 ((_output #f) (_cell0 0)) 
 (_block 
  _quit-1 
  (_block 
   _quit0 
   (_when (= N 0) (_quit0)) 
   (set! _cell0 2) 
   (_bloop 
    _abort1 
    (MINUS N 2) 
    (_block 
     _quit1 
     (_when (= (REMAINDER N _cell0) 0) (_quit0)) 
     (set! _cell0 (+ _cell0 1)) 
     _output)) 
   (set! _output #t) 
   _output) 
  _output))
(_show "PRIME?[1]")
(_show (PRIME? 1))
(_show "PRIME?[2]")
(_show (PRIME? 2))
(_show "PRIME?[3]")
(_show (PRIME? 3))
(_show "PRIME?[5]")
(_show (PRIME? 5))
(_show "PRIME?[7]")
(_show (PRIME? 7))
(_show "PRIME?[101]")
(_show (PRIME? 101))
(_show "PRIME?[40]")
(_show (PRIME? 40))
(_show "PRIME?[20]")
(_show (PRIME? 20))
(_show "PRIME?[0]")
(_show (PRIME? 0))
(_show "PRIME?[30]")
(_show (PRIME? 30))
(_show "PRIME?[10]")
(_show (PRIME? 10))
(_defproc 
 (GOLDBACH? N) 
 ((_output #f) (_cell0 0)) 
 (_block 
  _quit-1 
  (_block 
   _quit0 
   (set! _cell0 2) 
   (_bloop 
    _abort1 
    N 
    (_block 
     _quit1 
     (_when 
      (AND (PRIME? _cell0) (PRIME? (MINUS N _cell0))) 
      (_block _quit2 (set! _output #t) (_quit0) _output)) 
     (set! _cell0 (+ _cell0 1)) 
     _output)) 
   _output) 
  _output))
(_show "GOLDBACH?[2]")
(_show (GOLDBACH? 2))
(_show "GOLDBACH?[4]")
(_show (GOLDBACH? 4))
(_show "GOLDBACH?[6]")
(_show (GOLDBACH? 6))
(_show "GOLDBACH?[8]")
(_show (GOLDBACH? 8))
(_show "GOLDBACH?[10]")
(_show (GOLDBACH? 10))
(_show "GOLDBACH?[12]")
(_show (GOLDBACH? 12))
(_show "GOLDBACH?[14]")
(_show (GOLDBACH? 14))
(_show "GOLDBACH?[16]")
(_show (GOLDBACH? 16))
(_show "GOLDBACH?[18]")
(_show (GOLDBACH? 18))