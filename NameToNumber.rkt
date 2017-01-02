#lang r5rs

; ; ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; ; ;; CSCI
; ; ; ;; Winter 2016
; ; ; ;;
; ; ; ;;Number to Name
; ; ; ;;
; ; ; ;;Moustafa Naquib
; ; ; ;;
; ; ; ;;
; ; ; ;;The purpose of this program is to turn
; ; ; ;;any given positive integer number into
; ; ; ;;its equivalent english string list.
; ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;find name of number under 20
(define (find10s x)
  (cond
    ((= x 0) '())
    ((= x 1) '(one))
    ((= x 2) '(two))
    ((= x 3) '(three))
    ((= x 4) '(four))
    ((= x 5) '(five))
    ((= x 6) '(six))
    ((= x 7) '(seven))
    ((= x 8) '(eight))
    ((= x 9) '(nine))
    ((= x 10) '(ten))
    ((= x 11) '(eleven))
    ((= x 12) '(twelve))
    ((= x 13) '(thirteen))
    ((= x 14) '(fourteen))
    ((= x 15) '(fifteen))
    ((= x 16) '(sixteen))
    ((= x 17) '(seventeen))
    ((= x 18) '(eighteen))
    ((= x 19) '(nineteen))
    )
  )

;find name of number under 100 and then combines with results of find10's
(define (find100s x)
  (cond
    ((= (quotient x 10) 0) (append '() (find10s (remainder x 10))))
    ((< (quotient x 10) 2) (append '() (find10s x)))
    ((= (quotient x 10) 2) (append '(twenty) (find10s (remainder x 10))))
    ((= (quotient x 10) 3) (append '(thirty) (find10s (remainder x 10))))
    ((= (quotient x 10) 4) (append '(fourty) (find10s (remainder x 10))))
    ((= (quotient x 10) 5) (append '(fifty) (find10s (remainder x 10))))
    ((= (quotient x 10) 6) (append '(sixty) (find10s (remainder x 10))))
    ((= (quotient x 10) 7) (append '(seventy) (find10s (remainder x 10))))
    ((= (quotient x 10) 8) (append '(eighty) (find10s (remainder x 10))))
    ((= (quotient x 10) 9) (append '(ninety) (find10s (remainder x 10))))
    )
  )

;finds names of numbers under 1000 then combines with results of find10's
(define (find1000s x)
  (cond ((= (quotient x 100) 1) (append '(one hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 2) (append '(two hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 3) (append '(three hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 4) (append '(four hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 5) (append '(five hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 6) (append '(six hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 7) (append '(seven hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 8) (append '(eight hundred) (find100s (remainder x 100))))
        ((= (quotient x 100) 9) (append '(nine hundred) (find100s (remainder x 100))))
        ))

;calls either finds 10s 100s or 1000s to find number under 1000
(define (name<1000 x)
  ;if x is smaller than 20
  (if (< x 20)
      ;call find10s function only
      (find10s x)
      ;if x is smaller than 100
      (if (< x 100)
          ;call find100s function
          (find100s x)
          ;else call find1000s function
          (find1000s x)
          )
      ))

;names number depending on given i based on times called
(define (namer x)
  (cond ((= x 1) '())
        ((= x 2) '(thousand))
        ((= x 3) '(million))
        ((= x 4) '(billion))
        ((= x 5) '(trillion))
        ((= x 6) '(quadrillion))
        ((= x 7) '(quintillion))
        ((= x 8) '(sextillion))
        ((= x 9) '(septillion))
        ((= x 10) '(octillion))
        ((= x 11) '(nonillion))
        ((= x 12) '(decillion))
        ((= x 13) '(undecillion))
        ((= x 14) '(duodecillion))
        ((= x 15) '(tredecillion))
        ((= x 16) '(quattuordecillion))
        ((= x 17) '(quindecillion))
        ((= x 18) '(sexdecillion ))
        ((= x 19) '(septendecillion))
        ((= x 20) '(octodecillion))
        ((= x 21) '(novemdecillion ))
        ((= x 22) '(vigintillion))
        )         
  )

;puts number given into list split up into groups of threes and inserts name in between
(define (split n i) (if (zero? n) `()
                        (append (split (quotient n 1000) (+ i 1)) (append (name<1000 (remainder n 1000))
                                                                          ;if remainder is 0 dont return anything
                                                                          (if(= (remainder n 1000) 0)
                                                                             '()
                                                                             ;otherwise find name>=1000
                                                                             (namer i)))))
  )

;calls split function and returns
(define (number-name x)
  ;if x is 0 return zero
  (if (= x 0)
      '(zero)
      ;otherwise commence the PROCESS
      (split x 1))
  )
