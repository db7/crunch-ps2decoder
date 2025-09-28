(module (avr base)
        (set-bits!
         clr-bits!
         read-bits
         clr-reg!
         set-clock-prescale!
         delay-ms
         delay-us
         avr-include
         read-reg
         cli!
         sei!
         await
         loop)

        (import (scheme base))

(import (scheme base)
        (chicken syntax)
        (chicken bitwise)
        (crunch c)
        (crunch memory)
        (crunch aggregate-types)
        (crunch declarations))

(import-for-syntax
    (chicken bitwise))

(define-syntax avr-include
    (er-macro-transformer
     (lambda (x r c)
        `(,(r 'c-include)
                <util/delay.h>
                <avr/interrupt.h>
        	<avr/io.h>
        	<avr/power.h>))))
(avr-include)

(begin-for-syntax
 (import srfi-13)
    (define (combine-or lst)
        string-append (string-join lst " | "))
    (define (shift-bit . bits)
        (let* ((bits (map (lambda (bit)
                             (cond
                                  ((number? bit) (if (or (< bit 0) (>= bit 8))
                                                      (error 'shift-bit "invalid bit" bit)
                                                      (number->string bit)))
                                  ((symbol? bit) (symbol->string bit))
                                  (else (error 'shift-bit))))
                         bits))
              (bits (map (lambda (bit) (string-append "(1 << " bit ")")) bits)))
        (combine-or bits)))
)

(define-syntax await
    (er-macro-transformer
     (lambda (x r c)
         `(,(r 'do) () ,(cdr x) #f))))

(define-syntax loop
    (er-macro-transformer
     (lambda (x r c)
         `(,(r 'do) () (#f) ,@(cdr x)))))

(define-syntax read-bits
    (er-macro-transformer
     (lambda (x r c)
         (let* ((reg (symbol->string (cadr x)))
                (val (apply shift-bit (cddr x))))
         `(,(r 'c-value) integer ,@(list (string-append reg " & (" val ")")))))))

(define-syntax set-bits!
    (er-macro-transformer
     (lambda (x r c)
         (let* ((reg (symbol->string (cadr x)))
                (val (apply shift-bit (cddr x))))
         `(,(r 'c-value) void ,@(list (string-append reg " |= " val )))))))

(define-syntax clr-bits!
    (er-macro-transformer
     (lambda (x r c)
         (let* ((reg (symbol->string (cadr x)))
                (val (apply shift-bit (cddr x))))
         `(c-value void ,@(list (string-append reg " &= ~(" val ")")))))))

(define-syntax set-reg!
    (er-macro-transformer
     (lambda (x r c)
         (let* ((reg (symbol->string (cadr x)))
                (val (apply shift-bit (cddr x))))
         `(c-value void ,@(list (string-append reg " = " val)))))))

(define-syntax clr-reg!
   (syntax-rules ()
     ((_ REG) (set-reg! REG 0))))

(define-syntax read-reg
    (er-macro-transformer
     (lambda (x r c)
         (let* ((reg (symbol->string (cadr x))))
         `(c-value integer ,@(list reg))))))

(define-syntax set-clock-prescale!
                (er-macro-transformer
                 (lambda (x r c)
                     (let ((val (symbol->string (cadr x))))
                     `(,(r 'c-value) void ,(string-append "clock_prescale_set(" val ")"))))))

(define-syntax delay-ms
                (er-macro-transformer
                 (lambda (x r c)
                     (let* ((val (cadr x))
                            (val (cond
                                   ((number? val) (number->string val))
                                   ((symbol? val) (symbol->string val))
                                   (else (error 'delay-ms "invalid delay value" val)))))
                     `(,(r 'c-value) void ,(string-append "_delay_ms(" val ")"))))))

(define-syntax delay-us
                (er-macro-transformer
                 (lambda (x r c)
                     (let* ((val (cadr x))
                            (val (cond
                                   ((number? val) (number->string val))
                                   ((symbol? val) (symbol->string val))
                                   (else (error 'delay-ms "invalid delay value" val)))))
                     `(,(r 'c-value) void ,(string-append "_delay_us(" val ")"))))))



(c-include <avr/interrupt.h>)
(c-include <util/delay.h>)
(c-include <stdbool.h>)

#>
void call_sei() { sei(); }
void call_cli() { cli(); }
<#

(define sei!
    (c-lambda () void "sei();"))
(define cli!
    (c-lambda () void "cli();"))
)

