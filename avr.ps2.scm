(module (avr ps2)
        (ps2-init
         ps2-ready?
         ps2-read

         ; must export this one to ensure it won't be removed
         USART1_RX_vect)

(import (scheme base))
(import (crunch c)
        (crunch memory)
        (crunch declarations)
        (crunch aggregate-types)
        (chicken bitwise)
        (chicken number-vector)
        (avr base)
        byte-rbuf)

; c-include avr headers
(avr-include)

(c-declare "struct rbuf *g_rbp = NULL;")
(define get-rb  (c-lambda () (pointer (struct rbuf)) "g_rbp;"))
(define set-rb! (c-lambda (((pointer (struct rbuf)) ptr)) void "g_rbp = ptr;"))

;; -----------------------------------------------------------------------------
;; USART-based PS2 (lucky case)
;; -----------------------------------------------------------------------------

(define (open-clk)
    ;(set-bits! PORTD 5)
    (clr-bits! DDRD 5))
(define (pull-clk)
    (set-bits! DDRD 5)
    (clr-bits! PORTD 5))
(define (clk-hi?)
    (read-bits PIND 5))
(define (clk-lo?)
    (zero? (read-bits PIND 5)))

(define (open-dat)
    ;(set-bits! PORTD 2)
    (clr-bits! DDRD 2))
(define (pull-dat)
    (set-bits! DDRD 2)
    (clr-bits! PORTD 2))
(define (dat-hi?)
    (read-bits PIND 2))
(define (dat-lo?)
    (zero? (read-bits PIND 2)))

#>
void USART1_RX_vect(void) __attribute__ ((__signal__,__used__, __externally_visible__));
<#
(define (USART1_RX_vect)
    ; Check for frame & parity errors; discard if present.
    ; FE1 set => invalid stop; UPE1 set => parity mismatch.
    (cli!)
    (when (zero? (read-bits UCSR1A FE1 DOR1 UPE1))
        ; Valid 8 data bits received; start bit is implicitly validated by correct framing.
        (let ((x (read-reg UDR1)))
            (rbuf-push! (get-rb) x)))
    (sei!))

(define (ps2-drain!)
    (rbuf-clear! (get-rb)))

(define (ps2-ready?)
    (not (rbuf-empty? (get-rb))))

(define (ps2-read)
    (rbuf-pop! (get-rb)))

(define (usart1-error)
    (read-bits UCSR1A FE1 DOR1 UPE1))

(define (usart1-poweron)
    (clr-bits! PRR1 PRUSART1))
(define (usart1-poweroff)
    (set-bits! PRR1 PRUSART1))

(define (usart1-on)
    ; Disable TX/RX while changing mode
    (clr-reg! UCSR1B)

    ; UMSEL1[1:0]=01 (sync),
    ; UPM1[1:0]=11 (odd parity),
    ; USBS1=0 (1 stop)
    ; UCSZ1[1:0]=11 (8-bit), UCPOL1=0
    (clr-reg! UCSR1C)
    (set-bits! UCSR1C UMSEL10 UPM11 UPM10 UCSZ11 UCSZ10 UCPOL1)

    ; In synchronous slave, UBRR is ignored for clocking; set any value.
    (clr-reg! UCSR1A)
    (clr-reg! UBRR1H)
    (clr-reg! UBRR1L)

    ; Enable receiver and RX complete interrupt
    (set-bits! UCSR1B RXEN1 RXCIE1))

; Turn USART1 fully off so PD2/PD5 can be GPIO for bit-bang TX
(define (usart1-off)
	(clr-reg! UCSR1C)
	(clr-bits! UCSR1B RXEN1 TXEN1))

(define (ps2-init)
    (set-rb! (make-rbuf 32))
    (cli!)
    (usart1-poweron)
    ; Release lines (idle-high) via pull-ups
    (open-dat)
    (open-clk)
    (ps2-drain!)
    (usart1-on)
    (sei!)
    (delay-ms 50)))
