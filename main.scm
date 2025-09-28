;; -----------------------------------------------------------------------------
;; Simple PS/2 decoder implemented in CRUNCH
;;
;; Reads in a loop the values sent by a PS/2 mouse (trackpoint), decodes them,
;; and prints them via USB serial.
;;
;; Hardware:
;; 	atmega32u4
;; 	obscure PS/2 chip sold by SurnQiee and others
;; -----------------------------------------------------------------------------
(import (scheme base)
        (crunch c)
        (avr usb)
        (avr ps2)
        (avr base)
        ps2-decoder
        byte-utils)

; c-include avr headers
(avr-include)

;; -----------------------------------------------------------------------------
;; Teensy 2.0 LED, connected at PD6.
;; DDR pin 1 is output, 0 is input
;; -----------------------------------------------------------------------------
(define (led-config)
  (set-bits! DDRD 6))
(define (led-on)
  (set-bits! PORTD 6))
(define (led-off)
  (clr-bits! PORTD 6))

;; -----------------------------------------------------------------------------
;; main program
;; -----------------------------------------------------------------------------
(define (main)
  ; Configure clock, LEDs and USB
  (set-clock-prescale! clock_div_1)
  (led-config)
  (usb-config)
  (await (usb-ready))

  ; Enter infinite loop alternating LED state every 100 ms.
  (await (usb-serial-get-control))
  (ps2-init)
  (led-on)
  (println "PS2 decoder")
  (let ((pd (make-ps2dec)))
     (loop
        (println "piip")
        (if (ps2-ready?)
            (when (ps2dec-feed pd (ps2-read))
                (println "ps2"
                         "ts"  ;(number->string (ps2dec-ts pd))
                         "dx"  (byte->string (ps2dec-xval pd))
                         "dy"  (byte->string (ps2dec-yval pd))
                         "hdr" (byte->string (ps2dec-hdr pd))
                         ;"buttons (ps2dec-buttons pd)"
                         ;"overflow (ps2dec-overflow pd)"
                         ))
            (delay-ms 100)))))
