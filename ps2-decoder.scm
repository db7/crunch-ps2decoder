;; -----------------------------------------------------------------------------
;; PS/2 decoder
;; -----------------------------------------------------------------------------
(module ps2-decoder
        (make-ps2dec
         ps2dec-feed
         ps2dec-xval
         ps2dec-yval
         ps2dec-hdr
         )

(import (scheme base)
        (crunch c)
        (crunch memory)
        (crunch declarations)
        (crunch aggregate-types)
        (chicken bitwise)
        (chicken number-vector))

(define-enum State (EMPTY HEADER PARTIAL READY))

(define-struct ps2dec
    (hdr  byte)
    (xval  byte)
    (yval  byte)
    (state byte))

(define-compound-accessors (pointer (struct ps2dec))
    (mk-ps2dec hdr xval yval state)
    (hdr ps2dec-hdr ps2dec-hdr!)
    (xval ps2dec-xval ps2dec-xval!)
    (yval ps2dec-yval ps2dec-yval!)
    (state ps2dec-state ps2dec-state!))

(: (make-ps2dec) (pointer (struct ps2dec)))
(define (make-ps2dec)
    (mk-ps2dec 0 0 0 (State EMPTY)))

(: (ps2dec-feed (pointer (struct ps2dec)) byte) boolean)
(define (ps2dec-feed pd b)
    (let ((now 0))
        ;(when (and (= (ps2dec-state pd) (State HEADER))
        ;           (> now 0))
        ;      (ps2dec-state! pd (State EMPTY)))
        (cond
            ((or (= (ps2dec-state pd) (State PARTIAL))
                 (= (ps2dec-state pd) (State EMPTY)))
             ;;if not        (bitwise-and (byte->integer b) #x08)
             ;;   return because it is not a hdrer
             (ps2dec-hdr! pd b)
             (ps2dec-state! pd (State HEADER))
             ; set timestamp
             #f)
            ((= (ps2dec-state pd) (State HEADER))
             (ps2dec-xval! pd b)
             (ps2dec-state! pd (State PARTIAL))
             #f)
            ((= (ps2dec-state pd) (State PARTIAL))
             (ps2dec-yval! pd b)
             (ps2dec-state! pd (State READY))
             #t)
            (else  ;should not happen
             #f))))
)
