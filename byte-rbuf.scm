;; -----------------------------------------------------------------------------
;; byte-rbuf: ringbuffer of bytes, at most 255 items
;; -----------------------------------------------------------------------------
(module byte-rbuf
    (make-rbuf
     rbuf-clear!
     rbuf-empty?
     rbuf-push!
     rbuf-pop!)

    (import (scheme base)
            (crunch c)
            (chicken base)
            (crunch memory)
            (crunch declarations)
            (crunch aggregate-types)
            (chicken bitwise)
            byte-utils)

    (define-struct rbuf
        (cap  byte)
        (head byte)
        (tail byte)
        (buf  bytevector))

    (define-compound-accessors (pointer (struct rbuf))
        (mk-rbuf cap head tail buf)
        (cap  rbuf-cap)
        (head rbuf-head rbuf-head!)
        (tail rbuf-tail rbuf-tail!)
        (buf  rbuf-buf))

    (: (make-rbuf integer) (pointer (struct rbuf)))
    (define (make-rbuf cap)
        ; cap must be power of 2!
        (mk-rbuf (integer->byte cap)
                 (integer->byte 0)
                 (integer->byte 0)
                 (make-bytevector cap)))

    (: (rbuf-clear! (pointer (struct rbuf))) void)
    (define (rbuf-clear! rb)
        (rbuf-head! rb 0)
        (rbuf-tail! rb 0))

    (: (rbuf-push! (pointer (struct rbuf))) boolean)
    (define (rbuf-empty? rb)
        (= (rbuf-head rb) (rbuf-tail rb)))

    (: (next-index byte byte) byte)
    (define (next-index idx cap)
        (let ((idx (byte+ idx (integer->byte 1)))
              (cap (byte- cap (integer->byte 1))))
            (integer->byte
              (bitwise-and (byte->integer idx)
                           (byte->integer cap)))))

    (: (next-head (pointer (struct rbuf))) byte)
    (define (next-head rb)
        (next-index (rbuf-head rb) (rbuf-cap rb)))

    (: (rbuf-push! (pointer (struct rbuf)) byte) boolean)
    (define (rbuf-push! rb value)
        (let* ((nhead (next-head rb))
              (full (byte=? (rbuf-tail rb) nhead)))
              (unless full
        	      (bytevector-u8-set! (rbuf-buf rb)
                                          (byte->integer (rbuf-head rb))
                                          (byte->integer value))
                      (rbuf-head! rb nhead))
              (not full)))


    (: (next-tail (pointer (struct rbuf))) byte)
    (define (next-tail rb)
        (next-index (rbuf-tail rb) (rbuf-cap rb)))

    (: (rbuf-pop! (pointer (struct rbuf))) byte)
    (define (rbuf-pop! rb)
        (if (rbuf-empty? rb)
          (integer->byte 0)
          (let ((ntail (next-tail rb))
                (value (bytevector-u8-ref (rbuf-buf rb)
                                          (byte->integer (rbuf-tail rb)))))
              (rbuf-tail! rb ntail)
              value)))
)
