;; -----------------------------------------------------------------------------
;; byte-utils: Some byte-related procedures missing in CRUNCH
;; -----------------------------------------------------------------------------
(module byte-utils
        (byte=?
         byte+
         byte-
         byte->integer
         integer->byte
         byte->string)

(import (scheme base)
        (crunch c)
        (crunch declarations))

    (: (byte=? byte byte) boolean)
    (define byte=? (c-lambda ((byte x) (byte y)) boolean "return x == y;"))

    (: (byte+ byte byte) byte)
    (define byte+ (c-lambda ((byte x) (byte y)) byte "return (x + y);"))

    (: (byte- byte byte) byte)
    (define byte- (c-lambda ((byte x) (byte y)) byte "return (x - y);"))

    (: (byte->integer byte) integer)
    (define byte->integer (c-lambda ((byte x)) integer "return (long) x;"))

    (: (integer->byte integer) byte)
    (define integer->byte (c-lambda ((integer x)) byte "return (uint8_t) x;"))

    (: (byte->string byte) string)
    (define (byte->string b)
        (number->string
         (byte->integer b)))
)
