;; -----------------------------------------------------------------------------
;; USB serial wrapper around pjrc usb_serial library
;; -----------------------------------------------------------------------------
(module (avr usb)
        (usb-config
         usb-send-string
         usb-ready
         usb-serial-get-control
         println
         )

(import (scheme base)
        (crunch c)
        (crunch memory)
        (crunch declarations)
        (crunch aggregate-types)
        (chicken syntax)
        (avr base))

(c-include "usb_serial.h")
#>
int usb_control()
{
 	return usb_serial_get_control() & USB_SERIAL_DTR;
}
void usb_serial_putchar_int(int v)
{
	(void)usb_serial_putchar(v & 0xFF);
}
int usb_serial_getchar_int()
{
 	int16_t v = usb_serial_getchar();
        return v < 0 ? -1 : v & 0xFF;
}
void usb_serial_write_num(int val) {
   char msg[128];
   sprintf(msg, "%d", val);
   usb_serial_write(msg, strlen(msg));
}
void usb_serial_write_hex(int val) {
   char msg[128];
   sprintf(msg, "0x%02x", val) & 0xFF;
   usb_serial_write(msg, strlen(msg));
}
<#

(define usb-config (c-external usb_init () void))
(define usb-ready (c-external usb_configured () integer))
(define usb-serial-get-control (c-external usb_control () integer))
(define usb-serial-write (c-external usb_serial_write ((pointer char) integer) void))
(define usb-serial-write-num (c-external usb_serial_write_num (integer) void))
(define usb-serial-write-hex (c-external usb_serial_write_hex (integer) void))
(define usb-serial-getchar (c-external usb_serial_getchar_int () integer))
(define usb-serial-putchar (c-external usb_serial_putchar_int (integer) void))

(define (usb-send-string str)
        (usb-serial-write (string->pointer str) (string-length str)))


(define-syntax println
    (syntax-rules ()
        ((_ VAL ...)
         (begin
             (begin (usb-send-string VAL) (usb-send-string " "))
             ...
             (usb-send-string "\r\n")))))
)

