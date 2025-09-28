.SUFFIXES:	.scm .elf .hex .c .o .so
DEPS=		deps

CC=		avr-gcc
OBJCOPY=	avr-objcopy

CHCR_CFLAGS!=	chicken-crunch -cflags
CFLAGS= 	-Os -mmcu=atmega32u4
CFLAGS+=	--param=min-pagesize=0
CFLAGS+=	-DF_CPU=16000000UL
CFLAGS+=	-DCRUNCH_FREESTANDING
CFLAGS+=	$(CHCR_CFLAGS) -I. -I$(DEPS)/usb_serial

SOBJS=		byte-utils.import.so
SOBJS+=		byte-rbuf.import.so
SOBJS+=		ps2-decoder.import.so
SOBJS+=		avr.base.import.so
SOBJS+=		avr.ps2.import.so
SOBJS+=		avr.usb.import.so

OBJS=		byte-utils.o
OBJS+=		byte-rbuf.o ps2-decoder.o
OBJS+=		avr.base.o
OBJS+=		avr.ps2.o
OBJS+=		avr.usb.o $(DEPS)/usb_serial/usb_serial.o

TARGET=		decops2.hex

build: $(OBJS)
	# first make OBJS and generate .import.scm files
	# then run make again and build import.so files out of that.
	$(MAKE) build-all

build-all: $(SOBJS) main.o $(TARGET)

$(TARGET): main.o $(OBJS)
	$(CC) $(CFLAGS) -o $@ main.o $(OBJS)
	$(OBJCOPY) -O ihex $< $@

.scm.so:
	csc -s $<

.scm.c:
	chicken-crunch -J -I . $< -o $@

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

flash: build
	teensy_loader_cli -mmcu=atmega32u4 -w $(TARGET)

clean:
	rm -f *.hex *.elf *.o *.so *.c $(OBJS) $(SOBJS) *.import.*
