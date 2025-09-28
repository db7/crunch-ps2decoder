# PS/2 Mouse Decoder (Teensy 2.0 + CRUNCH Scheme)

This project implements a PS/2 mouse/trackpoint decoder on a Teensy 2.0
(ATmega32U4).  It uses USART1 in synchronous slave mode to sample PS/2 frames
and a ring buffer + state machine written in [CRUNCH
Scheme](https://wiki.call-cc.org/eggref/6/crunch).  Decoded motion packets are
streamed to the host via USB using PJRC’s `usb_serial`.

## Features
- No bit-banging — PS/2 captured in hardware via USART1.
- Handles unmarked PS/2 IC that streams immediately
 (no init required, no config possible).
- Ring buffer shared between ISR and main loop (atomic access).

## Hardware

- **Board:** Teensy 2.0 (ATmega32U4, 16 MHz)
- **Connections:**

```
DAT ── PD2 (RXD1)
CLK ── PD5 (XCK1)
VCC ── VCC (5 V)
GND ── GND
RST ── leave floating

CLK and DAT require 4.7 kΩ pull-ups to VCC.
```

## Software requirements
- `avr-gcc`, `make`, `teensy_loader_cli`
- CHICKEN Scheme v6.0.0pre1 + CRUNCH egg v0.98
- `curl` and `git` (for fetching `usb_serial`)

## Build & flash

First, fetch and build PJRC’s `usb_serial`:
```sh
make -C deps
```

Next, build firmware and flash board:

```sh
make
make flash
```

## Usage

Connect the Teensy to USB. Decoded PS/2 packets (X, Y motion, flags) are printed
over CDC serial.
Use `screen`, `minicom`, or `cu` to view output.

## Project structure

```
.
├── Makefile             # Build rules, fetches PJRC usb_serial if missing
├── main.scm             # Entry point, init + main loop
├── avr.ps2.scm          # USART1 setup, RX ISR, ps2-ready?/ps2-read
├── byte-rbuf.scm        # Ring buffer implementation
├── ps2-decoder.scm      # PS/2 packet state machine (header → motion)
├── byte-utils.scm       # Helpers for byte arithmetic and comparisons
├── avr.usb.scm          # Wrapper around PJRC usb_serial
└── crunch-environment.h # Freestanding CRUNCH environment config
```

## License

This project is released under the **0BSD license** -- free for any use, without
attribution requirements.
