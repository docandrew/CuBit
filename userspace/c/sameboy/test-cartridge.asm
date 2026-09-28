; Original CuBit test cartridge: checkerboard, joypad scrolling and a test tone.
; No game or firmware ROM data. Build with RGBDS; RGBFIX fills the GB header.
SECTION "Header", ROM0[$100]
    nop
    jp Start
    ds $150 - @, 0
SECTION "Code", ROM0[$150]
Start:
    di
    ld sp, $dfff
.wait:
    ldh a, [$ff44]
    cp 144
    jr c, .wait
    xor a
    ldh [$ff40], a
    ld hl, $8000
    ld b, 8
.tile:
    ld a, $0f
    ld [hli], a
    ld a, $33
    ld [hli], a
    dec b
    jr nz, .tile
    ld hl, $9800
    ld bc, 1024
.map:
    xor a
    ld [hli], a
    dec bc
    ld a, b
    or c
    jr nz, .map
    ld a, $e4
    ldh [$ff47], a
    ld a, $91
    ldh [$ff40], a
    ; Channel 2: continuous ~440 Hz square wave, moderate envelope amplitude.
    ld a, $80
    ldh [$ff26], a
    ld a, $77
    ldh [$ff24], a
    ld a, $22
    ldh [$ff25], a
    ld a, $80
    ldh [$ff16], a
    ld a, $80
    ldh [$ff17], a
    ld a, $d6
    ldh [$ff18], a
    ld a, $86
    ldh [$ff19], a
.frame:
    ldh a, [$ff44]
    cp 144
    jr c, .frame
    ld a, $20
    ldh [$ff00], a
    ldh a, [$ff00]
    bit 0, a
    jr nz, .left
    ldh a, [$ff43]
    inc a
    ldh [$ff43], a
.left:
    ldh a, [$ff00]
    bit 1, a
    jr nz, .end
    ldh a, [$ff43]
    dec a
    ldh [$ff43], a
.end:
    ldh a, [$ff44]
    cp 144
    jr nc, .end
    jr .frame
