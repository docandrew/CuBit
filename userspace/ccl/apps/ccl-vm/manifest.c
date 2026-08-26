/* CuBit Control Language VM test application identity. */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    0x02,
    0x10, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'c', 'c', 'l', '-', 'v', 'm',

    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

/* The test module may call only the CCL test host service, through slot 24. */
static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x01, 0x00,

    /* REQ_SERVICE, read/write, slot 24, driver 18. */
    0x02, 0x03, 0x18, 0x00,
    0x12, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
};
