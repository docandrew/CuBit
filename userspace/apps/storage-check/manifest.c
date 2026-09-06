/* CuBit live-storage diagnostic identity and requested authorities. */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44, 0x01, 0x00, 0x02, 0x00,
    0x02, 0x17, 0x00, 'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    's', 't', 'o', 'r', 'a', 'g', 'e', '-', 'c', 'h', 'e', 'c', 'k',
    0x07, 0x05, 0x00, 'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    0x54, 0x49, 0x42, 0x43, 0x01, 0x00, 0x01, 0x00,

    /* Filesystem service: RW endpoint at slot 1, driver ID 6. */
    0x02, 0x03, 0x01, 0x00,
    0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
};

static const unsigned char __cubit_access[]
    __attribute__((section(".cubit.access"), used)) = {
    0x43, 0x41, 0x43, 0x43, 0x01, 0x00, 0x01, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* READ | WRITE | CREATE, scoped to the headless NVMe test volume. */
    0x0B, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    '@', 'n', 'v', 'm', 'e', ':', '0', '/',
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0
};
