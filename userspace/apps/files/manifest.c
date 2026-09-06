/* CuBit Files: a read-only, authority-scoped directory browser. */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44, 0x01, 0x00, 0x02, 0x00,
    0x02, 0x0F, 0x00, 'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'f', 'i', 'l', 'e', 's',
    0x07, 0x05, 0x00, 'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    0x54, 0x49, 0x42, 0x43, 0x01, 0x00, 0x02, 0x00,

    /* Desktop surface service: RW endpoint at slot 21. */
    0x02, 0x03, 0x15, 0x00,
    0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,

    /* Filesystem service: RW endpoint at slot 22. */
    0x02, 0x03, 0x16, 0x00,
    0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
};

static const unsigned char __cubit_access[]
    __attribute__((section(".cubit.access"), used)) = {
    0x43, 0x41, 0x43, 0x43, 0x01, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* Read-only view of the first NVMe filesystem root. */
    0x01, 0x08, 0,0,0,0,0,0,
    '@','n','v','m','e',':','0','/',
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,

    /* Read-only view of the live image's writable memory filesystem. */
    0x01, 0x07, 0,0,0,0,0,0,
    '@','m','e','m',':','0','/',0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0
};
