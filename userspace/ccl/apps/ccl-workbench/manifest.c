/* CuBit CCL Workbench identity and explicitly requested service authorities. */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    0x02,
    0x17, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'c', 'c', 'l', '-', 'w', 'o', 'r', 'k', 'b', 'e', 'n', 'c', 'h',

    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x03, 0x00,

    /* REQ_SERVICE, read/write, slot 21, driver 15 (desktop.svc). */
    0x02, 0x03, 0x15, 0x00,
    0x0F, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,

    /* REQ_SERVICE, read/write, slot 25, driver 19 (clock.svc). */
    0x02, 0x03, 0x19, 0x00,
    0x13, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,

    /* Filesystem endpoint in slot 1; object access is restricted below. */
    0x02, 0x03, 0x01, 0x00,
    0x06, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
};

/* Transitional explicit work roots, not wildcard FS or policy-admin power. */
static const struct {
    unsigned char header[16];
    /* ELF access entries are 80 bytes; the FS IPC decoder uses 72 bytes. */
    struct {
        unsigned char rights, length, reserved[6], path[64], padding[8];
    } scopes[2];
} __cubit_access __attribute__((section(".cubit.access"), used)) = {
    .header = {0x43, 0x41, 0x43, 0x43, 1, 0, 2, 0},
    .scopes = {
        { .rights = 0x0b, .length = 12, .path = "@nvme:0/work" },
        { .rights = 0x0b, .length = 11, .path = "@mem:0/work" }
    }
};
_Static_assert(sizeof(__cubit_access) == 16 + 2 * 80, "access manifest layout");
