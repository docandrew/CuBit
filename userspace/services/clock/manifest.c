/* Identity for the CuBit monotonic clock service. */
static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    0x02,
    0x0f, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'c', 'l', 'o', 'c', 'k',

    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};
