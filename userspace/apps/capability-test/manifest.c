/* CuBit capability non-amplification regression app.
 *
 * The empty capability manifest is intentional. The process must receive no
 * service endpoint or capability-construction authority merely by starting.
 */

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    0x02, 0x12, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'c', 'a', 'p', '-', 't', 'e', 's', 't',

    0x07, 0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x00, 0x00
};
