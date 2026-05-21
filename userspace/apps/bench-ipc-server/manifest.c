/* CuBit IPC benchmark server identity */

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    /* Entry 0: id = "com.cubit.bench.ipc.server" */
    0x02,
    0x1A, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'b', 'e', 'n', 'c', 'h', '.', 'i', 'p', 'c', '.', 's', 'e', 'r', 'v', 'e', 'r',

    /* Entry 1: version = "1.0.0" */
    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};
