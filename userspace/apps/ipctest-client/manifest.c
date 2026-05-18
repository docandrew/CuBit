/* CuBit async IPC test client identity and service capability request */

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    /* Entry 0: id = "com.cubit.ipctest.client" */
    0x02,
    0x18, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.',
    'i', 'p', 'c', 't', 'e', 's', 't', '.', 'c', 'l', 'i', 'e', 'n', 't',

    /* Entry 1: version = "1.0.0" */
    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 1 */
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x01, 0x00,

    /* Entry 0: REQ_SERVICE, RW, slot 18, driver_id=14 (DRIVER_IPCTEST) */
    0x02, 0x03, 0x12, 0x00,
    0x0E, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
