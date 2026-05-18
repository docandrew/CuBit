/*---------------------------------------------------------------------------
 * CuBit Security Center - Package Identity and Capabilities
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    /* Entry 0: id = "com.cubit.security-center" */
    0x02,
    0x19, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 's', 'e', 'c', 'u', 'r', 'i', 't', 'y', '-', 'c', 'e', 'n', 't', 'e', 'r',

    /* Entry 1: version = "0.1.0" */
    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 1 */
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x01, 0x00,

    /* Entry 0: REQ_FRAMEBUFFER, RW, slot 4 */
    0x01, 0x03, 0x04, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
