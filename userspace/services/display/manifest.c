/*---------------------------------------------------------------------------
 * CuBit display.svc - Package Identity and Capabilities
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,
    0x01, 0x00,
    0x02, 0x00,

    /* Entry 0: id = "com.cubit.display" */
    0x02,
    0x11, 0x00,
    'i', 'd',
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 'd', 'i', 's', 'p', 'l', 'a', 'y',

    /* Entry 1: version = "0.1.0" */
    0x07,
    0x05, 0x00,
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '0', '.', '1', '.', '0'
};

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 3 */
    0x54, 0x49, 0x42, 0x43,
    0x01, 0x00,
    0x03, 0x00,

    /* Entry 0: REQ_FRAMEBUFFER, RW, slot 4 */
    0x01, 0x03, 0x04, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* Entry 1: REQ_IOPORT, R, slot 8, base port 0x03DA (VGA status)
     * Slot 8 avoids procmgr's reserved post-spawn bootstrap slots.
     */
    0x03, 0x01, 0x08, 0x00,
    0xDA, 0x03, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    /* Entry 2: REQ_SERVICE, RW, slot 9, driver_id=17 (DRIVER_GPU)
     * Optional at runtime: display.svc falls back to linear-fb if missing.
     */
    0x02, 0x03, 0x09, 0x00,
    0x11, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
