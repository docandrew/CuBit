/*---------------------------------------------------------------------------
 * CuBit sleep - Package Identity
 *
 * Declares package identity in .cubit.id ELF section.
 *
 * Keys:
 *   id      = com.cubit.sleep
 *   version = 1.0.0
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,     /* magic */
    0x01, 0x00,                 /* version */
    0x02, 0x00,                 /* count */

    /* Entry 0: id = "com.cubit.sleep" */
    0x02,                       /* keyLen = 2 */
    0x0F, 0x00,                 /* valLen = 15 */
    'i', 'd',                   /* key */
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 's', 'l', 'e', 'e', 'p',

    /* Entry 1: version = "1.0.0" */
    0x07,                       /* keyLen = 7 */
    0x05, 0x00,                 /* valLen = 5 */
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};

/*---------------------------------------------------------------------------
 * CuBit sleep - Stream Declarations
 *
 * Declares I/O streams in .cubit.streams ELF section.
 *
 * Streams:
 *   STDOUT (id=2, 4 pages, TYPE_TEXT)
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_streams[]
    __attribute__((section(".cubit.streams"), used)) = {
    /* Header: magic "CBST" LE, version 1, count 1 */
    0x43, 0x42, 0x53, 0x54,     /* magic */
    0x01, 0x00,                 /* version */
    0x01, 0x00,                 /* count */

    /* Entry 0: streamID=2(STDOUT), pages=4, typeTag=1(TEXT), flags=0 */
    0x02, 0x00, 0x04, 0x00, 0x01, 0x00, 0x00, 0x00
};
