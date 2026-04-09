/*---------------------------------------------------------------------------
 * CuBit wget - Package Identity
 *
 * Declares package identity in .cubit.id ELF section.
 *
 * Keys:
 *   id      = com.cubit.wget
 *   version = 1.0.0
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,     /* magic */
    0x01, 0x00,                 /* version */
    0x02, 0x00,                 /* count */

    /* Entry 0: id = "com.cubit.wget" */
    0x02,                       /* keyLen = 2 */
    0x0E, 0x00,                 /* valLen = 14 */
    'i', 'd',                   /* key */
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 'w', 'g', 'e', 't',

    /* Entry 1: version = "1.0.0" */
    0x07,                       /* keyLen = 7 */
    0x05, 0x00,                 /* valLen = 5 */
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};

/*---------------------------------------------------------------------------
 * CuBit wget - Stream Declarations
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

/*---------------------------------------------------------------------------
 * CuBit wget - Capability Manifest
 *
 * Declares required capabilities in .cubit.caps ELF section.
 *
 * Entries:
 *   slot 11 - CAP_ENDPOINT to netstack (DRIVER_NETSTACK = 3)
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 1 */
    0x54, 0x49, 0x42, 0x43,     /* magic */
    0x01, 0x00,                 /* version */
    0x01, 0x00,                 /* count */

    /* Entry 0: REQ_SERVICE, RW, slot 11, driver_id=3 (DRIVER_NETSTACK) */
    0x02, 0x03, 0x0B, 0x00,     /* type, rights, slot, reserved */
    0x03, 0x00, 0x00, 0x00,     /* param0 = DRIVER_NETSTACK */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   /* param1 */
};
