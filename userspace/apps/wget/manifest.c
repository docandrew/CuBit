/*---------------------------------------------------------------------------
 * CuBit wget - Capability Manifest
 *
 * Declares required capabilities in .cubit.caps ELF section.
 * The process manager reads this section and mints capabilities
 * into the spawned process.
 *
 * Entries:
 *   slot 11 - CAP_ENDPOINT to netstack (DRIVER_NETSTACK = 3)
 *   stream  - STDOUT (stream ID 0x02, 4 pages, TYPE_TEXT)
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 2 */
    0x54, 0x49, 0x42, 0x43,     /* magic */
    0x01, 0x00,                 /* version */
    0x02, 0x00,                 /* count */

    /* Entry 0: REQ_SERVICE, RW, slot 11, driver_id=3 (DRIVER_NETSTACK) */
    0x02, 0x03, 0x0B, 0x00,     /* type, rights, slot, reserved */
    0x03, 0x00, 0x00, 0x00,     /* param0 = DRIVER_NETSTACK */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* param1 */

    /* Entry 1: REQ_STREAM, W, slot 0, stream=STDOUT(0x02), pages=4 */
    0x08, 0x02, 0x00, 0x00,     /* type=REQ_STREAM, rights=W, slot=0, rsv */
    0x02, 0x00, 0x04, 0x00,     /* param0: lo16=streamId(2), hi16=pages(4) */
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   /* param1: typeTag=1 */
};
