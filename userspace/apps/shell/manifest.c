/*---------------------------------------------------------------------------
 * CuBit Shell - Capability Manifest
 *
 * Declares required capabilities in .cubit.caps ELF section.
 * The process manager reads this section and mints capabilities
 * into the spawned process.
 *
 * Entries:
 *   slot 4  - CAP_DEVICE_MEM (framebuffer)
 *   slot 1  - CAP_ENDPOINT to FS server (DRIVER_FS = 6)
 *   slot 12 - CAP_ENDPOINT to procmgr (DRIVER_PROCMGR = 4)
 *   slot 11 - CAP_ENDPOINT to netstack (DRIVER_NETSTACK = 3)
 *   slot 20 - CAP_ENDPOINT to config (DRIVER_CONFIG = 11)
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_manifest[]
    __attribute__((section(".cubit.caps"), used)) = {
    /* Header: magic "CBIT" LE, version 1, count 5 */
    0x54, 0x49, 0x42, 0x43,     /* magic */
    0x01, 0x00,                 /* version */
    0x05, 0x00,                 /* count */

    /* Entry 0: REQ_FRAMEBUFFER, RW, slot 4 */
    0x01, 0x03, 0x04, 0x00,     /* type, rights, slot, reserved */
    0x00, 0x00, 0x00, 0x00,     /* param0 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* param1 */

    /* Entry 1: REQ_SERVICE, RW, slot 1, driver_id=6 (DRIVER_FS) */
    0x02, 0x03, 0x01, 0x00,     /* type, rights, slot, reserved */
    0x06, 0x00, 0x00, 0x00,     /* param0 = DRIVER_FS */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* param1 */

    /* Entry 2: REQ_SERVICE, RW, slot 12, driver_id=4 (DRIVER_PROCMGR) */
    0x02, 0x03, 0x0C, 0x00,     /* type, rights, slot, reserved */
    0x04, 0x00, 0x00, 0x00,     /* param0 = DRIVER_PROCMGR */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* param1 */

    /* Entry 3: REQ_SERVICE, RW, slot 20, driver_id=11 (DRIVER_CONFIG) */
    0x02, 0x03, 0x14, 0x00,     /* type, rights, slot, reserved */
    0x0B, 0x00, 0x00, 0x00,     /* param0 = DRIVER_CONFIG */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* param1 */

    /* Entry 4: REQ_SERVICE, RW, slot 11, driver_id=3 (DRIVER_NETSTACK) */
    0x02, 0x03, 0x0B, 0x00,     /* type, rights, slot, reserved */
    0x03, 0x00, 0x00, 0x00,     /* param0 = DRIVER_NETSTACK */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   /* param1 */
};
