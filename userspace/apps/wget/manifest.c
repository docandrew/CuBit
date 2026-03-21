/*---------------------------------------------------------------------------
 * CuBit wget - Capability Manifest
 *
 * Declares required capabilities in .cubit.caps ELF section.
 * The process manager reads this section and mints capabilities
 * into the spawned process.
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
