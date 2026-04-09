/*---------------------------------------------------------------------------
 * CuBit logstore - Package Identity
 *
 * Declares package identity in .cubit.id ELF section.
 *
 * Keys:
 *   id      = com.cubit.logstore
 *   version = 1.0.0
 *---------------------------------------------------------------------------*/

static const unsigned char __cubit_id[]
    __attribute__((section(".cubit.id"), used)) = {
    /* Header: magic "CBID" LE, version 1, count 2 */
    0x43, 0x42, 0x49, 0x44,     /* magic */
    0x01, 0x00,                 /* version */
    0x02, 0x00,                 /* count */

    /* Entry 0: id = "com.cubit.logstore" */
    0x02,                       /* keyLen = 2 */
    0x12, 0x00,                 /* valLen = 18 */
    'i', 'd',                   /* key */
    'c', 'o', 'm', '.', 'c', 'u', 'b', 'i', 't', '.', 'l', 'o', 'g',
    's', 't', 'o', 'r', 'e',

    /* Entry 1: version = "1.0.0" */
    0x07,                       /* keyLen = 7 */
    0x05, 0x00,                 /* valLen = 5 */
    'v', 'e', 'r', 's', 'i', 'o', 'n',
    '1', '.', '0', '.', '0'
};
