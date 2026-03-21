/*---------------------------------------------------------------------------
 * CuBit Shell - Filesystem Access Declaration
 *
 * Declares required filesystem paths in .cubit.access ELF section.
 * The process manager reads this section and sends ACL entries to the
 * FS server.
 *
 * The shell is a general-purpose file browser, so it gets wildcard
 * READ access (prefixLen=0) for FS and RW access for config.
 *---------------------------------------------------------------------------*/
#include "../../c/cubit.h"

static const unsigned char __cubit_access[]
    __attribute__((section(".cubit.access"), used)) = {
    /* Header (16 bytes): magic "CACC" LE, version 1, count 1,
     * uid=0, gid=0, trustFloor=0, reserved=0 */
    0x43, 0x41, 0x43, 0x43,             /* magic */
    0x01, 0x00,                         /* version */
    0x02, 0x00,                         /* count */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    0x00,                               /* trustFloor */
    0x00, 0x00, 0x00,                   /* reserved */

    /* Entry 0 (80 bytes): FS READ wildcard (prefixLen=0, service=0) */
    CUBIT_ACL_READ,                     /* rights */
    0,                                  /* prefixLen = 0 (wildcard) */
    CUBIT_SERVICE_FS,                   /* service = 0 (FS) */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix (unused) */
    0,0,0,0,0,0,0,0,                   /* reserved64 */

    /* Entry 1 (80 bytes): Config RW wildcard (prefixLen=0, service=1) */
    CUBIT_ACL_READ | CUBIT_ACL_WRITE,   /* rights */
    0,                                  /* prefixLen = 0 (wildcard) */
    CUBIT_SERVICE_CONFIG,               /* service = 1 (config) */
    0x00,                               /* reserved */
    0x00, 0x00,                         /* uid */
    0x00, 0x00,                         /* gid */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   /* 64 bytes prefix (unused) */
    0,0,0,0,0,0,0,0                    /* reserved64 */
};
