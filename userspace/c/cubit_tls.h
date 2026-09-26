/* C view of tls.svc's IPC protocol. The Ada definition is authoritative:
 * userspace/runtime/gnat/cubit-tls_protocol.ads. Keep the two in step. */
#ifndef CUBIT_TLS_H
#define CUBIT_TLS_H

#define CUBIT_TLS_SERVICE_ROLE 23u

/* OPEN: tag.length = length of "HOST:PORT" at transfer offset 0;
 *   words = {transfer slot, transfer bytes, 0, transfer generation}.
 *   Deferred reply after the handshake: OK {channel} or ERROR {failure}.
 * WRITE: words = {channel, offset, length}. OK {bytes accepted}.
 * READ: words = {channel, offset, max length[, absolute deadline ms]}.
 *   Deferred: OK {length}, EOF after close_notify, or ERROR {failure}.
 * SHUT: words = {channel}. INFO: words = {channel} -> OK {version, suite}. */
#define CUBIT_TLS_OPEN  0x0C01u
#define CUBIT_TLS_WRITE 0x0C02u
#define CUBIT_TLS_READ  0x0C03u
#define CUBIT_TLS_SHUT  0x0C04u
#define CUBIT_TLS_INFO  0x0C05u

#define CUBIT_TLS_REPLY_OK    0xF000u
#define CUBIT_TLS_REPLY_ERROR 0xF001u
#define CUBIT_TLS_REPLY_EOF   0xF006u

/* ERROR word 0. */
enum cubit_tls_failure {
    CUBIT_TLS_MALFORMED_REQUEST = 1,
    CUBIT_TLS_SCOPE_DENIED = 2,
    CUBIT_TLS_UNKNOWN_CHANNEL = 3,
    CUBIT_TLS_QUOTA_EXCEEDED = 4,
    CUBIT_TLS_BUSY = 5,
    CUBIT_TLS_CONNECT_FAILED = 6,
    CUBIT_TLS_TIMEOUT = 7,
    CUBIT_TLS_CLOCK_UNTRUSTED = 8,
    CUBIT_TLS_CERTIFICATE_EXPIRED = 9,
    CUBIT_TLS_CERTIFICATE_UNTRUSTED = 10,
    CUBIT_TLS_CERTIFICATE_REJECTED = 11,
    CUBIT_TLS_PROTOCOL_ALERT = 12,
    CUBIT_TLS_PEER_CLOSED = 13,
    CUBIT_TLS_SERVICE_FAILURE = 14
};

#endif
