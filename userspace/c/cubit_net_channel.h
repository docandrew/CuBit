/*
 * CuBit stream network channels for C: the shared layout and the ring entry
 * points. Used by the libc (copied in by userspace/libc/build.sh) and by
 * NetSurf's fetcher. Mirrors userspace/runtime/gnat/
 * cubit-net_channel_layout.ads (checked by tests/channel-rings/run.sh) and
 * the C entry points of the proved CuBit.Channel_Rings
 * (userspace/runtime/gnat/cubit-channel_rings_c.ads).
 */
#ifndef CUBIT_NET_CHANNEL_H
#define CUBIT_NET_CHANNEL_H

#include <stdint.h>

/* Inside the libc these are internal symbols. */
#ifndef hidden
#define hidden
#endif

enum {
	NET_HEADER_BYTES = 4096,
	/* The client's line. */
	NET_TX_PRODUCED_AT = 0,
	NET_RX_CONSUMED_AT = 4,
	NET_WANT_AT = 8,
	NET_SHUT_WRITE_AT = 12,
	NET_TX_SIZE_AT = 16,
	NET_RX_SIZE_AT = 20,
	NET_WAIT_BIT_AT = 24,
	/* Netstack's line. */
	NET_TX_CONSUMED_AT = 64,
	NET_RX_PRODUCED_AT = 68,
	NET_KICK_WANTED_AT = 72,
	NET_STATUS_AT = 76,
	NET_TARGET_AT = 256,
	NET_TARGET_MAXIMUM = 255,

	NET_WANT_READABLE = 1,
	NET_WANT_WRITABLE = 2,
	NET_KICK_ON_SEND = 1,
	NET_KICK_ON_RECEIVE = 2,

	NET_STATUS_OPENING = 0,
	NET_STATUS_OPEN = 1,
	NET_STATUS_PEER_FINISHED = 2,
	NET_STATUS_RESET = 3,
	NET_STATUS_TIMED_OUT = 4,
	NET_STATUS_UNREACHABLE = 5,
	NET_STATUS_PROTOCOL_ERROR = 6,

	NET_MAXIMUM_WAIT_BIT = 63,
	NET_DATAGRAM_MAXIMUM = 1472,
	OP_NET_WAIT = 0x0428,
	OP_NET_KICK = 0x0429,
	NET_END_WAIT = 1,
	OP_NET_ARENA = 0x042A,
	OP_NET_ARENA_RELEASE = 0x042B,
	NET_MAXIMUM_ARENA_BUFFERS = 1024,
	OP_NET_SCOPE = 0x042C,
	NET_OFFER_BYTES = 12,
	NET_OFFER_ARENA_AT = 0,
	NET_OFFER_BUFFER_AT = 8,
	NET_ARRIVAL_BYTES = 40,
	NET_ARRIVAL_CHANNEL_AT = 0,
	NET_ARRIVAL_ARENA_AT = 8,
	NET_ARRIVAL_BUFFER_AT = 16,
	NET_ARRIVAL_PORT_AT = 20,
	NET_ARRIVAL_ADDRESS_AT = 24,
	NET_ADDRESS_BYTES = 16,
};

/* {size, own, count}: a producer's own index is what it produced and count
 * its fill; a consumer's own index is what it consumed and count what is
 * available. */
struct cubit_ring {
	uint32_t size, own, count;
};

hidden int __cubit_ring_accept_consumed(struct cubit_ring *, uint32_t);
hidden int __cubit_ring_accept_produced(struct cubit_ring *, uint32_t);
hidden void __cubit_ring_free_slices(const struct cubit_ring *, uint32_t *,
	uint32_t *, uint32_t *);
hidden void __cubit_ring_data_slices(const struct cubit_ring *, uint32_t *,
	uint32_t *, uint32_t *);
hidden int __cubit_ring_commit(struct cubit_ring *, uint32_t);
hidden int __cubit_ring_consume(struct cubit_ring *, uint32_t);
hidden int __cubit_ring_valid_size(uint32_t);
/* Datagram records (CuBit.Datagram_Rings): put is 1 if put, 0 if no room,
 * -1 if too large; take is 1 if taken, 0 if empty, -1 if malformed. */
hidden int __cubit_datagram_put(struct cubit_ring *, void *ring,
	const void *data, uint32_t length);
hidden int __cubit_datagram_take(struct cubit_ring *, const void *ring,
	void *into, uint32_t room, uint32_t *length, int *truncated);

#endif
