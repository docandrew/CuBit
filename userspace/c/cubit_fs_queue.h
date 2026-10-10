/*
 * CuBit filesystem request queue (cubit-filesystem_queues.ads): the layout
 * a client and the filesystem service share. The client opens three
 * channels on the filesystem endpoint (docs/data-plane.md): the transfer
 * arena, the dirty arena, then the queue pair, whose client region holds
 * the requests and the indices the client writes, and whose service region
 * (granted back, read-only) holds the answers, the service's indices, its
 * wake word, the namespace generation and the delegations. Checked against
 * the Ada by tests/fs-bench/queue-layout-check.py.
 */
#ifndef CUBIT_FS_QUEUE_H
#define CUBIT_FS_QUEUE_H

enum {
	OP_FS_WAKE = 0x0022,
	FS_QUEUE_CONNECTOR = 1,
	FS_TRANSFER_CONNECTOR = 2,
	FS_DIRTY_CONNECTOR = 3,
	FS_EVENT_CONNECTOR = 4,
	FS_SLOT_BITS = 6,
	FS_SLOTS = 64,
	FS_REQUEST_BYTES = 64,
	FS_ANSWER_BYTES = 32,
	FS_PAGE_BYTES = 4096,
	FS_CLIENT_PAGES = 2,
	FS_CLIENT_SUBMITTED_AT = 0,
	FS_CLIENT_REAPED_AT = 64,
	FS_CLIENT_REQUESTS_AT = 4096,
	FS_SERVER_ANSWERED_AT = 0,
	FS_SERVER_TAKEN_AT = 64,
	FS_SERVER_WAKE_AT = 128,
	FS_SERVER_NAMESPACE_AT = 192,
	FS_SERVER_ANSWERS_AT = 4096,
	FS_DELEGATIONS_AT = 8192,
	FS_DELEGATION_BYTES = 32,
	FS_MAXIMUM_DELEGATIONS = 2048,
	FS_DELEGATION_VALID_AT = 0,
	FS_DELEGATION_MODE_AT = 4,
	FS_DELEGATION_INODE_AT = 8,
	FS_DELEGATION_VERSION_AT = 16,
	FS_DELEGATION_SIZE_AT = 24,
	FS_READ_DELEGATION = 1,
	FS_WRITE_DELEGATION = 2,
	FS_DIRTY_ENTRIES = 2048,
	FS_DIRTY_ENTRY_BYTES = 16,
	FS_DIRTY_PAGES_AT = 32768,
	FS_DIRTY_PAGE_BYTES = 4096,
	FS_DIRTY_ARENA_BYTES = 8421376,
	FS_DIRTY_SEQUENCE_AT = 0,
	FS_DIRTY_SLOT_AT = 4,
	FS_TAG_SLOT_BITS = 11,
	FS_TAG_GENERATION_BITS = 21,
	FS_DIRTY_PAGE_AT = 8,
	FS_DIRTY_START_AT = 12,
	FS_DIRTY_STOP_AT = 14,
	FS_QUEUE_OPEN = 1,
	FS_QUEUE_CLOSE = 2,
	FS_QUEUE_READ_AT = 3,
	FS_QUEUE_WRITE_AT = 4,
	FS_QUEUE_FLUSH = 5,
	FS_QUEUE_WRITEBACK = 6,
	FS_QUEUE_UNLINK = 7,
	FS_QUEUE_MKDIR = 8,
	FS_QUEUE_RMDIR = 9,
	/* Directory.Page.V2 pages (cubit-directory_pages.ads). */
	FS_QUEUE_READ_DIRECTORY = 10,
	FS_DIRECTORY_METADATA = 1,
	FS_QUEUE_OPEN_DIRECTORY = 11,
	FS_QUEUE_CLOSE_DIRECTORY = 12,
	FS_QUEUE_PARK = 13,
	/* One Directory.Inspection.V1 record of an open handle (fstat). */
	FS_QUEUE_DESCRIBE = 15,
	/* A file's new size (ftruncate), after the client's earlier writes. */
	FS_QUEUE_RESIZE = 16,
	/* Old then new path at the arena offset; length = both, position = the
	 * old path's length. Rename or move within one volume (as OP_RENAME). */
	FS_QUEUE_RENAME = 17,
	/* Resume a directory listing at a page's resume token (0: start). */
	FS_QUEUE_SEEK_DIRECTORY = 18,
	/* Watch a directory (handle; options FS_WATCH_SUBTREE): events in the
	 * event ring (cubit-filesystem_events.ads). Value: the watch number. */
	FS_QUEUE_WATCH = 19,
	FS_WATCH_SUBTREE = 1,
	/* Ends a watch; its last record is a Watch_Ended. */
	FS_QUEUE_UNWATCH = 20,
	/* The caller's access profile as cubit-file_access.ads wire entries
	 * into the arena range; value = entry count (NO_SPACE: did not fit). */
	FS_QUEUE_LIST_SCOPES = 21,
	/* Path (position = its length) at the range's start; the range
	 * receives one Volume.Description.V1 (cubit-volume_descriptions.ads). */
	FS_QUEUE_DESCRIBE_VOLUME = 22,
	FS_VOLUME_DESCRIPTION_BYTES = 104,
	/* Server-side copy: handle = source, spare 1 = target, position /
	 * arena offset = source / target offsets, length (or FS_COPY_TO_END),
	 * spare 2 = deadline (ms). Answered once, value = bytes copied. */
	FS_QUEUE_COPY = 23,
	/* handle = the copy's token. */
	FS_QUEUE_CANCEL = 24,
	/* Progress: token (u64) then bytes done (u64) per running copy. */
	FS_SERVER_COPIES_AT = 256,
	FS_COPY_ENTRY_BYTES = 16,
	FS_COPY_TOKEN_AT = 0,
	FS_COPY_DONE_AT = 8,
	FS_MAXIMUM_COPIES = 4,
	/* The event ring: bounded records, 16 pages. */
	FS_EVENT_RECORD_BYTES = 4128,
	FS_EVENT_RING_PAGES = 16,
	FS_TOKEN_AT = 0,
	FS_OPERATION_AT = 8,
	FS_OPTIONS_AT = 12,
	FS_HANDLE_AT = 16,
	FS_POSITION_AT = 24,
	FS_LENGTH_AT = 32,
	FS_ARENA_OFFSET_AT = 40,
	FS_STATUS_AT = 8,
	FS_RIGHTS_AT = 12,
	FS_RIGHTS_READ = 1,
	FS_RIGHTS_WRITE = 2,
	/* The policy would let it write the file or create in the directory. */
	FS_RIGHTS_POLICY_WRITE = 4,
	FS_VALUE_AT = 16,
	FS_TRANSFER_PAGES = 256,
};

#define FS_COPY_TO_END (~0ULL)


#endif
