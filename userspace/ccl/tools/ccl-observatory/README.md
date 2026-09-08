# CCL observatory work in progress

This browser client is unfinished and has not been end-to-end tested. It has
no synthetic topology or simulated service replies.

The initial local-relay transport was abandoned in favor of native inbound TCP
support in CuBit. The experimental relay has been removed. The frontend and
`ccl-control` application's transport code still need migration to the native
listener/HTTP adapter; their old relay protocol references are not a runnable
or supported connection path.

See `docs/network-inbound-implementation.md` for current status and the security
admission prerequisites. Do not expose the unfinished CCL transport publicly.
