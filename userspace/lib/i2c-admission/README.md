# I2C channel and transfer wire codecs

Pure shared data codec for launcher-issued i2c channels and driver admission.
It does not mint capabilities or authenticate a caller. The driver decodes only
the KERNEL-STAMPED authorityTag of its admitted endpoint, never a message-word
substitute. Endpoint identity selects the controller instance; badge data selects
that channel's target/addressing/max speed. The domain tag is not a service-role
number: connect binding name is i2c; catalog role allocation remains separate.

Numeric64bit format, version1:

| Bits | Field |
|---|---|
|63..48|domain0x4932 (I2)|
|47..44|version1|
|43|reserved, zero|
|42|0 seven-bit,1 ten-bit|
|41..32|address,0..1023; seven-bit requires0..127|
|31..0|max frequency in Hz, unsigned32, nonzero|

No address/speed truncation, implicit default, or unused-bit alias. Total Encode
returns Invalid_Target on a bad typed target. Total Decode returns a typed reason
for wrong domain/version, reserved encoding, zero speed or invalid7bit address.
Canonical is a representation helper, not an admission decision; consumers use
Encode/Decode status. Callers validate raw firmware integers before converting
to the named Address_Value range. Mode/speed support, reserved/general-call
address policy, timing feasibility, pin/target rights and deadlines belong to
launcher/driver admission, not this representation. Current controller support
may reject ten-bit or high/low speeds; the codec preserves those facts faithfully.

Golden value: seven-bit0x15 at400000Hz =0x4932101500061A80. A decoded target is
well formed, re-encodes to the exact original word, and every valid typed target
survives encode/decode unchanged. Proof covers ALL typed target/word values,
not just the hosted vectors. No imported callback/pragmaAssume is used here.

Current level-2 milestone:134 scoped SPARK checks across the three codecs,
CuBit.Grant_References and the root package (99prover/35flow), with no unproved,
justified or pragmaAssume checks. Runtime safety and declared contracts are
proved. This is not a proof of unspecified transfer round-trip semantics,
endpoint minting/stamping, stateful ownership, volatile access or whole ACPI.
Tests in tests/i2c-admission cover1545346checks/profile checked/release, including
the actual driver-result conversion and timing-aware finite work policy.

I2C_Transfer_Wire defines request226 and completion227 with protocol version1 in
the message header. Requests carry exactly four words: canonical input/output
grant reference identities (zero when unused), packed16bit write/read counts
with the high32bits reservedzero, and an explicit deadline. Combined capacity
must be nonzero and fit the admitted buffer. Within is absolute GETTIME
milliseconds; only the all-ones word represents Forever. Runtime clocks and
deadlines are checked separately from representation. Target/speed come from
the kernel-stamped badge; payloads contain no target, speed or work quota.

Completion word0 carries explicit status in its low8bits, hardware-success in
bit8 and effects-possible in bit9; all higher bits are zero. Word1 is the full
64bit ticket. Word2 packs16bit requested-write/delivered-read counts. Word3 is
reservedzero. The write count is not evidence of bytes acknowledged on the bus.
Processing completion is distinct from hardware success and output delivery;
clients inspect the typed status and flags. Unknown statuses/reserved fields
are refused, not truncated. Header, arbitrary array origins, count/deadline
boundaries and canonical references are tested. Length comparisons are made in
64bit words to avoid narrowing a full-range array length to Natural.

I2C_Configure_Replies is retained solely as four-word startup diagnostic data
for logs/tests: version1, typed status, startup stage and unsigned32 sample.
Ready has no failure fields and encodes zero diagnostic words. Accepted frames
re-encode exactly and typed diagnostic round trips are part of its contracts.
Physical quiescence and effects classifications come from retained owner
evidence; these pure values cannot establish that a device is safe.

The old two-word startup codec and saved configure-reply wrapper are removed.
Startup uses the existing Generic_PCI launch and generic readiness report;
there is no configure RPC or separate I2C launch kind. Neither wire decoding
nor a badge creates authority: native copies acquire each exact grant reference
with the kernel-stamped sender as expected owner. Never derive addresses from
grant slots or accept payload words as caller identity.
