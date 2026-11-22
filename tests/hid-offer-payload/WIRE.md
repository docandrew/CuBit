# HID offer wire v1

Test-local golden wire layout. The independent JS encoder spells numeric
offsets directly; it does not derive them from the Ada implementation.
New operation `CuBit.HID_Offer_Exchange.Offer_Request = 0x0252` on the existing
firmware-data endpoint, requiring its authenticated authority tag. One candidate
per offer. No timing/polling policy and no MMIO address authority in the payload.

Request: three words `[request_id, encoded_readonly_grant_reference, byte_length]`.
Unused word3 and all protocol/IPC/reserved flags zero. Request id nonzero.
Receiver acquires with authenticated sender as expected owner, copies exactly
the declared bytes, returns acquisition, then decodes/decides/records/enqueues.
Receipt key is `(authenticated sender, payload epoch, request_id)`. A repeat
returns the recorded answer; it never re-decides or launches twice.

Reply: four words `[request_id, disposition, detail, queue_position]`, same label
and zero flags/reserved. Dispositions:1 Queued,2 Not_Approved,3 Malformed,
4 Capacity,5 Unknown_Controller,6 Controller_Mismatch,7 GPIO_Not_Admitted,
8 Duplicate_Device,9 Copy_Failed. Detail is a payload field/byte offset for
Malformed and0 otherwise. Queue position is nonzero only for Queued. Queued
means validated+queued, not driver readiness. A canonical device path already
queued for this sender/epoch yields Duplicate_Device for a distinct request.

Sender uses an explicit finite kernel-millisecond deadline (budget at most
1000ms), one same-ID confirmation, immutable page and original startup deadline.
Unknown completion stays unknown; grant retirement alone does not establish a
launch outcome. No child launch/readiness wait or callback query inside the
handler. Deferred devmgr loop owns launch and driver lifetime.

## Payload

All integer fields are little-endian. No C/Ada record layout is put on the wire.
The fixed header is552 bytes. Exact length is552 plus the four raw span lengths;
maximum1576 bytes. A zero-filled dedicated read-only page is sufficient.
Reserved fields and unused NameSeg slots must be zero.

| Offset | Bytes | Field |
| ---: | ---: | --- |
|0|4|Version=1|
|4|4|Exact payload byte length|
|8|8|ACPI incarnation epoch, nonzero; same epoch as accepted GPIO record|
|16|8|Catalog revision, provenance only|
|24|8|Observation sequence, nonzero; provenance only|
|32|8|Evaluated `_SEG`, or zero when absent|
|40|8|Evaluated host-bridge `_BBN`, or zero when absent|
|48|8|Evaluated controller `_ADR`, or zero when absent|
|56|4|I2C maximum Hz, nonzero|
|60|2|I2C address|
|62|2|HID descriptor register selector (zero preserved)|
|64|2|GPIO pin|
|66|1|I2C descriptor revision,1 or2|
|67|1|I2C type revision|
|68|1|I2C source index|
|69|1|PCI known bits: bit0 SEG, bit1 BBN, bit2 ADR; higher bits zero|
|70|2|Original I2C type-specific flags, preserved|
|72|1|I2C resource role:0 consumer required|
|73|1|I2C initiator:0 controller required|
|74|1|I2C sharing:0 exclusive,1 shared|
|75|1|I2C addressing:0 seven-bit,1 ten-bit|
|76|1|GPIO interrupt:0 level,1 edge|
|77|1|GPIO polarity:0 high,1 low,2 both (both requires edge)|
|78|1|GPIO interrupt sharing:0 exclusive,1 shared|
|79|1|Wake capability:0 no,1 yes|
|80|1|GPIO role:0 consumer required|
|81|1|Pin configuration, preserved|
|82|2|Drive strength, hundredths mA, preserved|
|84|2|Debounce, hundredths ms, preserved|
|86|1|GPIO UID kind:0 none,1 integer,2 string|
|87|1|Device canonical path segment count,1..32|
|88|1|I2C-controller canonical path segment count,1..32|
|89|1|GPIO-controller canonical path segment count,1..32|
|90|2|Raw I2C source length,0..256|
|92|2|Raw I2C vendor length,0..256|
|94|2|Raw GPIO source length,0..256|
|96|2|Raw GPIO vendor length,0..256|
|98|2|Reserved zero|
|100|8|GPIO UID, integer or8 little-endian packed string bytes;0 for none|
|108|8|GPIO HID printable ASCII, NUL padded, at least one character|
|116|4|Defining device table signature, printable ASCII|
|120|8|Defining device OEM table ID, ASCII20h..7Eh|
|128|8|Defining table fingerprint, diagnostic only (0 if unavailable)|
|136|4|Automatic initializer invocation count, diagnostic only|
|140|1|Observation phase=3, after device initialization|
|141|3|Reserved zero|
|144|8|Ordered initializer fingerprint, diagnostic only|
|152|8|Copied resource-template fingerprint, diagnostic only (0 if unavailable)|
|160|8|Matched compatible ID: ASCII `PNP0C50` followed by NUL|
|168|128|Device path:32 fixed4-byte NameSeg slots|
|296|128|I2C-controller path, same format|
|424|128|GPIO-controller path, same format|
|552|variable|Raw I2C source, I2C vendor, GPIO source, GPIO vendor, in that order|

NameSegs are from the namespace root, without root/separator characters: first
character A-Z/underscore, remaining characters additionally0-9. Unused slots
are all NUL. No path truncation. GPIO UID kind and opaque64-bit value exactly
match the accepted GPIO payload: no UID requires0, integer and string kinds
retain all64 bits without additional text interpretation or narrowing.
Raw source/vendor spans preserve every byte, including NUL; no reinterpretation.

PCI fields preserve presence independently of their value: evaluated bus0 is
different from absent `_BBN`. Absent fields must encode0. Known SEG<=65535,
BBN<=255; known ADR is device*65536+function with device<=31/function<=7 and no
other bits. Any known-bit mask0..6 is a well-formed unresolved observation and
must be refused as Unknown_Controller; only mask7 supplies a complete BDF.
There are no guessed defaults. Address<=127 in seven-bit mode,<=1023 in ten-bit.

The codec validates shape/canonical fields, not controller admission. Receiver
checks its PCI inventory/class/driver manifest and accepted GPIO record for the
same authenticated sender/epoch plus HID/UID/canonical path. Fingerprints, table
provenance and observation sequence confer no authority or freshness proof.
Native publication must re-observe/serialize its source facts before offering.

On malformed input no partial row escapes. The diagnostic offset denotes the
rejected field start (or offending path/text byte); structural/version/length
checks precede field checks. Malformed spans are reported at their length field.
