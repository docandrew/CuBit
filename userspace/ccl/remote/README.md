# Development control wire profile v1

The transport-independent `CCL.Control` dispatcher exchanges typed Ada records.
`Control_Wire` validates and encodes the CBOR boundary; `Control_HTTP` handles
the bounded development HTTP envelope. The native `Control_Transport` owns
grant-backed networking. The browser is only another client, not an evaluator.

Operations 1–3 use one definite CBOR array `[1, requestId, operation, source]`.
Operations 4–6 use `[1, requestId, operation, source, targetGeneration]`.
Request IDs are nonzero uint64 values, echoed for correlation, **not authority
or replay protection**. Source is at most 1024 ASCII bytes (printable plus tab,
CR, LF); it must be empty except for evaluation or starting a monitor.
The target is nonzero for stop and zero for start/inspect. No maps, floats, byte strings,
tags, references, indefinite lengths, noncanonical integers, or trailing items.

| Operation | Response array |
| --- | --- |
| 1 Inspect bindings | `[1,id,1,pid,networkPid,clockPid,clockAvailable,monotonicMs,digest0,digest1,digest2,digest3]` |
| 2 Evaluate expression | `[1,id,2,ok,displayText,typeCode,diagnosticPosition,fuelRemaining]` |
| 3 Read clock | `[1,id,3,clockAvailable,monotonicMs]` |
| 4 Start monitor | `[1,id,4,accepted,state,generation,runs,intervalMs,source,ok,displayText,typeCode,diagnosticPosition,fuelRemaining,nextDeadline]` |
| 5 Stop monitor | Same 15-field shape, operation 5 |
| 6 Inspect monitor | Same 15-field shape, operation 6 |

Monitor state codes: Empty 0, Waiting 1, Executing 2, Stopping 3, Stopped 4,
Faulted 5. There is one shared lab-owned slot; starting an active slot is
rejected without replacing its source. Stop requires its current generation.
Generation protects against stale actions within this host lifetime, not
forgery, replay across restarts, or unauthorized clients. The host fixes the
interval at 1000 ms and fuel at 4096. Inspect does not create or reload a
program. Source/result remain inspectable after stop; reboot clears the slot.

Integers are unsigned uint64 on the wire; display text carries the existing CCL
result image (including signed values); a record or payload variant result
travels as its canonical literal in the display text with type code 0
(operation 7 presents it typed). Type codes: invalid/no scalar 0,
Integer 1, Boolean 2, String 3, Character 4, List 5, Function 6 (a function
value, described by the display text only).

### Lists

An evaluation whose result is a list has three more fields (11 in all):
`[1,id,2,ok,displayText,5,diagnosticPosition,fuelRemaining,elementType,elements,total]`.
`total` is the list's full length (at most 4096). `elements` carries its first
`min(total, 64)` items; a list of strings may carry fewer when their text
exceeds 1024 bytes. The display text ends with `... N more` when shortened.
- `elementType` is Integer 1, Boolean 2, String 3, Character 4, or
  enumeration 6 (a member's position).
- `elements` is one definite array of at most 64 items of that type.
  - Integer elements are signed: negative values use CBOR major type 1.
    This is the only place the profile admits negative integers.
  - Characters are one-byte text strings.
- The shape is valid only with type code 5; the browser rejects any mismatch
  between the type code, the field count or an element's type.
- Monitor responses keep their 15 fields. A list result there has type code 5
  and is carried by the display text only. Diagnostic positions are 1-based,
zero if absent. Fuel is at most 16,777,216 (`CCL.Sessions.Maximum_Fuel`); evaluations start
with 1,000,000 and monitors with 4096. Responses are at most 8192 bytes, text at
most 4096 ASCII bytes. CBOR is not a trust boundary substitute: the browser
validates shape, field types, limits, version, operation and request ID again.

The HTTP lab profile accepts only POST/OPTIONS `/ccl` with HTTP/1.1, Host
`127.0.0.1:18445` and Origin `http://127.0.0.1:8787`. POST requires
`application/cbor` and Content-Length 1..1100. Headers are limited to 2048
bytes; input storage is 4096 bytes. Duplicate critical headers, transfer
encoding, Expect, encoding/upgrade/trailer headers, folded/malformed headers,
and already-buffered excess bytes are rejected. One request per connection;
no keep-alive/pipelining. A fixed five-second deadline covers all request reads.

Origin checks are browser cross-origin defenses, not authentication. Trusted
local clients may forge them. TLS, admission and authenticated discovery remain
future work; this endpoint must remain in an isolated loopback-only lab.

CBOR implementation: unmodified `cbor_ada` 0.3.0, Copyright Baris Erdem, at the revision pinned by
`flake.nix` / `flake.lock`, Apache-2.0. Its license is in the pinned source's
`LICENSE` file, reproduced as `CBOR_LICENSE.txt` here. The native app build also
stages it at `/licenses/cbor_ada.txt` in the ISO tree. It is linked into this
**userspace** app, never the kernel.
The profile excludes floats; upstream float-proof gaps remain recorded in
`docs/ccl-cbor-evaluation.md` and are not claimed resolved here.

### Presentations and images

Operations 7 and 8 serve the Observatory's REPL transcript. It renders a
result exactly as the native CCL console does, from the same
`CCL.Presentations` description.

**Requests:**

- **7, present:** `[1,id,7,source]`. It evaluates like operation 2.
- **8, image rows:** `[1,id,8,"",imageId,firstRow]`.
  - `imageId` is nonzero.
  - `firstRow` is below 512, the store's largest side.

**Response 7:** `[1,id,7,ok,typeText,valueText,form,diagnosticPosition,fuelRemaining,detail]`.

| form | meaning | detail |
| --- | --- | --- |
| 0 failure | `valueText` is the diagnostic | `[]` |
| 1 text | the value as text, its type in `typeText` | `[]` |
| 2 table | a record or a list of records | `[rowType, many, total, [[field, typeName, numeric]...], rows]` |
| 3 picture | an `Image` (interfaces/image.schema) | `[width, height, imageId]` |
| 4 gallery | a list of `Image`s | `[total, [[width, height, imageId]...]]` (`valueText` empty) |

- **Tables:**
  - `rows` holds as many whole rows as fit in the response; `total` counts them all.
  - Each row is one canonical CCL literal per field, so a click can write it back into source.
  - A table's `valueText` is empty: its rows are the value.
- **Pictures:**
  - `imageId` is the content digest of the pixels in the guest's image store (`CCL.Image_Store`).
  - It grants nothing: it names pixels that the guest itself produced.

**Operation 9, present monitor:** request `[1,id,9,"",0]`. It responds with operation 7's fields for the periodic program's last result, then the program's state and completed runs: `[1,id,9,ok,typeText,valueText,form,position,fuel,detail,state,runs]`. The web console's `:watch` observes a live cell this way: the program runs natively, and the page only reads its result.

**Operation 10, complete:** request `[1,id,10,source]`, where `source` is the text before the caret. It responds with `[1,id,10,prefixLength,[[name,origin,signature]...],beyond,signature]`, from the guest catalog's `CCL.Completions`, which is the native console's completion.
- `origin` is 0 for a service operation, 1 for a built-in, and 2 for a form.
- The last `signature` is the called operation's, when the caret is in a call's arguments, and is empty otherwise.

**Response 8:** `[1,id,8,known,width,height,firstRow,rowCount,rgb]`.

- `rgb` is a **byte string**, three bytes a pixel. This is the only byte string the profile admits.
- It carries `rowCount` whole rows from `firstRow`, as many as fit in 8192 bytes.
- An id no longer in the store answers `known = false` with zeros and empty bytes. It is never answered with other pixels.
- Ids are content digests, so a client may cache rows by id for as long as it likes.

Unlike the other operations, these responses nest arrays (at most four deep).

**What is proved and what is tested:**

- `Control_Wire` decodes and validates both requests, and is proved at level 2 with the rest of the codec (`make prove-ccl-remote`).
- `Control_Presentation` encodes both responses. The presentation model and the image store are not SPARK units, so these encoders are **tested, not proved**:
  - `tests/ccl-remote/wire_vectors.adb` encodes real evaluations.
  - `make test-ccl-remote` compares its bytes with `tools/ccl-observatory/wire-vectors.json`.
  - The browser's decoder must read that file as the console shows it (`wire.test.mjs`).
