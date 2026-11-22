# CCL Interface Descriptors

Status: initial canonical format for the discoverable-authority foundation.

## Purpose

A CCL interface descriptor gives a service's externally visible operations a
stable, content-addressed identity. The descriptor describes types and
authority requirements; it does not confer authority. A process must receive a
catalog view before it can discover an interface and must separately possess an
authorized endpoint or session before it can invoke an operation.

Compiler linkage records pin the interface version, operation ordinal, and
SHA-256 digest. A loader must reject linkage when the resolved descriptor does
not match that identity.

## Canonical text format version 1

The canonical representation is UTF-8 restricted to printable ASCII plus LF.
It has no byte-order mark, trailing spaces, blank lines, comments, or CR bytes.
Every record, including `end-interface`, ends in one LF byte. Keywords and enum
values are lowercase except for the fixed first-line magic. Unsigned integers
use minimal decimal notation with no sign or leading zeroes, except for zero
itself.

The records occur in this exact order:

```text
CCL-INTERFACE-DESCRIPTOR 1
name <byte-length>:<interface-name>
version <major> <minor>
operations <count>
operation <zero-based-ordinal>
name <byte-length>:<operation-name>
parameters <count>
argument <value-kind>
result <value-kind>
authority <authority-class>
ownership-argument <true-or-false>
transfer <transfer-mode>
cancellation <cancellation-mode>
success-verb <ordinal>
failure-verb <ordinal>
cancel-verb <ordinal>
end-operation
... repeated operation records ...
end-interface
```

Operation ordinals must be consecutive and operations must appear in ordinal
order. Version 1 supports the scalar value kinds `integer` and `boolean`, the
authority classes `none`, `observe`, `control`, `secret-use`, and `network`, and
the transfer modes `copy`, `move`, `borrowed-ro`, and `borrowed-rw`.
Cancellation modes are `not-cancellable`, `best-effort`, and
`guaranteed-request`.

The `parameters` field is currently zero or one because CCLB v2 host imports
carry one scalar argument. For a zero-parameter operation, `argument` must be
`integer`, `ownership-argument` must be `false`, and `transfer` must be `copy`.
These fields describe CCLB v2's temporary zero sentinel and will disappear when
the bytecode format has a `unit` value.

The digest is SHA-256 over the complete canonical byte sequence. It is stored
as four consecutive unsigned 64-bit words in network byte order. A descriptor
digest never includes runtime-local data such as a VM import slot, driver ID,
process ID, endpoint, handle, or host binding. Those values are resolved after
descriptor validation and authority admission.

## Shared definitions and future stream profiles

The target source of truth describes semantic operations once, with explicit
[embedded/published exposure](ccl-interactive-composition.md#one-interface-definition-explicit-exposure)
and [call/stream contracts](typed-ipc.md#calls-and-streams-share-one-interface-model).
Generate the CCL signatures and local/IPC adapters from that definition rather
than maintaining separate, divergent APIs. Exposure policy never grants an
endpoint or reveals a descriptor to an unauthorized caller.

The scalar format above is not yet an encoding for the complete stream model.
Delivery, backpressure, ownership and lifetime profiles need an explicit,
versioned canonical representation before tooling may claim compatibility.
Transport-specific layouts/profiles can require distinct descriptor identities;
sharing one semantic definition does not justify ignoring digest mismatches or
pretending a local borrowed buffer can be serialized unchanged over a network.

## Clock descriptor

The initial example is
[`clock.ccl-interface`](../userspace/ccl/interfaces/clock.ccl-interface). Its
digest is
`7dea174599ce1fb1a09cb4f81b3de54c7e67c846742b4022f76c60389b9678d2`
and can be reproduced with:

```sh
sha256sum userspace/ccl/interfaces/clock.ccl-interface
```

The Workbench may expose this descriptor because its host policy grants that
catalog view. Clock remains absent from the CCL parser, type checker, compiler,
and VM; the hosted adapter maps the resolved operation to its local monotonic
clock implementation.

The checked, generated-style Ada representation is `CCL.Interfaces.Clock`.
Both the Linux Workbench and the native CuBit VM host publish that same value
into the catalog they are authorized to expose. The CuBit host then compiles
`(clock.monotonic-ms)` as unresolved linkage and installs its local Clock
endpoint binding only during trusted admission. This package is temporary
scaffolding for the future descriptor compiler; it keeps descriptor identity
out of the language implementation without pretending that static inclusion is
runtime discovery.

The Clock build also embeds the exact canonical descriptor bytes in the
non-loadable ELF section `.cubit.interfaces`. This section is covered by future
whole-binary signatures but is never mapped into the service address space.
Its SHA-256 digest is byte-for-byte identical to the generated-style binding.
The next catalog milestone is to have launch admission validate this section
and publish an authority-filtered view through a dedicated broker. Until then,
the section is an advertisement artifact, not an ambient discovery channel.
