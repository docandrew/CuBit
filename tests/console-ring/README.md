# Console ring (asynchronous kernel console)

`kernel/src/console_ring.ad[sb]` is the bounded byte FIFO behind TextIO's
serial output (docs/development-backlog.md, "Console output is synchronous
serial I/O"). A print copies its bytes into the ring under the output lock;
idle CPUs write it to the UART in batches of at most 16 bytes (the 16550A
transmit FIFO), one `rep outsb` per batch, only when the transmit holding
register is empty. CPU 0's timer writes one batch per tick after
`Console_Stall_Ticks` ticks without drain progress (no CPU idle). Nothing is
lost: a writer that finds the ring full writes its oldest batch itself, as
every print did before. Fatal stops (`Last_Chance_Handler`, the panic vector,
kernel exceptions) call `TextIO.stopAsynchronous`, which writes everything
queued and makes later output direct. Output before the first idle thread
runs is direct.

**Proved** (SPARK level 2, `prove.gpr`): every index and count is in range;
`Put` appends exactly one byte and keeps the queued ones; `Take` removes
`min (Length, 16)` of the oldest bytes, returns them in order, and keeps the
rest in order.

**Hosted** (`ring_tests`): 2,000,000 random puts and batch takes against a
reference FIFO, the second half write-heavy so the ring fills and the writer
pays (about 47,000 full-ring batches); every byte comes out in order.

**Not proved**: the TextIO locking/ownership protocol (output lock, then the
`transmitting` flag; interrupts masked while a batch is owned), the UART and
the idle/timer drains. Native evidence is the QEMU measurement in
coordination/desktop-1hz-stall.md.

```sh
cd kernel
alr exec -- gnatprove -P ../tests/console-ring/prove.gpr --mode=all --level=2 \
  --report=fail --checks-as-errors=on
alr exec -- gprbuild -p -P ../tests/console-ring/ring_tests.gpr
../tests/console-ring/build/test/ring_tests
```
