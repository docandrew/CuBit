# Checked syscall source reads

Run the actual kernel helper algorithms on Linux, with assertions enabled only
in this host executable:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/user-memory/user_memory_tests.gpr && ../tests/user-memory/build/main'
nix develop -c bash -lc 'cd kernel && alr exec -- gnatprove -P ../tests/user-memory/user_memory_tests.gpr -u copy_proof.adb --mode=all --level=1 --checks-as-errors=on -j2'
```

`User_Buffer_Copy` tests cover every 4 KiB starting offset, lengths across
multiple pages, exact source/destination coverage, failure-prefix termination,
null/wrapping/noncanonical ranges, and bounded names. A NUL at the last readable
byte succeeds without probing the next page; a full name truncates without
reading a seventeenth byte.

`User_Page_Walk` runs against synthetic page tables. Tests require present and
user access at every level, reject large/huge mappings without following them
as tables, allow the leaf PAT bit, and reject invalid physical frame geometry.

The focused SPARK harness instantiates the same generic algorithms. Its 36
obligations cover initialization, arithmetic/index safety, loop assertions,
termination and the declared range/result contracts. No assumptions or waived
checks are used. The callback fixtures are not proofs of real physical memory,
allocator pins, page-table lifetime, or SMP serialization.

The kernel adapter in `Process.User_Memory` permits only the executing caller:
ordinary owned 4 KiB frames are ownership-checked and pinned atomically in the
buddy allocator, then copied through the kernel physical alias. Explicit
readable initrd mappings use the permanent multiboot reservation instead;
only bytes inside the image, not surrounding page padding, qualify. Received
grant mappings, other unowned frames, MMIO and large pages are rejected by this
initial interface. The caller's execution pin prevents address-space retirement;
this is not a general remote address-space inspection API.

SPAWN checks the claimed ELF range before length conversion, snapshots its ELF
and program headers, reads needed segment bytes through this adapter, and copies
the bounded process name. A failed segment read rolls back the unpublished
process. Unused ELF bytes need not be mapped. Pins preserve frame lifetime, not
payload immutability: executable authentication still needs an immutable-input
design. Other syscall buffers and shared/remote page-table mutation remain
separate audit work.

The native capability-security regression covers actual initrd service loading,
filesystem-backed ELF reads, invalid ELF geometry and repeated rollback/PID
reuse. It does **not** inject bad pointers into an authorized native SPAWN; those
range/walk/name cases are covered here at the helper level. No new production
test authority or fault-injection syscall was introduced.
