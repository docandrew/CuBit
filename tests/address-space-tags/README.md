# Address-space tags (x86 PCIDs)

`nix develop -c make -C kernel test-address-space-tags prove-address-space-tags`
and `tests/address-space-tags/mutations.sh` (from the Nix shell).

With CR4.PCIDE set, the TLB tags each translation with the 12-bit PCID of
the address space that made it, and a CR3 load can keep the loaded tag's
translations instead of dropping them. `TLB_Context`
(`kernel/src/tlb_context.ads`, trusted) turns this on when the CPU has both
PCID and INVPCID and the build says `TAGGED_TLB=1` (off by default until it
has run on Intel hardware; see below): the kernel's page tables
load as tag 0, each PID's as tag PID + 1. The rules it applies are
`Address_Space_Tags` (`kernel/src/address_space_tags.ads`, SPARK):

- each tag has a life, advanced (`Next_Life`) whenever its PID gets a new
  address space (`TLB_Context.Begin_Life`, at process creation);
- each CPU remembers, per tag, the life it last loaded the tag for; a load
  keeps the tag's translations only when that is the tag's current life
  (`Keep`); otherwise it drops them (CR3 bit 63 clear);
- removing or downgrading any mapping, kernel or user, drops every tag's
  translations (`TLB_Context.Flush_All` is INVPCID all-contexts, and
  `TLB_Shootdown` runs it on every online CPU), never only the current tag's.

**Proved (GNATprove, level 2):** 87 checks, none unproved or justified, no
`Assume`. `Address_Space_Tags`: the CR3 encoding (root, tag and keep bit
read back as written). `Tag_Protocol` (this directory, test-only) models
three CPUs and three PID slots with the hardware's per-CPU, per-tag cached
translations, and proves:

- the invariant: what a CPU caches under a tag belongs to the life it last
  loaded the tag for, never ahead of the tag's life; a CPU running a slot
  last loaded it for its current life. Every operation keeps it;
- `No_Stale` / `Prove_No_Stale`: a CPU running a slot holds no translation
  of any other life of it (another process that had the PID);
- exit and PID reuse: `Begin_Life` leaves no CPU able to keep the tag's
  translations, and `Prove_Reuse_Drops` (a life runs, every CPU leaves it,
  the PID begins a new life and is loaded on the same CPU) ends with nothing
  cached under the tag. Exit itself needs no shootdown;
- revocation and unmap: `Prove_Shootdown_Forgets`, after a mapping is
  removed and every CPU runs `Flush_All`, no CPU holds it under any tag.

**Mutants** (`mutations.sh`, each must fail to prove; all eight do): keep an
older life's translations, keep any life's, a new address space that does
not advance the life, a CR3 value without the tag, a load that does not
record the life, a load that keeps dropped translations, `Flush_All` as a
CR3 reload (only the running tag), and a new life while a CPU still runs the
old address space.

**Hosted simulation** (`main.adb`): the CR3 encoding at its edges, and one
million random steps over the model with contracts checked at run time,
asserting the invariant and `No_Stale` after every step.

**Native** (`tests/headless/run.sh --test tlb-tags`, two CPUs): `tlb-check`
launches `tlb-probe` twelve times through procmgr. Each life gets the lowest
free PID, so later lives reuse earlier lives' PIDs (10 of 12 in practice)
and with them their tags, and its 1 MiB buffer is at the same address. Each
life checks that every page of its fresh buffer reads zero before writing
its life number into it: an earlier life's translation kept across the
PID's reuse would show that life's data (exit, then reuse). Every second
life maps 16 pages `tlb-check` grants it; a second thread reads them
(caching their translations on its CPU, often the other one: the main
thread spins meanwhile) and waits. The main thread gives the acquisition
back (the pages stay mapped until the owner revokes) and asks `tlb-check`
to revoke, so the kernel unmaps them while `tlb-check`'s address space runs
and shoots down every CPU. The reader's next read must fault and the kernel
kills the life (six kills required): a shootdown that dropped only the
running tag's translations would leave the probe's (revocation, unmap).

This can only catch a stale translation on a CPU that tags them: Intel with
PCID and INVPCID (the laptop, the NUC). The AMD hosts' KVM guests have no
PCID and QEMU's TCG implements neither PCID nor INVPCID (the boot line says
"Address-space tags (PCID) off" on both), so there the test checks the
untagged path and the protocol only. The tagged path (CR4.PCIDE, tagged CR3
loads, INVPCID) has not executed on any machine yet.

**Not proved, required of the kernel (TLB_Context's callers):**
- `Begin_Life` runs before a PID's new address space is first loaded, and
  only after no CPU has the PID's previous address space in CR3 (its
  contexts have stopped and switched away: `Process_Stops`,
  `noteContextStopped`);
- every removal or downgrade of a present mapping is followed by
  `Flush_All` on every CPU that may hold it (`TLB_Shootdown.Invalidate_All`)
  before the page or page table is reused. Kernel mappings too: every
  address space shares the kernel half, so a kernel translation can be
  cached under any tag (`Mem_mgr.forgetKernelTranslations`);
- `Load_Process` and `Load_Kernel` run with interrupts disabled (the
  per-CPU table is the running CPU's);
- the hardware: CR3 bit 63 keeps only the loaded tag's translations, a load
  with bit 63 clear drops them (and the tag's paging-structure caches), and
  INVPCID type 2 drops every tag's (Intel SDM Vol. 3A, 4.10).
