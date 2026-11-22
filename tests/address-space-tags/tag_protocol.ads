-------------------------------------------------------------------------------
-- Tag_Protocol: a proved model of address-space tags (x86 PCIDs) on a few
-- CPUs and process slots, using the kernel's rules (Address_Space_Tags).
-- Test-only; not kernel code.
--
-- Hardware, as far as stale translations go: each CPU caches, under each
-- tag, the translations of at most one life of that tag's slot (Cached;
-- No_Life when it holds none). A CR3 load of a tag either keeps them or
-- drops them; Flush_All drops every tag's (INVPCID all contexts). Running
-- a slot (Use) caches translations of the slot's current life.
--
-- The kernel (TLB_Context): each slot's life advances when it gets a new
-- address space (Begin_Life), which happens only when no CPU runs it; a
-- load keeps a tag's translations only when Keep says this CPU last loaded
-- the tag for its current life (Loaded); removing a mapping from a live
-- address space (Remove) is followed by Flush_All on every CPU (Shootdown)
-- before the page is reused.
--
-- Invariant: whatever a CPU caches under a tag belongs to the life it last
-- loaded the tag for, which is never ahead of the slot's life; a CPU
-- running a slot last loaded it for its current life. Theorems: a CPU
-- running a slot holds no translation of another life of it (No_Stale,
-- Prove_No_Stale); a new life is never loaded with translations kept
-- (Begin_Life's post, Prove_Reuse_Drops); after a shootdown no CPU holds a
-- removed mapping under any tag (Prove_Shootdown_Forgets).
-------------------------------------------------------------------------------
with Address_Space_Tags; use Address_Space_Tags;

package Tag_Protocol with SPARK_Mode => On is

    type CPU is range 0 .. 2;
    subtype Slot is Process_Tag range Process_Tag'First .. Process_Tag'First + 2;
    -- What a CPU runs: the kernel's tables or a slot's.
    subtype Running is Tag range Kernel_Tag .. Slot'Last;

    type Life_Table is array (Slot) of Life;
    type CPU_Lives is array (CPU, Slot) of Life;
    type CPU_Flags is array (CPU, Slot) of Boolean;
    type Active_Table is array (CPU) of Running;

    type State is record
        Lives   : Life_Table := (others => No_Life);
        -- The kernel's per-CPU memory of the life it last loaded a tag for.
        Loaded  : CPU_Lives := (others => (others => No_Life));
        -- Hardware: the life whose translations a CPU holds under a tag.
        Cached  : CPU_Lives := (others => (others => No_Life));
        -- Hardware: a CPU may still hold a translation of a removed mapping
        -- under the tag.
        Removed : CPU_Flags := (others => (others => False));
        Active  : Active_Table := (others => Kernel_Tag);
    end record;

    function Inv (S : State) return Boolean is
      ((for all C in CPU =>
          (for all T in Slot =>
             (S.Cached (C, T) = No_Life or else S.Cached (C, T) = S.Loaded (C, T))
             and then S.Loaded (C, T) <= S.Lives (T)
             and then (if S.Removed (C, T) then S.Cached (C, T) /= No_Life)))
       and then
       (for all C in CPU =>
          (if S.Active (C) in Slot then
             S.Lives (S.Active (C)) /= No_Life and then
             S.Loaded (C, S.Active (C)) = S.Lives (S.Active (C)))));

    -- The safety property: a CPU running a slot holds no translation of
    -- any other life of it.
    function No_Stale (S : State) return Boolean is
      (for all C in CPU =>
         (if S.Active (C) in Slot then
            S.Cached (C, S.Active (C)) = No_Life or else
            S.Cached (C, S.Active (C)) = S.Lives (S.Active (C))));

    function Runs_Nowhere (S : State; T : Slot) return Boolean is
      (for all C in CPU => S.Active (C) /= T);

    procedure Prove_No_Stale (S : State)
    with
        Ghost,
        Global => null,
        Pre  => Inv (S),
        Post => No_Stale (S);

    -- The slot gets a new address space (no CPU runs it): no CPU's memory
    -- of the tag lets a load keep translations.
    procedure Begin_Life (S : in out State; T : Slot)
    with
        Global => null,
        Pre  => Inv (S) and then Runs_Nowhere (S, T) and then
                S.Lives (T) < Life'Last,
        Post => Inv (S) and then S.Lives (T) = Next_Life (S'Old.Lives (T)) and then
                (for all C in CPU => not Keep (S.Loaded (C, T), S.Lives (T))) and then
                S.Cached = S'Old.Cached and then S.Active = S'Old.Active;

    -- Load a slot's page tables on a CPU (TLB_Context.Load_Process).
    procedure Load (S : in out State; C : CPU; T : Slot; Kept : out Boolean)
    with
        Global => null,
        Pre  => Inv (S) and then S.Lives (T) /= No_Life,
        Post => Inv (S) and then No_Stale (S) and then S.Active (C) = T and then
                Kept = Keep (S'Old.Loaded (C, T), S.Lives (T)) and then
                (if not Kept then S.Cached (C, T) = No_Life) and then
                S.Lives = S'Old.Lives and then
                (for all D in CPU =>
                   (if D /= C then S.Active (D) = S'Old.Active (D)));

    -- Load the kernel's page tables (one life, never replaced).
    procedure Load_Kernel (S : in out State; C : CPU)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Active (C) = Kernel_Tag and then
                S.Lives = S'Old.Lives and then
                (for all D in CPU =>
                   (if D /= C then S.Active (D) = S'Old.Active (D)));

    -- The CPU runs what it loaded and caches its translations.
    procedure Use_Translations (S : in out State; C : CPU)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Lives = S'Old.Lives and then
                S.Active = S'Old.Active;

    -- A mapping of a live slot is removed from its page tables: any CPU
    -- caching the slot's translations may still hold it.
    procedure Remove (S : in out State; T : Slot)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Lives = S'Old.Lives and then
                S.Active = S'Old.Active and then
                (for all C in CPU =>
                   S.Removed (C, T) = (S.Cached (C, T) /= No_Life));

    -- INVPCID all contexts on one CPU: every tag's translations go.
    procedure Flush_All (S : in out State; C : CPU)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Lives = S'Old.Lives and then
                S.Active = S'Old.Active and then
                (for all T in Slot => S.Cached (C, T) = No_Life and then
                                      not S.Removed (C, T)) and then
                (for all D in CPU =>
                   (if D /= C then
                      (for all T in Slot =>
                         S.Cached (D, T) = S'Old.Cached (D, T) and then
                         S.Removed (D, T) = S'Old.Removed (D, T))));

    -- TLB_Shootdown.Invalidate_All: Flush_All on every CPU.
    procedure Shootdown (S : in out State)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Lives = S'Old.Lives and then
                S.Active = S'Old.Active and then
                (for all C in CPU =>
                   (for all T in Slot =>
                      S.Cached (C, T) = No_Life and then not S.Removed (C, T)));

    -- PID reuse: a CPU ran a slot's life, the slot ended (no shootdown at
    -- exit) and began a new life; loading the slot again on that CPU keeps
    -- nothing.
    procedure Prove_Reuse_Drops (S : in out State; C : CPU; T : Slot)
    with
        Ghost,
        Global => null,
        Pre  => Inv (S) and then S.Lives (T) /= No_Life and then
                S.Lives (T) < Life'Last,
        Post => Inv (S) and then S.Cached (C, T) = No_Life and then
                S.Active (C) = T;

    -- Revocation: after Remove and a shootdown, no CPU holds the removed
    -- mapping under any tag, wherever each CPU runs.
    procedure Prove_Shootdown_Forgets (S : in out State; T : Slot)
    with
        Ghost,
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then
                (for all C in CPU => (for all U in Slot => not S.Removed (C, U)));

end Tag_Protocol;
