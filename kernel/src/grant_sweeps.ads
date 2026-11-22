-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Walks of a process's owned or received grant list (linked
-- through the grant records) that allocate nothing, while each visit may
-- unlink any grant, others on the same list included, link new ones, or
-- start another walk.
--
-- A walk keeps its place in a cursor in its own stack frame: the grant it
-- visits next. Whoever unlinks a grant calls Pass first, which moves every
-- cursor resting on that grant to its successor. The cursors chain through
-- the walks' frames, innermost first, so no walk needs memory and no depth
-- is fixed. A grant linked during a walk goes to the list head, behind
-- every cursor, and that walk does not visit it.
--
-- Process teardown walks these lists exactly when memory may be exhausted
-- (an out-of-memory kill), so a walk must not allocate. The protocol is
-- proved on a model in tests/grant-sweeps (each grant visited at most once
-- per walk; every cursor rests on a listed grant), and this unit is tested
-- there against the model.
--
-- The caller serializes every Walk and Pass (the kernel: grantLock). Not
-- SPARK: the cursor chain points into stack frames.
-------------------------------------------------------------------------------
generic
    type Slot is range <>;
    No_Slot : Slot;
    -- Which list: (process, owned or received) in the kernel.
    type List_Key is private;
package Grant_Sweeps with SPARK_Mode => Off is

    -- Visit every grant on Key's list, head first, as the list is when the
    -- walk reaches it.
    generic
        with function First (Key : List_Key) return Slot;
        with function Next (Key : List_Key; S : Slot) return Slot;
        with procedure Visit (S : Slot);
    procedure Walk (Key : List_Key);

    -- S, followed by Following on Key's list, is about to leave it.
    procedure Pass (Key : List_Key; S, Following : Slot)
      with Pre => S /= No_Slot;

end Grant_Sweeps;
