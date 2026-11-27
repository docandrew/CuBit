-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- What may be copied into another process's capability table, and when
-- (CAP-002, docs/capability-handles.md; "CSPACE" in its implementation
-- notes).
--
-- @description
-- Two system calls place a capability in another process's table, and both
-- copy one the caller already holds:
--
-- * INSTALL_CAPABILITY, through a CSPACE capability. SPAWN gives the
--   spawner one CSPACE per child, generation-bound to that child; it admits
--   installs only until the child first runs (Sealed after) and only while
--   the child lives (Target_Gone after).
-- * REPLY_WITH_CAPABILITY, into the process whose request the caller is
--   answering, and only when that request said it accepts a capability (its
--   reply authority carries RIGHT_GRANT).
--
-- The copy is a Derivation of the caller's capability, by the rules of
-- Capability_Derivation.
--
-- Proved (gnatprove, tests/capability-install): rights never widen; a range
-- never widens; a tag is only set on an untagged endpoint; a sealed, stale
-- or mismatched CSPACE admits nothing; a reply without RIGHT_GRANT accepts
-- nothing. Placement itself is Capability_Tables.Insert, proved never to
-- touch a live entry (Capability_Install.Placement composes the two).
-------------------------------------------------------------------------------
pragma Ada_2022;

with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capability_ABI;

package Capability_Install with SPARK_Mode is

    package ABI renames Capability_ABI;

    ---------------------------------------------------------------------------
    -- Admission
    ---------------------------------------------------------------------------
    type Admission (Admitted : Boolean := False) is record
        case Admitted is
            when True  => null;
            when False => Why : ABI.Refusal := ABI.Not_Permitted;
        end case;
    end record;

    -- A CSPACE admits installing into Target while Target is the process
    -- (and incarnation) it names and has not started running.
    function Cspace_Admits
      (Cspace            : Capability;
       Target            : Unsigned_64;
       Target_Generation : Generation;
       Target_Started    : Boolean) return Admission
    with Post =>
        Cspace_Admits'Result.Admitted =
          (Cspace.capType = CAP_CSPACE
           and then Cspace.rights (RIGHT_GRANT)
           and then Cspace.object.ref = Target
           and then Cspace.gen = Target_Generation
           and then not Target_Started);

    -- A reply authority admits a transfer when its request accepted one.
    function Reply_Admits (Reply : Capability) return Admission
    with Post =>
        Reply_Admits'Result.Admitted =
          (Reply.capType = CAP_REPLY and then Reply.rights (RIGHT_GRANT));

end Capability_Install;
