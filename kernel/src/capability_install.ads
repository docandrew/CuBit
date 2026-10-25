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
-- The copy is a Derivation of the caller's capability: the same kind,
-- object, generation and tag, with at most the source's rights; a range
-- capability may be narrowed to a subrange, a scheduling capability to a
-- smaller share, and an untagged endpoint may be given a tag (badging, as
-- in seL4). The source must carry RIGHT_GRANT. Reply and CSPACE
-- capabilities are never copied.
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
with Realtime_Admission;
with Device_Vector_ABI;

package Capability_Install with SPARK_Mode is

    package ABI renames Capability_ABI;
    package RA renames Realtime_Admission;

    ---------------------------------------------------------------------------
    -- Kinds
    ---------------------------------------------------------------------------

    -- Kinds a derivation may copy into another table. Never a reply (one-use
    -- authority of one thread), a CSPACE (scoped to its spawner), a
    -- resource quota (retired) or a hardware catalog entry.
    function Transferable (Kind : CapabilityType) return Boolean is
        (Kind in CAP_ENDPOINT | CAP_NOTIFICATION | CAP_IOPORT | CAP_IRQ |
                 CAP_PROCESS | CAP_DEVICE_MEM | CAP_SCHEDULING |
                 CAP_FIRMWARE_SNAPSHOT);

    -- Kinds whose object is a range (first, count or bytes): device memory
    -- (base, bytes), I/O ports (first port, count), interrupt vectors
    -- (first vector, count), notification roles (first role, count).
    function Range_Kind (Kind : CapabilityType) return Boolean is
        (Kind in CAP_DEVICE_MEM | CAP_IOPORT | CAP_IRQ | CAP_NOTIFICATION);

    ---------------------------------------------------------------------------
    -- Requests (decoded from ABI.Derivation, Source excluded)
    ---------------------------------------------------------------------------
    type Request is record
        Rights  : CapabilityRights := NO_RIGHTS;
        Set_Tag : Boolean := False;
        Tag     : Authority_Tag := NO_AUTHORITY_TAG;
        Narrow  : Boolean := False;
        Base    : Unsigned_64 := 0;
        Length  : Unsigned_64 := 0;
    end record;

    function Rights_Of (Mask : Unsigned_64) return CapabilityRights is
        ((RIGHT_READ    => (Mask and ABI.Right_Read) /= 0,
          RIGHT_WRITE   => (Mask and ABI.Right_Write) /= 0,
          RIGHT_EXECUTE => (Mask and ABI.Right_Execute) /= 0,
          RIGHT_GRANT   => (Mask and ABI.Right_Grant) /= 0,
          RIGHT_REVOKE  => (Mask and ABI.Right_Revoke) /= 0));

    function Mask_Of (Rights : CapabilityRights) return Unsigned_64 is
        ((if Rights (RIGHT_READ) then ABI.Right_Read else 0) or
         (if Rights (RIGHT_WRITE) then ABI.Right_Write else 0) or
         (if Rights (RIGHT_EXECUTE) then ABI.Right_Execute else 0) or
         (if Rights (RIGHT_GRANT) then ABI.Right_Grant else 0) or
         (if Rights (RIGHT_REVOKE) then ABI.Right_Revoke else 0));

    -- The derivation words (but Source) are well formed: known rights and
    -- flag bits, a nonzero tag when one is set, no range unless narrowing.
    function Well_Formed (Words : ABI.Derivation) return Boolean is
        ((Words (ABI.Rights) and not ABI.All_Rights) = 0
         and then (Words (ABI.Flags) and not ABI.Derivation_Flags) = 0
         and then (if (Words (ABI.Flags) and ABI.Set_Tag) /= 0
                   then Words (ABI.Tag) /= NO_AUTHORITY_TAG
                   else Words (ABI.Tag) = NO_AUTHORITY_TAG)
         and then (if (Words (ABI.Flags) and ABI.Narrow) = 0
                   then Words (ABI.Base) = 0 and then Words (ABI.Length) = 0));

    function Decode (Words : ABI.Derivation) return Request is
        ((Rights  => Rights_Of (Words (ABI.Rights)),
          Set_Tag => (Words (ABI.Flags) and ABI.Set_Tag) /= 0,
          Tag     => Words (ABI.Tag),
          Narrow  => (Words (ABI.Flags) and ABI.Narrow) /= 0,
          Base    => Words (ABI.Base),
          Length  => Words (ABI.Length)))
    with Pre => Well_Formed (Words);

    ---------------------------------------------------------------------------
    -- Derivation
    ---------------------------------------------------------------------------

    -- A scheduling capability's (budget, period) in Realtime_Admission's
    -- ranges.
    function Valid_Share (Budget, Period : Unsigned_64) return Boolean is
        (Budget <= Unsigned_64 (RA.Microseconds'Last)
         and then Period in 1 .. Unsigned_64 (RA.Microseconds'Last));

    -- Child (budget, period) is no larger a share than Parent's.
    function Share_Covered (Child, Parent : ObjectRef) return Boolean is
        (Valid_Share (Child.ref, Child.param)
         and then Valid_Share (Parent.ref, Parent.param)
         and then RA.Covers (RA.Microseconds (Parent.ref),
                             RA.Microseconds (Parent.param),
                             RA.Microseconds (Child.ref),
                             RA.Microseconds (Child.param)));

    -- Copy is a derivation of Source: same kind and generation; at most
    -- Source's rights; the same object, or a narrower one (a subrange, a
    -- smaller share); the same tag, unless Source is an untagged endpoint.
    function Is_Derivation (Copy, Source : Capability) return Boolean is
        (Copy.capType = Source.capType
         and then Copy.gen = Source.gen
         and then isSubsetOf (Copy.rights, Source.rights)
         and then (Copy.object = Source.object
                   or else (Range_Kind (Source.capType)
                            and then isSubrangeOf (Copy.object, Source.object))
                   or else (Source.capType = CAP_SCHEDULING
                            and then Share_Covered (Copy.object, Source.object)))
         and then (Copy.authorityTag = Source.authorityTag
                   or else (Source.capType in CAP_ENDPOINT | CAP_NOTIFICATION
                            and then Source.authorityTag = NO_AUTHORITY_TAG)));

    -- Device-vector capabilities (the device manager's vector authority, a
    -- driver's allocated block) are made by ALLOCATE_DEVICE_VECTORS only and
    -- never copied: a copy could name vectors bound to someone else.
    function Names_Device_Vectors (Item : ObjectRef) return Boolean is
        (Item.ref <= Unsigned_64 (Device_Vector_ABI.Last_Vector)
         and then (Item.param = 0
                   or else Item.ref + (Item.param - 1) >=
                           Unsigned_64 (Device_Vector_ABI.First_Vector)))
    with Pre => Item.param = 0 or else Item.param - 1 <= Unsigned_64'Last - Item.ref;

    -- Source may be copied at all: a transferable kind, not a device-vector
    -- capability, and carrying RIGHT_GRANT (the right to pass it on).
    function Copyable (Source : Capability) return Boolean is
        (Transferable (Source.capType)
         and then (if Source.capType = CAP_IRQ then
                      Source.object.param > 0
                      and then Source.object.param - 1 <=
                               Unsigned_64'Last - Source.object.ref
                      and then not Names_Device_Vectors (Source.object))
         and then Source.rights (RIGHT_GRANT));

    -- The narrower object Item asks for is inside Source's.
    function Narrowing_Within (Source : Capability; Item : Request) return Boolean is
        ((Range_Kind (Source.capType)
          and then isSubrangeOf ((ref => Item.Base, param => Item.Length),
                                 Source.object))
         or else (Source.capType = CAP_SCHEDULING
                  and then Share_Covered ((ref => Item.Base, param => Item.Length),
                                          Source.object)));

    -- Everything Item asks of Source is allowed.
    function Derivable (Source : Capability; Item : Request) return Boolean is
        (Copyable (Source)
         and then isSubsetOf (Item.Rights, Source.rights)
         and then (if Item.Set_Tag then
                      Source.capType in CAP_ENDPOINT | CAP_NOTIFICATION
                      and then Source.authorityTag = NO_AUTHORITY_TAG
                      and then Item.Tag /= NO_AUTHORITY_TAG)
         and then (if Item.Narrow then Narrowing_Within (Source, Item)));

    -- The copy Item asks for (meaningful when Derivable).
    function Copy_Of (Source : Capability; Item : Request) return Capability is
        ((capType      => Source.capType,
          rights       => Item.Rights,
          authorityTag => (if Item.Set_Tag then Item.Tag else Source.authorityTag),
          object       => (if Item.Narrow
                           then (ref => Item.Base, param => Item.Length)
                           else Source.object),
          gen          => Source.gen));

    type Outcome (Accepted : Boolean := False) is record
        case Accepted is
            when True  => Copy : Capability;
            when False => Why  : ABI.Refusal := ABI.Invalid_Argument;
        end case;
    end record;

    -- The copy Item asks of Source, or why not.
    function Derive (Source : Capability; Item : Request) return Outcome
    with Post =>
        Derive'Result.Accepted = Derivable (Source, Item)
        and then (if Derive'Result.Accepted then
                     Derive'Result.Copy = Copy_Of (Source, Item)
                     and then Is_Derivation (Derive'Result.Copy, Source));

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
