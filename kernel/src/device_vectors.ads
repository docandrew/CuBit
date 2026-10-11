-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The device interrupt vector allocator (docs/msi-interrupts.md).
--
-- @description
-- Every IDT vector in Device_Vector_ABI's device range is Free, Bound to one
-- owner (the process identity of the driver whose interrupt capability names
-- it) or Retired (its owner is gone, but the device may still hold the
-- vector until the device manager masks it and releases the block).
--
-- Proved (gnatprove level 2, tests/msi-vectors):
--  * no double assignment: Allocate binds only vectors that were Free, so a
--    vector never passes from one owner to another without a Release;
--  * a Retired vector is never handed out again until it is released;
--  * Allocate finds a block whenever one fits (first fit, complete);
--  * Release frees exactly one block, Retire touches only its owner's
--    vectors, and Recipient never names a retired owner.
--
-- Pure table operations, no locking: Device_Interrupts holds the one live
-- table under its spinlock.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Device_Vector_ABI;

package Device_Vectors with
    SPARK_Mode => On
is
    subtype Vector is Device_Vector_ABI.Device_Vector;
    subtype Block_Size is Device_Vector_ABI.Block_Size;

    -- The owner's process identity word (Process_Identities.To_Word).
    type Owner_Key is new Unsigned_64;
    NO_OWNER : constant Owner_Key := 0;

    type Vector_State is (Free, Bound, Retired);

    -- Every member of a block records the block's first vector and size.
    type Vector_Entry is record
        State : Vector_State := Free;
        Owner : Owner_Key    := NO_OWNER;
        Head  : Vector       := Vector'First;
        Count : Block_Size   := 1;
    end record;

    FREE_ENTRY : constant Vector_Entry :=
      (State => Free, Owner => NO_OWNER, Head => Vector'First, Count => 1);

    type Vector_Table is array (Vector) of Vector_Entry;
    EMPTY_TABLE : constant Vector_Table := [others => FREE_ENTRY];

    ---------------------------------------------------------------------------
    -- A free vector has no owner; a bound or retired one has exactly one.
    ---------------------------------------------------------------------------
    function Consistent (T : Vector_Table) return Boolean is
      (for all V in Vector => (T (V).State = Free) = (T (V).Owner = NO_OWNER));

    -- Whether Count vectors from Head stay inside the device range.
    function Fits (Head : Vector; Count : Block_Size) return Boolean is
      (Count - 1 <= Vector'Last - Head);

    function In_Block (Head : Vector; Count : Block_Size; V : Vector)
      return Boolean is
      (V >= Head and then V - Head < Count);

    function All_Free (T : Vector_Table; Head : Vector; Count : Block_Size)
      return Boolean is
      (for all V in Vector => (if In_Block (Head, Count, V) then T (V).State = Free));

    -- Head names a live (bound or retired) block.
    function Is_Head (T : Vector_Table; Head : Vector) return Boolean is
      (T (Head).State /= Free and then T (Head).Head = Head);

    -- V belongs to the live block that starts at Head.
    function Member (T : Vector_Table; Head, V : Vector) return Boolean is
      (T (V).State /= Free and then T (V).Head = Head);

    -- Who an interrupt on V is delivered to: only a bound owner.
    function Recipient (T : Vector_Table; V : Vector) return Owner_Key is
      (if T (V).State = Bound then T (V).Owner else NO_OWNER);

    ---------------------------------------------------------------------------
    -- Allocate
    -- Bind Count contiguous free vectors to Owner, first fit. Contiguous
    -- blocks let one capability (first vector, count) name them all.
    ---------------------------------------------------------------------------
    type Allocation_Status is (Allocated, Exhausted, Invalid_Owner);

    procedure Allocate
      (T      : in out Vector_Table;
       Owner  : Owner_Key;
       Count  : Block_Size;
       Head   : out Vector;
       Status : out Allocation_Status)
    with
        Pre  => Consistent (T),
        Post => Consistent (T)
          and then (Status = Invalid_Owner) = (Owner = NO_OWNER)
          and then
            (case Status is
               when Allocated =>
                 Fits (Head, Count)
                 and then All_Free (T'Old, Head, Count)
                 and then (for all V in Vector =>
                             (if In_Block (Head, Count, V)
                              then T (V) = (State => Bound, Owner => Owner,
                                            Head => Head, Count => Count)
                              else T (V) = T'Old (V))),
               when Exhausted =>
                 T = T'Old
                 and then (for all H in Vector =>
                             (if Fits (H, Count) then not All_Free (T, H, Count))),
               when Invalid_Owner =>
                 T = T'Old);

    ---------------------------------------------------------------------------
    -- Release
    -- Free the block that starts at Head, bound or retired. The device
    -- manager calls this only after masking the vectors at the device.
    ---------------------------------------------------------------------------
    type Release_Status is (Released, Not_A_Block);

    procedure Release
      (T      : in out Vector_Table;
       Head   : Vector;
       Status : out Release_Status)
    with
        Pre  => Consistent (T),
        Post => Consistent (T)
          and then (Status = Released) = Is_Head (T'Old, Head)
          and then
            (if Status = Released then
               (for all V in Vector =>
                  (if Member (T'Old, Head, V) then T (V) = FREE_ENTRY
                   else T (V) = T'Old (V)))
             else T = T'Old);

    ---------------------------------------------------------------------------
    -- Retire
    -- Owner is gone: stop delivering to it, but keep its vectors reserved
    -- until the device manager has masked them and releases each block.
    ---------------------------------------------------------------------------
    procedure Retire (T : in out Vector_Table; Owner : Owner_Key)
    with
        Pre  => Consistent (T) and then Owner /= NO_OWNER,
        Post => Consistent (T)
          and then (for all V in Vector =>
                      (if T'Old (V).State = Bound and then T'Old (V).Owner = Owner
                       then T (V) = (T'Old (V) with delta State => Retired)
                       else T (V) = T'Old (V)))
          and then (for all V in Vector => Recipient (T, V) /= Owner);
end Device_Vectors;
