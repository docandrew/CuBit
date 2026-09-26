-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  Pure policy for shared-memory ownership and derived loans.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Memory_Grants with
    Pure,
    SPARK_Mode => On
is
    Page_Size              : constant Unsigned_64 := 4096;
    Maximum_Page_Count     : constant Positive := 4096;
    Maximum_Process_Count  : constant Positive := 256;
    Grants_Per_Process     : constant Positive := 16;
    Grant_Slot_Bytes       : constant Unsigned_64 :=
        Unsigned_64 (Maximum_Page_Count) * Page_Size;

    Received_Region_First  : constant Unsigned_64 := 16#0000_4000_0000_0000#;
    Received_Region_Limit  : constant Unsigned_64 :=
        Received_Region_First +
        Unsigned_64 (Maximum_Process_Count) *
        Unsigned_64 (Grants_Per_Process) * Grant_Slot_Bytes;

    subtype Page_Count is Positive range 1 .. Maximum_Page_Count;
    subtype Page_Offset is Natural range 0 .. Maximum_Page_Count - 1;

    subtype Process_Index is Natural range 0 .. Maximum_Process_Count - 1;
    subtype Local_Slot is Natural range 0 .. Grants_Per_Process - 1;
    subtype Global_Slot is Natural range
      0 .. Maximum_Process_Count * Grants_Per_Process - 1;

    type Grant_Generation is mod 2 ** 32;
    Initial_Generation : constant Grant_Generation := 1;
    subtype Live_Grant_Generation is Grant_Generation range
      Initial_Generation .. Grant_Generation'Last;

    type Reference is record
        slot       : Global_Slot;
        generation : Live_Grant_Generation;
    end record;

    function Owner_Of (slot : Global_Slot) return Process_Index is
      (slot / Grants_Per_Process);
    function Local_Slot_Of (slot : Global_Slot) return Local_Slot is
      (slot mod Grants_Per_Process);

    function Make_Global_Slot
      (owner : Process_Index;
       slot  : Local_Slot) return Global_Slot is
      (owner * Grants_Per_Process + slot)
      with Post =>
        Owner_Of (Make_Global_Slot'Result) = owner and then
        Local_Slot_Of (Make_Global_Slot'Result) = slot;

    --  Zero is never a live generation. Generation exhaustion retires the
    --  identity rather than wrapping and making an ancient reference current.
    procedure Advance_Generation
      (value    : in out Live_Grant_Generation;
       reusable :    out Boolean)
      with Post =>
        (if value'Old < Grant_Generation'Last then
             reusable and then value = value'Old + 1
         else
             not reusable and then value = value'Old);

    --  Generations are namespaced by the owning process's life
    --  (docs/threads.md): the high 16 bits are the process generation, the
    --  low 16 bits count grants within that life, starting at 1. Every reuse
    --  of a process ID starts in a fresh range, so a reference made for an
    --  earlier process never matches, without keeping per-ID state after the
    --  process record is freed.
    Process_Generation_Limit : constant := 2 ** 16 - 1;
    subtype Process_Generation is Unsigned_32 range 0 .. Process_Generation_Limit;

    function Life_Base (Life : Process_Generation) return Live_Grant_Generation is
      (Grant_Generation (Life) * 2 ** 16 + 1);

    function Life_Ceiling (Life : Process_Generation) return Live_Grant_Generation is
      (Grant_Generation (Life) * 2 ** 16 + (2 ** 16 - 1));

    --  The last generation of the life a generation belongs to (its high half).
    function Ceiling_Of (G : Live_Grant_Generation) return Live_Grant_Generation is
      ((G / 2 ** 16) * 2 ** 16 + (2 ** 16 - 1))
      with Post => Ceiling_Of'Result >= G and then
                   Ceiling_Of'Result / 2 ** 16 = G / 2 ** 16;

    --  Advance within one life. At the ceiling the slot is retired for this
    --  life (not reusable) instead of stepping into the next life's range.
    procedure Advance_Generation_Within
      (value    : in out Live_Grant_Generation;
       ceiling  : Live_Grant_Generation;
       reusable :    out Boolean)
      with Pre  => value <= ceiling,
           Post =>
             value <= ceiling and then
             (if value'Old < ceiling then
                  reusable and then value = value'Old + 1
              else
                  not reusable and then value = value'Old);

    --  Different lives never share a generation.
    procedure Prove_Lives_Disjoint (Earlier, Later : Process_Generation)
      with Ghost,
           Pre  => Earlier < Later,
           Post => Life_Ceiling (Earlier) < Life_Base (Later);

    --  References use two explicit wire fields. Keeping slot and generation
    --  separate avoids a hidden packing ABI and makes validation mandatory at
    --  the Unsigned_64 boundary.
    function Is_Valid_Generation_Field (value : Unsigned_64) return Boolean is
      (value >= Unsigned_64 (Initial_Generation) and then
       value <= Unsigned_64 (Grant_Generation'Last));

    function To_Live_Generation
      (value : Unsigned_64) return Live_Grant_Generation
      with Pre  => Is_Valid_Generation_Field (value),
           Post => Unsigned_64 (To_Live_Generation'Result) = value;

    function Is_Current
      (value      : Reference;
       generation : Grant_Generation) return Boolean is
      (value.generation = generation);

    type Permission is (Borrowed_Read_Only, Borrowed_Read_Write);

    --  Ordinary grants contain only owner pages. Received grant mappings must
    --  use the future explicit derivation operation so parentage is retained.
    function Overlaps_Received_Region
      (firstByte : Unsigned_64;
       pages     : Page_Count) return Boolean;

    --  Derivation may preserve or remove write authority, never add it.
    function Permission_Attenuates
      (parent, child : Permission) return Boolean;

    --  A child loan must be a non-empty page subrange of its parent.
    function Range_Attenuates
      (parentPages : Page_Count;
       childOffset : Page_Offset;
       childPages  : Page_Count) return Boolean;

    --  A grant's usable lifetime is represented as one state machine rather
    --  than independent active/pending/count fields.  Revocation completes
    --  immediately when there is no borrower or forwarding hold; otherwise
    --  it prevents new acquisitions until both kinds of use have ended.
    Maximum_Acquisition_Count : constant Natural := 127;
    subtype Acquisition_Count is
      Natural range 0 .. Maximum_Acquisition_Count;

    type Lifecycle is private;
    Inactive_Lifecycle  : constant Lifecycle;
    Available_Lifecycle : constant Lifecycle;

    function Is_Valid (value : Lifecycle) return Boolean;
    function Is_Active (value : Lifecycle) return Boolean;
    function Is_Available (value : Lifecycle) return Boolean;
    function Is_Revocation_Pending (value : Lifecycle) return Boolean;
    function Acquisition_Total
      (value : Lifecycle) return Acquisition_Count;
    function Can_Acquire (value : Lifecycle) return Boolean;

    --  One kernel-owned hold per forwarding scope, separate from user returns.
    --  Retention is one-shot for this grant identity. These operations do not
    --  authenticate forwarding authority, map pages or establish DMA quiescence.
    function Has_Forwarding_Hold (value : Lifecycle) return Boolean;
    function Can_Retain_Forwarding_Hold (value : Lifecycle) return Boolean;
    procedure Retain_Forwarding_Hold
      (value : in out Lifecycle; applied : out Boolean)
      with Pre => Is_Valid (value),
           Post => Is_Valid (value) and then
             applied = Can_Retain_Forwarding_Hold (value'Old) and then
             Acquisition_Total (value) = Acquisition_Total (value'Old) and then
             (if applied then
                Is_Available (value) and Has_Forwarding_Hold (value) and
                not Can_Retain_Forwarding_Hold (value)
              else value = value'Old);

    procedure Record_Acquire (value : in out Lifecycle)
      with Pre  => Is_Valid (value) and then Can_Acquire (value),
           Post => Is_Valid (value) and then Is_Available (value) and then
             Has_Forwarding_Hold (value) = Has_Forwarding_Hold (value'Old) and then
             Acquisition_Total (value) =
               Acquisition_Total (value'Old) + 1;

    type Revocation_Result is
      (Revocation_Rejected, Revocation_Pending, Revocation_Completed);

    procedure Request_Revocation
      (value  : in out Lifecycle;
       result : out Revocation_Result)
      with Pre  => Is_Valid (value),
           Post => Is_Valid (value) and then
             Has_Forwarding_Hold (value) = Has_Forwarding_Hold (value'Old) and then
             (if not Is_Active (value'Old) then
                  result = Revocation_Rejected and then value = value'Old
              elsif Acquisition_Total (value'Old) = 0 and then
                    not Has_Forwarding_Hold (value'Old) then
                  result = Revocation_Completed and then
                  not Is_Active (value)
              else
                  result = Revocation_Pending and then
                  Is_Revocation_Pending (value) and then
                  Acquisition_Total (value) =
                    Acquisition_Total (value'Old));

    type Return_Result is
      (Return_Rejected, Acquisition_Returned, Revocation_Completed_On_Return);

    procedure Record_Return
      (value  : in out Lifecycle;
       result : out Return_Result)
      with Pre  => Is_Valid (value),
           Post => Is_Valid (value) and then
             Has_Forwarding_Hold (value) = Has_Forwarding_Hold (value'Old) and then
             (if Acquisition_Total (value'Old) = 0 then
                  result = Return_Rejected and then value = value'Old
              elsif Is_Revocation_Pending (value'Old) and then
                    Acquisition_Total (value'Old) = 1 and then
                    not Has_Forwarding_Hold (value'Old)
              then
                  result = Revocation_Completed_On_Return and then
                  not Is_Active (value)
              else
                  result = Acquisition_Returned and then
                  Is_Active (value) and then
                  Acquisition_Total (value) =
                    Acquisition_Total (value'Old) - 1);

    type Hold_Release_Result is
      (Hold_Release_Rejected, Forwarding_Hold_Released,
       Revocation_Completed_On_Hold_Release);

    --  Only after the associated scope is closed and every child mapping has
    --  retired. This is NOT the userspace return-grant operation.
    procedure Release_Forwarding_Hold
      (value : in out Lifecycle; result : out Hold_Release_Result)
      with Pre => Is_Valid (value),
           Post => Is_Valid (value) and then
             Acquisition_Total (value) = Acquisition_Total (value'Old) and then
             (if not Has_Forwarding_Hold (value'Old) then
                result = Hold_Release_Rejected and value = value'Old
              else not Has_Forwarding_Hold (value) and
                not Can_Retain_Forwarding_Hold (value) and
                (if Is_Revocation_Pending (value'Old) and
                    Acquisition_Total (value'Old) = 0 then
                   result = Revocation_Completed_On_Hold_Release and
                   not Is_Active (value)
                 else result = Forwarding_Hold_Released and
                   Is_Active (value) and
                   Is_Available (value) = Is_Available (value'Old)));

    --  Receiver death ends its ordinary acquisitions, not downstream use.
    --  The adapter still unmaps/shoots down the dead receiver's mappings;
    --  a retained lifecycle protects the parent identity and owner resources.
    procedure Close_Receiver
      (value            : in out Lifecycle;
       had_acquisitions : out Boolean)
      with Pre  => Is_Valid (value),
           Post => Is_Valid (value) and then
             Acquisition_Total (value) = 0 and then
             Has_Forwarding_Hold (value) = Has_Forwarding_Hold (value'Old) and then
             Is_Active (value) = Has_Forwarding_Hold (value'Old) and then
             (if Has_Forwarding_Hold (value'Old) then
                Is_Revocation_Pending (value)) and then
             had_acquisitions =
               (Acquisition_Total (value'Old) /= 0);

private
    type Lifecycle_State is
      (Inactive, Available, Revocation_Requested);
    for Lifecycle_State use
      (Inactive => 0, Available => 1, Revocation_Requested => 2);
    for Lifecycle_State'Size use 8;

    type Forwarding_Hold_State is (Unused, Retained, Released);
    type Lifecycle is record
        -- Group the small state fields before the aligned reader count.
        state        : Lifecycle_State := Inactive;
        forwarding   : Forwarding_Hold_State := Unused;
        acquisitions : Acquisition_Count := 0;
    end record;

    Inactive_Lifecycle : constant Lifecycle :=
      (state => Inactive, acquisitions => 0, forwarding => Unused);
    Available_Lifecycle : constant Lifecycle :=
      (state => Available, acquisitions => 0, forwarding => Unused);
end Memory_Grants;
