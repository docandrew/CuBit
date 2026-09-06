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
    --  immediately when there is no borrower, otherwise it prevents new
    --  acquisitions until the final return.
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

    procedure Record_Acquire (value : in out Lifecycle)
      with Pre  => Is_Valid (value) and then Can_Acquire (value),
           Post => Is_Valid (value) and then Is_Available (value) and then
             Acquisition_Total (value) =
               Acquisition_Total (value'Old) + 1;

    type Revocation_Result is
      (Revocation_Rejected, Revocation_Pending, Revocation_Completed);

    procedure Request_Revocation
      (value  : in out Lifecycle;
       result : out Revocation_Result)
      with Pre  => Is_Valid (value),
           Post => Is_Valid (value) and then
             (if not Is_Active (value'Old) then
                  result = Revocation_Rejected and then value = value'Old
              elsif Acquisition_Total (value'Old) = 0 then
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
             (if Acquisition_Total (value'Old) = 0 then
                  result = Return_Rejected and then value = value'Old
              elsif Is_Revocation_Pending (value'Old) and then
                    Acquisition_Total (value'Old) = 1
              then
                  result = Revocation_Completed_On_Return and then
                  not Is_Active (value)
              else
                  result = Acquisition_Returned and then
                  Is_Active (value) and then
                  Acquisition_Total (value) =
                    Acquisition_Total (value'Old) - 1);

    procedure Force_Close
      (value            : in out Lifecycle;
       had_acquisitions : out Boolean)
      with Pre  => Is_Valid (value),
           Post => Is_Valid (value) and then not Is_Active (value) and then
             had_acquisitions =
               (Acquisition_Total (value'Old) /= 0);

private
    type Lifecycle_State is
      (Inactive, Available, Revocation_Requested);
    for Lifecycle_State use
      (Inactive => 0, Available => 1, Revocation_Requested => 2);
    for Lifecycle_State'Size use 8;

    type Lifecycle is record
        state        : Lifecycle_State := Inactive;
        acquisitions : Acquisition_Count := 0;
    end record;

    Inactive_Lifecycle : constant Lifecycle :=
      (state => Inactive, acquisitions => 0);
    Available_Lifecycle : constant Lifecycle :=
      (state => Available, acquisitions => 0);
end Memory_Grants;
