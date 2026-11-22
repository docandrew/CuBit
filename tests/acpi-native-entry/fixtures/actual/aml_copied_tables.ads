pragma Ada_2022;
with Firmware_Tables;
with Interfaces;
-- Append-only service-owned SDT candidates. No address mapping, hardware
-- authority, namespace admission or AML execution. The owner keeps this state
-- alive while loaded code/references can use it; there is no reset/slot reuse.
package AML_Copied_Tables with SPARK_Mode, Pure is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Table_Count is Natural;
   subtype Byte_Count is Natural;
   package F renames Firmware_Tables;
   use type F.Byte;
   subtype Table_Length is Natural range F.Table_Header_Size .. Natural'Last;
   type Table_ID is new Natural;
   No_Table : constant Table_ID := 0;
   type State (Table_Capacity, Byte_Capacity : Natural) is limited private
     with Default_Initial_Condition => Valid (State);
   -- Complete immutable audit snapshot, including spans and byte backing. Ghost
   -- equality can frame failed imports and distinguish same-size different data.
   type Store_Model (Table_Capacity, Byte_Capacity : Natural) is private with Ghost;
   function Model (Store : State) return Store_Model with Ghost,
     Post => Model'Result.Table_Capacity = Store.Table_Capacity
       and then Model'Result.Byte_Capacity = Store.Byte_Capacity;
   function Count (Store : State) return Natural;
   function Used_Bytes (Store : State) return Natural;
   function Valid (Store : State) return Boolean;
   function Present (Store : State; ID : Table_ID) return Boolean;
   function Length (Store : State; ID : Table_ID) return Table_Length
     with Pre => Valid (Store) and then Present (Store, ID);
   function Byte_At (Store : State; ID : Table_ID; Offset : Natural) return F.Byte
     with Pre => Valid (Store) and then Present (Store, ID)
       and then Offset < Length (Store, ID);
   type Outcome is (Not_Attempted, Added, Already_Retained, Invalid_Table,
      Non_SDT, Table_Limit, Byte_Limit, Budget_Exceeded);
   type Report is record
      Status : Outcome := Not_Attempted;
      ID : Table_ID := No_Table;
      Raw_Length : Interfaces.Unsigned_32 := 0;
      Extent, Charged : Natural := 0;
      Admission : F.Admission := F.Truncated;
   end record;
   -- Source is a stable owned copy for this call, never a live physical view.
   -- Validate/checksum the declared extent and copy exactly it; trailing source
   -- bytes are ignored. FACS has a different header and is refused. Other SDTs
   -- are only storage candidates: signature/AML semantics are separate policy.
   -- Charge header inspection, checksum extent, each candidate comparison and
   -- final copy before any mutation. Full-byte duplicates consume no capacity,
   -- even when full. No failure publishes a partial table or advances an ID.
   -- IDs are local metadata, not cross-owner references or public DDB handles.
   procedure Retain
     (Store : in out State; Source : F.Bytes; Fuel : Natural; Result : out Report)
     with Pre => Valid (Store), Post => Valid (Store)
       and then Result.Charged <= Fuel
       and then Count (Store) >= Count (Store)'Old
       and then Used_Bytes (Store) >= Used_Bytes (Store)'Old
       and then (if Result.Status = Added then
          Count (Store) = Count (Store)'Old + 1
          and then Used_Bytes (Store) = Used_Bytes (Store)'Old + Result.Extent
          and then Present (Store, Result.ID)
          and then Length (Store, Result.ID) = Result.Extent
        else Count (Store) = Count (Store)'Old
          and then Used_Bytes (Store) = Used_Bytes (Store)'Old)
       and then (if Result.Status /= Added then Model (Store) = Model (Store)'Old)
       and then (if Result.Status = Already_Retained then Present (Store, Result.ID));
   -- Exact requested window only; a refused window is zeroed in full. This
   -- does not export a mutable alias to backing storage. No implicit truncation.
   procedure Copy_Window
     (Store : State; ID : Table_ID; Offset : Natural;
      Data : out F.Bytes; Copied : out Boolean)
     with Pre => Valid (Store),
       Post => Copied = (Present (Store, ID)
         and then Offset <= Length (Store, ID)
         and then Data'Length <= Length (Store, ID) - Offset)
       and then (if Copied then
         (for all I in Data'Range =>
           Data (I) = Byte_At (Store, ID, Offset + (I - Data'First)))
       else (for all I in Data'Range => Data (I) = 0));
private
   type Span is record Offset, Extent : Natural := 0; end record;
   type Spans is array (Positive range <>) of Span;
   type Store_Model (Table_Capacity, Byte_Capacity : Natural) is record
      Tables : Spans (1 .. Table_Capacity);
      Data : F.Bytes (1 .. Byte_Capacity);
      Stored, Used : Natural;
   end record;
   type State (Table_Capacity, Byte_Capacity : Natural) is limited record
      Tables : Spans (1 .. Table_Capacity);
      Data : F.Bytes (1 .. Byte_Capacity) := [others => 0];
      Stored : Table_Count := 0;
      Used : Byte_Count := 0;
   end record;
   function Count (Store : State) return Natural is (Store.Stored);
   function Used_Bytes (Store : State) return Natural is (Store.Used);
   function Present (Store : State; ID : Table_ID) return Boolean is
     (ID /= No_Table and then ID <= Table_ID (Store.Stored));
   function Length (Store : State; ID : Table_ID) return Table_Length is
     (Store.Tables (Positive (ID)).Extent);
end AML_Copied_Tables;
