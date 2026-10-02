pragma Ada_2022;
with Firmware_Tables.Catalog;
-- One boot-lifetime owned snapshot. No reset, mapping or physical-address API.
-- Build privately, then seal the entire inventory; no prefix is observable.
package Firmware_Tables.Snapshots with SPARK_Mode, Pure is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Byte;
   Max_Tables : constant := 32;
   Max_Table_Bytes : constant := 65_536;
   Max_Total_Bytes : constant := 1_048_576;
   type Phase is (Empty, Building, Ready, Failed);
   -- Capacities are chosen before construction from the discovered inventory.
   -- Defaults retain the boot prototype's policy until its allocator is wired.
   -- Payload is packed, so storage grows with Byte_Capacity, not tables * limit.
   type State
     (Table_Capacity : Positive := Max_Tables;
      Byte_Capacity : Positive := Max_Total_Bytes;
      Table_Byte_Limit : Positive := Max_Table_Bytes) is limited private;
   type Image (Table_Capacity, Byte_Capacity : Positive) is private;
   function Contents (S : State) return Image with Ghost;
   function Current (S : State) return Phase;
   function Count (S : State) return Natural with
     Post => Count'Result <= S.Table_Capacity
       and then (if Current (S) /= Ready then Count'Result = 0);
   function Item (S : State; Index : Positive) return Catalog.Descriptor with
     Pre => Index <= Count (S),
     Post => Item'Result.Extent <= S.Table_Byte_Limit;
   function Value (S : State; Index : Positive; Offset : Natural) return Byte with
     Pre => Index <= Count (S) and then Offset < Item (S, Index).Extent;
   procedure Begin_Snapshot (S : in out State; Tables : Natural) with
     Post => (if Current (S)'Old /= Empty then Current (S) = Current (S)'Old)
       and then (if Current (S)'Old /= Empty then Contents (S) = Contents (S)'Old);
   procedure Append
     (S : in out State; Expected : Catalog.Descriptor; Source : Bytes;
      Success : out Boolean) with
     Post => Count (S) = Count (S)'Old
       and then (if Current (S)'Old /= Building then not Success
                   and then Current (S) = Current (S)'Old
                   and then Contents (S) = Contents (S)'Old);
   procedure Seal (S : in out State) with
     Post => Current (S) /= Building
       and then (if Current (S)'Old /= Building then Current (S) = Current (S)'Old
                   and then Contents (S) = Contents (S)'Old);
   procedure Reject (S : in out State) with
     Post => (if Current (S)'Old = Building then Current (S) = Failed
              else Current (S) = Current (S)'Old
                and then Contents (S) = Contents (S)'Old);
   -- No kernel addresses or uninitialized padding leave through this interface.
   procedure Copy
     (S : State; Index : Positive; Destination : out Bytes;
      Success : out Boolean) with
     Post => (if Success then Index <= Count (S)
       and then Destination'Length >= Item (S, Index).Extent
       and then (for all I in Destination'Range =>
         Destination (I) =
           (if I - Destination'First < Item (S, Index).Extent
            then Value (S, Index, I - Destination'First) else 0))
       else (for all B of Destination => B = 0));
private
   type Table_Entry is record
      Description : Catalog.Descriptor;
      Offset : Natural := 0;
   end record;
   type Entries is array (Positive range <>) of Table_Entry;
   -- Ghost projection makes the complete freeze property explicit while the
   -- real snapshot remains limited (no accidental assignment or reset).
   type Image (Table_Capacity, Byte_Capacity : Positive) is record
      Mode : Phase;
      Required, Used, Total : Natural;
      Tables : Entries (1 .. Table_Capacity);
      Data : Bytes (1 .. Byte_Capacity);
   end record;
   type State
     (Table_Capacity : Positive := Max_Tables;
      Byte_Capacity : Positive := Max_Total_Bytes;
      Table_Byte_Limit : Positive := Max_Table_Bytes) is limited record
      Mode : Phase := Empty;
      Required, Used : Natural := 0;
      Total : Natural := 0;
      Tables : Entries (1 .. Table_Capacity);
      Data : Bytes (1 .. Byte_Capacity) := [others => 0];
   end record with Type_Invariant =>
     State.Required <= State.Table_Capacity
     and then State.Used <= State.Required
     and then State.Total <= State.Byte_Capacity
     and then (for all I in 1 .. State.Used =>
       State.Tables (I).Description.Extent <= State.Table_Byte_Limit
       and then State.Tables (I).Offset <= State.Total
       and then State.Tables (I).Description.Extent <=
         State.Total - State.Tables (I).Offset)
     and then
     (if State.Mode in Building | Ready then State.Required > 0
       and then State.Used <= State.Required)
     and then (if State.Mode = Ready then State.Used = State.Required);
end Firmware_Tables.Snapshots;
