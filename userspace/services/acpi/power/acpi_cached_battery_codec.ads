pragma Ada_2022;
with Interfaces;
with ACPI_Protocol;
with ACPI_Cached_Power_Values;
with ACPI_Cached_Battery_Values;
with ACPI_Battery_Observation_Protocol;
with ACPI_Battery_Status;
generic
   Read_Label : Interfaces.Unsigned_32;
   Battery_Page : Interfaces.Unsigned_64;
package ACPI_Cached_Battery_Codec with Pure, SPARK_Mode is
   package P renames ACPI_Battery_Observation_Protocol;
   package V renames ACPI_Cached_Power_Values;
   package B renames ACPI_Cached_Battery_Values;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type V.Generation, V.Availability, V.Microseconds;
   use type P.Subpage;
   use type B.Entry_Status, B.Acquisition_Reason, B.Information_Source, B.Unit_Kind;
   subtype Word is Interfaces.Unsigned_64;
   subtype Word_Index is Natural range 0 .. 3;
   type Words is array (Word_Index) of Word;
   Maximum_Reply_Word : constant Word := Word (Interfaces.Integer_64'Last);
   subtype Reply_Word is Word range 0 .. Maximum_Reply_Word;
   type Reply_Words is array (Word_Index) of Reply_Word;
   Empty_Reply : constant Reply_Words := [others => 0];
   Request_Length : constant Interfaces.Unsigned_8 := Words'Length;
   Page_Word : constant Word_Index := 0;
   Maximum_Age_Word : constant Word_Index := 1;
   Selector_Word : constant Word_Index := 2;
   Expected_Generation_Word : constant Word_Index := 3;
   type Packet is record
      Label : Interfaces.Unsigned_32 := 0;
      Length, Flags : Interfaces.Unsigned_8 := 0;
      Reserved : Interfaces.Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
   subtype Slot_Number is Natural range 0 .. B.Maximum_Batteries;
   function Valid_Location (Page : P.Subpage; Slot : Slot_Number) return Boolean is
     (if Page in P.Inventory | P.Sample_Time then Slot = 0 else Slot > 0)
     with Global => null, Annotate => (GNATprove, Inline_For_Proof);
   type Decision is (Not_Handled, Denied, Malformed, Read_Inventory);
   -- Executable wire contract, independent of the decoder's conversions:
   -- global inventory selector0; time selector1 and all entry pages require
   -- a positive generation. Gaps and out-of-budget slot numbers are refused.
   function Selector_Valid (Selector, Expected : Word) return Boolean is
     (Selector / P.Selector_Stride <= Word (Slot_Number'Last)
      and then Selector mod P.Selector_Stride <= Word (P.Subpage'Pos (P.Subpage'Last))
      and then Valid_Location
        (P.Subpage'Val (Natural (Selector mod P.Selector_Stride)), Slot_Number (Selector / P.Selector_Stride))
      and then (Selector mod P.Selector_Stride = Word (P.Subpage'Pos (P.Inventory)) or else Expected > 0))
     with Ghost, Global => null, Annotate => (GNATprove, Inline_For_Proof);
   function Canonical_Selector (Selector, Expected : Word) return Boolean is
     (Selector / P.Selector_Stride <= Word (Slot_Number'Last)
      and then Selector mod P.Selector_Stride <= Word (P.Subpage'Pos (P.Subpage'Last))
      and then Valid_Location
        (P.Subpage'Val (Natural (Selector mod P.Selector_Stride)), Slot_Number (Selector / P.Selector_Stride))
      and then (Selector mod P.Selector_Stride = Word (P.Subpage'Pos (P.Inventory)) or else Expected > 0))
     with Global => null,
       Post => Canonical_Selector'Result = Selector_Valid (Selector, Expected)
         and then (if Canonical_Selector'Result then
           Selector / P.Selector_Stride <= Word (Slot_Number'Last)
      and then Selector mod P.Selector_Stride <= Word (P.Subpage'Pos (P.Subpage'Last))
      and then Valid_Location
        (P.Subpage'Val (Natural (Selector mod P.Selector_Stride)), Slot_Number (Selector / P.Selector_Stride))
      and then (Selector mod P.Selector_Stride = Word (P.Subpage'Pos (P.Inventory)) or else Expected > 0));
   function Expected_Decision (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Decision is
     (if Request.Label /= Read_Label or else Request.Data (Page_Word) /= Battery_Page then Not_Handled
      elsif not ACPI_Protocol.Is_Observer (Stamp) then Denied
      elsif Request.Length /= Request_Length or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (Expected_Generation_Word) > Maximum_Reply_Word
        or else not Selector_Valid (Request.Data (Selector_Word), Request.Data (Expected_Generation_Word))
      then Malformed else Read_Inventory)
     with Ghost, Global => null, Annotate => (GNATprove, Inline_For_Proof);
   type Selection (Kind : Decision := Not_Handled) is record
      case Kind is
         when Read_Inventory =>
            Maximum_Age : V.Microseconds := 0;
            Page : P.Subpage := P.Inventory;
            Slot : Slot_Number := 0;
            Expected : V.Generation := 0;
         when others => null;
      end case;
   end record;
   function Inspect (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Selection
     with Global => null,
       Post => Inspect'Result.Kind = Expected_Decision (Request, Stamp)
         and then (if Inspect'Result.Kind = Read_Inventory then
         Valid_Location (Inspect'Result.Page, Inspect'Result.Slot)
         and then (Inspect'Result.Page = P.Inventory or else Inspect'Result.Expected > 0)
         and then Inspect'Result.Maximum_Age = V.Microseconds (Request.Data (Maximum_Age_Word))
         and then Inspect'Result.Expected = V.Generation (Request.Data (Expected_Generation_Word))
         and then Word (Inspect'Result.Slot) = Request.Data (Selector_Word) / P.Selector_Stride
         and then Word (P.Subpage'Pos (Inspect'Result.Page)) = Request.Data (Selector_Word) mod P.Selector_Stride);
   function Expected_Status (Value : B.View) return V.Availability is
     (if Value.Status /= V.Available then Value.Status
      elsif not Value.Time.Known then V.Clock_Unknown
      elsif Value.Revision = 0 then V.Failure else V.Available)
     with Ghost, Global => null;
   function Effective_Status (Value : B.View) return V.Availability
     with Global => null, Post => Effective_Status'Result = Expected_Status (Value);
   function Entry_Consistent (Item : B.Entry_Value) return Boolean is
     (case Item.Status is
         when B.Sampled => Item.Reason = B.No_Failure and then Item.Value.Source /= B.None_Reported
           and then Item.Value.Units /= B.Unspecified_Unit,
         when B.Known_Absent => Item.Reason = B.Absent,
         when B.Acquisition_Failed => Item.Reason not in B.No_Failure | B.Absent)
     with Ghost, Global => null;
   function Entry_Status (Item : B.Entry_Value) return B.Entry_Status is
     (if Entry_Consistent (Item) then Item.Status else B.Acquisition_Failed)
     with Ghost, Global => null;
   function Entry_Reason (Item : B.Entry_Value) return B.Acquisition_Reason is
     (if Entry_Consistent (Item) then Item.Reason else B.Provider_Contract_Error)
     with Ghost, Global => null;
   function Entry_Availability (Item : B.Entry_Value) return V.Availability is
     (case Entry_Status (Item) is when B.Sampled => V.Available,
        when B.Known_Absent => V.Missing, when B.Acquisition_Failed => V.Failure)
     with Ghost, Global => null;
   function Quantity_Word (Known : Boolean; Raw : Interfaces.Unsigned_32) return Reply_Word is
     (if Known then Reply_Word (Raw) else P.Unknown_Quantity)
     with Ghost, Global => null;
   Half_Word_Mask : constant Word := Word (Interfaces.Unsigned_32'Last);
   function Join (Low, High : Reply_Word) return Word is
     (Low or Interfaces.Shift_Left (High, Interfaces.Unsigned_32'Size))
     with Ghost, Global => null, Pre => Low <= Half_Word_Mask and then High <= Half_Word_Mask;
   function Entry_Page_Matches (Item : B.Entry_Value; Page : P.Subpage; Data : Reply_Words) return Boolean is
     (if Page = P.Entry_Summary then
         Data (1) = V.Availability'Pos (V.Available)
           and then Data (2) = B.Entry_Status'Pos (Entry_Status (Item))
           and then Data (3) = B.Acquisition_Reason'Pos (Entry_Reason (Item))
      elsif Data (1) /= V.Availability'Pos (Entry_Availability (Item)) then False
      elsif Entry_Status (Item) /= B.Sampled then Data (2 .. 3) = [0, 0]
      else (case Page is
         when P.Information => Data (2) = B.Information_Source'Pos (Item.Value.Source)
           and then Data (3) = B.Unit_Kind'Pos (Item.Value.Units),
         when P.Capacities => Data (2) = Quantity_Word (Item.Value.Design_Capacity.Known, Item.Value.Design_Capacity.Value)
           and then Data (3) = Quantity_Word (Item.Value.Last_Full_Capacity.Known, Item.Value.Last_Full_Capacity.Value),
         when P.Design_Voltage => Data (2) = Quantity_Word (Item.Value.Design_Voltage.Known, Item.Value.Design_Voltage.Value)
           and then Data (3) = 0,
         when P.Dynamic_State => Data (2) = ACPI_Battery_Status.Direction'Pos (Item.Value.Dynamic.Flow)
           and then Data (3) = (if Item.Value.Dynamic.Critical then P.Critical_Flag else 0)
             + (if Item.Value.Dynamic.Charge_Limiting then P.Charge_Limiting_Flag else 0),
         when P.Rate_And_Capacity => Data (2) = Quantity_Word (Item.Value.Dynamic.Rate.Known, Item.Value.Dynamic.Rate.Value)
           and then Data (3) = Quantity_Word (Item.Value.Dynamic.Remaining_Capacity.Known, Item.Value.Dynamic.Remaining_Capacity.Value),
         when P.Dynamic_Voltage => Data (2) = Quantity_Word (Item.Value.Dynamic.Voltage.Known, Item.Value.Dynamic.Voltage.Value)
           and then Data (3) = Reply_Word (Item.Value.Dynamic.Reserved_State_Bits),
         when P.Inventory | P.Sample_Time | P.Entry_Summary => False))
     with Ghost, Global => null, Pre => Page in P.Entry_Summary .. P.Dynamic_Voltage;
   function Page_Matches (Value : B.View; Page : P.Subpage; Slot : Slot_Number; Data : Reply_Words) return Boolean is
     (if Page = P.Sample_Time then
         Data (1) = Boolean'Pos (Value.Time.Known)
           and then Data (2) <= Half_Word_Mask and then Data (3) <= Half_Word_Mask
           and then Join (Data (2), Data (3)) = (if Value.Time.Known then Word (Value.Time.Value) else 0)
      elsif Effective_Status (Value) /= V.Available then
         Data (1) = V.Availability'Pos (Effective_Status (Value)) and then Data (2 .. 3) = [0, 0]
      elsif Page = P.Inventory then Data (1) = V.Availability'Pos (V.Available)
         and then Data (2) = Word (Value.Count) and then Data (3) = 0
      elsif Slot > Value.Count then Data (1) = V.Availability'Pos (V.Missing) and then Data (2 .. 3) = [0, 0]
      else Entry_Page_Matches (Value.Items (B.Slot (Slot)), Page, Data))
     with Ghost, Global => null, Pre => Valid_Location (Page, Slot);
   function Encode (Value : B.View; Page : P.Subpage; Slot : Slot_Number) return Reply_Words
     with Global => null, Pre => Valid_Location (Page, Slot),
       Post => Encode'Result (0) = Reply_Word (Value.Revision)
         and then Page_Matches (Value, Page, Slot, Encode'Result)
         and then (if Page /= P.Sample_Time and then Effective_Status (Value) /= V.Available
           then Encode'Result (2 .. 3) = [0, 0]);
end ACPI_Cached_Battery_Codec;
