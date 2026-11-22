pragma Ada_2022;
package body ACPI_Cached_Battery_Codec with SPARK_Mode is
   function Inspect (Request : Packet; Stamp : ACPI_Protocol.Role_Tag) return Selection is
      Raw_Slot, Raw_Page : Word;
   begin
      if Request.Label /= Read_Label or else Request.Data (Page_Word) /= Battery_Page then
         return (Kind => Not_Handled);
      elsif not ACPI_Protocol.Is_Observer (Stamp) then return (Kind => Denied);
      elsif Request.Length /= Request_Length or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (Expected_Generation_Word) > Maximum_Reply_Word then
         return (Kind => Malformed);
      end if;
      if not Canonical_Selector (Request.Data (Selector_Word), Request.Data (Expected_Generation_Word)) then
         return (Kind => Malformed);
      end if;
      Raw_Slot := Request.Data (Selector_Word) / P.Selector_Stride;
      Raw_Page := Request.Data (Selector_Word) mod P.Selector_Stride;
      declare
         Page : constant P.Subpage := P.Subpage'Val (Natural (Raw_Page));
         Slot : constant Slot_Number := Slot_Number (Raw_Slot);
      begin
         -- Check each conversion fact separately before assembling the variant.
         pragma Assert (Word (P.Subpage'Pos (Page)) = Raw_Page);
         pragma Assert (Word (Slot) = Raw_Slot);
         pragma Assert (Valid_Location (Page, Slot));
         pragma Assert (Page = P.Inventory or else Request.Data (Expected_Generation_Word) > 0);
         return (Read_Inventory, V.Microseconds (Request.Data (Maximum_Age_Word)),
           Page, Slot, V.Generation (Request.Data (Expected_Generation_Word)));
      end;
   end Inspect;
   function Effective_Status (Value : B.View) return V.Availability is
   begin
      if Value.Status = V.Available and then not Value.Time.Known then return V.Clock_Unknown;
      elsif Value.Status = V.Available and then Value.Revision = 0 then return V.Failure;
      else return Value.Status;
      end if;
   end Effective_Status;
   function Encode (Value : B.View; Page : P.Subpage; Slot : Slot_Number) return Reply_Words is
      Status : constant V.Availability := Effective_Status (Value);
      Result : Reply_Words := [Reply_Word (Value.Revision), V.Availability'Pos (Status), 0, 0];
      function Quantity (Known : Boolean; Raw : Interfaces.Unsigned_32) return Reply_Word is
        (if Known then Reply_Word (Raw) else P.Unknown_Quantity);
   begin
      if Page = P.Sample_Time then
         Result (1) := Boolean'Pos (Value.Time.Known);
         if Value.Time.Known then
            Result (2) := Word (Value.Time.Value) and Word (Interfaces.Unsigned_32'Last);
            Result (3) := Interfaces.Shift_Right (Word (Value.Time.Value), Interfaces.Unsigned_32'Size);
         end if;
         return Result;
      elsif Status /= V.Available then return Result;
      elsif Page = P.Inventory then Result (2) := Reply_Word (Value.Count); return Result;
      elsif Slot > Value.Count then
         Result (1) := V.Availability'Pos (V.Missing); return Result;
      end if;
      declare
         Item : B.Entry_Value renames Value.Items (B.Slot (Slot));
         Entry_Status : B.Entry_Status := Item.Status;
         Reason : B.Acquisition_Reason := Item.Reason;
      begin
         if (Entry_Status = B.Sampled and then (Reason /= B.No_Failure
              or else Item.Value.Source = B.None_Reported or else Item.Value.Units = B.Unspecified_Unit))
           or else (Entry_Status = B.Known_Absent and then Reason /= B.Absent)
           or else (Entry_Status = B.Acquisition_Failed and then Reason in B.No_Failure | B.Absent) then
            Entry_Status := B.Acquisition_Failed; Reason := B.Provider_Contract_Error;
         end if;
         if Page = P.Entry_Summary then
            Result (2) := B.Entry_Status'Pos (Entry_Status);
            Result (3) := B.Acquisition_Reason'Pos (Reason);
            return Result;
         elsif Entry_Status /= B.Sampled then
            Result (1) := V.Availability'Pos (if Entry_Status = B.Known_Absent then V.Missing else V.Failure);
            return Result;
         end if;
         case Page is
            when P.Information =>
               Result (2) := B.Information_Source'Pos (Item.Value.Source);
               Result (3) := B.Unit_Kind'Pos (Item.Value.Units);
            when P.Capacities =>
               Result (2) := Quantity (Item.Value.Design_Capacity.Known, Item.Value.Design_Capacity.Value);
               Result (3) := Quantity (Item.Value.Last_Full_Capacity.Known, Item.Value.Last_Full_Capacity.Value);
            when P.Design_Voltage =>
               Result (2) := Quantity (Item.Value.Design_Voltage.Known, Item.Value.Design_Voltage.Value);
            when P.Dynamic_State =>
               Result (2) := ACPI_Battery_Status.Direction'Pos (Item.Value.Dynamic.Flow);
               Result (3) := (if Item.Value.Dynamic.Critical then P.Critical_Flag else 0)
                 + (if Item.Value.Dynamic.Charge_Limiting then P.Charge_Limiting_Flag else 0);
            when P.Rate_And_Capacity =>
               Result (2) := Quantity (Item.Value.Dynamic.Rate.Known, Item.Value.Dynamic.Rate.Value);
               Result (3) := Quantity (Item.Value.Dynamic.Remaining_Capacity.Known, Item.Value.Dynamic.Remaining_Capacity.Value);
            when P.Dynamic_Voltage =>
               Result (2) := Quantity (Item.Value.Dynamic.Voltage.Known, Item.Value.Dynamic.Voltage.Value);
               Result (3) := Reply_Word (Item.Value.Dynamic.Reserved_State_Bits);
            when P.Inventory | P.Sample_Time | P.Entry_Summary => null;
         end case;
      end;
      return Result;
   end Encode;
end ACPI_Cached_Battery_Codec;
