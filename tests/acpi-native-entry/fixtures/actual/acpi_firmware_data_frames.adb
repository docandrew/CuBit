pragma Ada_2022;
package body ACPI_Firmware_Data_Frames with SPARK_Mode is
   use Interfaces;
   use type IPC.Time_Instant;
   function Finite (Value : IPC.Time_Instant) return Boolean is
      (Value /= 0 and then Value /= IPC.No_Finite_Deadline);
   function Valid (Item : Request) return Boolean is
   begin
      if Item.ID = No_ID then return False; end if;
      case Item.Kind is
         when Publish =>
            return L.Valid (Item.Region) and then Finite (Item.Publication_Deadline);
         when Recover_Publication => return Finite (Item.Recovery_Deadline);
         when Begin_Scope =>
            return Finite (Item.Scope_Deadline)
              and then Word (Item.Scope_Deadline) <= Maximum_Scope_Deadline
              and then ((Item.Lock_Domain = 0 and then Item.Lock_Sequence = 0)
                or else (Item.Lock_Domain /= 0 and then Item.Lock_Sequence /= 0));
         when Select_Datum =>
            return Item.Record_ID /= No_ID and then Item.Client_Descriptor_ID /= No_ID
              and then Item.Offset <= Maximum_Offset;
         when Read_Datum | Query_Operation => return Item.Operation_ID /= No_ID;
         when Write_Datum => return Item.Write_Operation_ID /= No_ID;
         when others => return True;
      end case;
   end Valid;
   function Encode (Item : Request) return IPC.Packet is
      Result : IPC.Packet := (Label => Label_For (Item.Kind), others => <>);
   begin
      Result.Data (0) := Word (Item.ID);
      case Item.Kind is
         when Publish => Result.Data (1) := Item.Region.Address;
            Result.Data (2) := Item.Region.Length;
            Result.Data (3) := Word (Item.Publication_Deadline);
         when Recover_Publication => Result.Data (1) := Word (Item.Recovery_Deadline);
         when Begin_Scope => Result.Data (1) := Word (Item.Scope_Deadline);
            Result.Data (2) := Item.Lock_Domain; Result.Data (3) := Item.Lock_Sequence;
         when Select_Datum => Result.Data (1) := Word (Item.Record_ID);
            Result.Data (2) := Shift_Left (Item.Offset, Width_Bits)
              or Word (T.Access_Width'Pos (Item.Width));
            Result.Data (3) := Word (Item.Client_Descriptor_ID);
         when Read_Datum | Query_Operation =>
            Result.Data (1) := Word (Item.Operation_ID);
         when Write_Datum => Result.Data (1) := Word (Item.Write_Operation_ID);
            Result.Data (2) := Item.Value;
         when others => null;
      end case;
      return Result;
   end Encode;
   function Decode (Item : IPC.Packet; Has_Capability : Boolean) return Decoded is
   begin
      if Has_Capability or else Item.Length /= 4 or else Item.Flags /= 0
        or else Item.Reserved /= 0 or else Item.Label < First_Label
        or else Item.Label > Label_For (Operation'Last) or else Item.Data (0) = 0
      then return (Valid => False); end if;
      declare
         Kind : constant Operation := Operation'Val (Item.Label - First_Label);
         Result : Request (Kind);
      begin
         Result.ID := Identifier (Item.Data (0));
         case Kind is
            when Publish => Result.Region := (Item.Data (1), Item.Data (2));
               Result.Publication_Deadline := IPC.Time_Instant (Item.Data (3));
            when Recover_Publication =>
               if Item.Data (2) /= 0 or else Item.Data (3) /= 0 then return (Valid => False); end if;
               Result.Recovery_Deadline := IPC.Time_Instant (Item.Data (1));
            when Begin_Scope => Result.Scope_Deadline := IPC.Time_Instant (Item.Data (1));
               Result.Lock_Domain := Item.Data (2); Result.Lock_Sequence := Item.Data (3);
            when Select_Datum => Result.Record_ID := Identifier (Item.Data (1));
               Result.Offset := Shift_Right (Item.Data (2), Width_Bits);
               Result.Width := T.Access_Width'Val (Item.Data (2) and Width_Mask);
               Result.Client_Descriptor_ID := Identifier (Item.Data (3));
            when Read_Datum | Query_Operation =>
               if Item.Data (2) /= 0 or else Item.Data (3) /= 0 then return (Valid => False); end if;
               Result.Operation_ID := Identifier (Item.Data (1));
            when Write_Datum =>
               if Item.Data (3) /= 0 then return (Valid => False); end if;
               Result.Write_Operation_ID := Identifier (Item.Data (1)); Result.Value := Item.Data (2);
            when others =>
               if Item.Data (1) /= 0 or else Item.Data (2) /= 0 or else Item.Data (3) /= 0
               then return (Valid => False); end if;
         end case;
         if not Valid (Result) then return (Valid => False); end if;
         return (Valid => True, Item => Result);
      end;
   end Decode;
end ACPI_Firmware_Data_Frames;
