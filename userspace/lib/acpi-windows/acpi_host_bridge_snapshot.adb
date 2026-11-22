pragma Ada_2022;
with Interfaces;
package body ACPI_Host_Bridge_Snapshot with SPARK_Mode is
   use type W.Word;
   use all type Snapshot_State;
   use type M.Failure_Reason;
   use type Firmware_Tables.Byte;
   Segment_Present : constant W.Word := 1;
   Bus_Present : constant W.Word := 2;
   All_Claims : constant W.Word := W.Has_General or W.Has_Minimum or W.Has_Maximum
     or W.Has_Granularity or W.Has_Translation or W.Has_Length or W.Has_Attributes
     or W.Has_Cache or W.Has_Writable or W.Has_Source or W.Has_Decoding or W.Has_Encoded_Alignment;
   type Header_Index is (Signature, Format_Version, Header_Size, Descriptor_Size,
      State_Code, Root, Serial, Coordinate_Flags, Segment_Value, Bus_Value,
      Descriptor_Count, Required_Count, Source_Length, Failure_Reason);
   type Header_Data is array (Header_Index) of W.Word;
   function Flag (Value : Boolean; Mask : W.Word) return W.Word is (if Value then Mask else 0);
   function Valid_Header (Info : Header; Count, Source_Bytes : Natural) return Boolean is
     (Info.Root_ID /= 0 and then Info.Sequence /= 0
      and then (if Info.Segment.Present then Info.Segment.Value <= 65_535 else Info.Segment.Value = 0)
      and then (if Info.Base_Bus.Present then Info.Base_Bus.Value <= 255 else Info.Base_Bus.Value = 0)
      and then (if Info.State = Complete then Info.Required_Descriptors = W.Word (Count) and then Info.Reason = M.No_Failure
                else Count = 0 and then Source_Bytes = 0 and then Info.Reason /= M.No_Failure));
   function Valid_Row (Data : W.Row; Number : Positive; Source_Bytes : Natural) return Boolean is
      C : constant W.Word := Data (W.Claims);
      Geometry : constant W.Word := W.Has_Minimum or W.Has_Maximum or W.Has_Granularity or W.Has_Translation or W.Has_Length;
      Expected : W.Word := 0;
      Last : W.Word;
   begin
      if Data (W.Ordinal) /= W.Word (Number) or else Data (W.Form) not in W.Word_Address .. W.Memory_32
        or else Data (W.Kind) not in W.Memory_Kind .. W.Vendor_Kind
        or else (C and not All_Claims) /= 0 or else Data (W.General_Flags) > 15
        or else Data (W.Specific_Flags) > 255 or else Data (W.Writable) > 1
        or else Data (W.Cache) > 3 or else Data (W.Source_Index) > 255
        or else Data (W.Descriptor_Extent) = 0
        or else Data (W.Descriptor_Offset) > W.Word (Source_Bytes)
        or else Data (W.Descriptor_Extent) > W.Word (Source_Bytes) - Data (W.Descriptor_Offset)
      then return False; end if;
      Last := Data (W.Descriptor_Offset) + Data (W.Descriptor_Extent);
      if Data (W.Form) in W.Word_Address .. W.Extended_Address then
         if Data (W.Address_Bits) /=
           (if Data (W.Form) = W.Word_Address then 16 elsif Data (W.Form) = W.DWord_Address then 32 else 64)
         then return False; end if;
         Expected := Geometry or W.Has_General;
         if Data (W.Form) = W.Extended_Address then
            Expected := Expected or W.Has_Attributes;
            if Data (W.Kind) = W.PCC_Kind then return False; end if;
         end if;
         if Data (W.Kind) = W.Memory_Kind then
            Expected := Expected or W.Has_Writable;
            if Data (W.Form) /= W.Extended_Address then Expected := Expected or W.Has_Cache; end if;
         end if;
         if (C and W.Has_Source) /= 0 then
            if Data (W.Form) = W.Extended_Address then return False; end if;
            Expected := Expected or W.Has_Source;
         end if;
      elsif Data (W.Form) = W.IO_Descriptor then
         if Data (W.Kind) /= W.IO_Kind or else Data (W.Address_Bits) /= 16
           or else Data (W.Decoding_Bits) not in 10 | 16 then return False; end if;
         Expected := W.Has_Minimum or W.Has_Maximum or W.Has_Granularity or W.Has_Length or W.Has_Decoding;
      elsif Data (W.Form) = W.Fixed_IO_Descriptor then
         if Data (W.Kind) /= W.IO_Kind or else Data (W.Address_Bits) /= 16
           or else Data (W.Decoding_Bits) /= 10 then return False; end if;
         Expected := W.Has_Minimum or W.Has_Length or W.Has_Decoding;
      elsif Data (W.Form) = W.Fixed_Memory_32 then
         if Data (W.Kind) /= W.Memory_Kind or else Data (W.Address_Bits) /= 32 then return False; end if;
         Expected := W.Has_Minimum or W.Has_Length or W.Has_Writable;
      else
         if Data (W.Kind) /= W.Memory_Kind or else Data (W.Address_Bits) /=
           (if Data (W.Form) = W.Memory_24 then 24 else 32) then return False; end if;
         Expected := W.Has_Minimum or W.Has_Maximum or W.Has_Granularity or W.Has_Length or W.Has_Writable or W.Has_Encoded_Alignment;
      end if;
      if C /= Expected then return False; end if;
      if Data (W.Kind) = W.Vendor_Kind then
         if Data (W.Vendor_Type) not in 16#C0# .. 16#FF# then return False; end if;
      elsif Data (W.Vendor_Type) /= 0 then return False; end if;
      if ((C and W.Has_General) = 0 and then Data (W.General_Flags) /= 0)
        or else ((C and W.Has_Maximum) = 0 and then Data (W.Maximum) /= 0)
        or else ((C and W.Has_Granularity) = 0 and then Data (W.Granularity_Or_Alignment) /= 0)
        or else ((C and W.Has_Translation) = 0 and then Data (W.Translation) /= 0)
        or else ((C and W.Has_Attributes) = 0 and then Data (W.Attributes) /= 0)
        or else ((C and W.Has_Cache) = 0 and then Data (W.Cache) /= 0)
        or else ((C and W.Has_Writable) = 0 and then Data (W.Writable) /= 0)
        or else ((C and W.Has_Decoding) = 0 and then Data (W.Decoding_Bits) /= 0)
        or else ((C and W.Has_Encoded_Alignment) = 0 and then Data (W.Encoded_Alignment) /= 0)
      then return False; end if;
      if (C and W.Has_Source) /= 0 then
         if Data (W.Source_Name_Offset) <= Data (W.Descriptor_Offset)
           or else Data (W.Source_Name_Offset) >= Last
           or else Data (W.Source_Name_Length) /= Last - Data (W.Source_Name_Offset) - 1 then return False; end if;
      elsif Data (W.Source_Index) /= 0 or else Data (W.Source_Name_Offset) /= 0 or else Data (W.Source_Name_Length) /= 0 then return False;
      end if;
      return True;
   end Valid_Row;
   -- Call only after Valid_Row has checked the source window. The same
   -- canonical NUL-terminated tail is required in both directions.
   function Valid_Source (Item : W.Row; Source : Bytes) return Boolean is
      First, Length : Natural;
   begin
      if (Item (W.Claims) and W.Has_Source) = 0 then return True; end if;
      First := Natural (Item (W.Source_Name_Offset));
      Length := Natural (Item (W.Source_Name_Length));
      for I in 0 .. Length - 1 loop
         if Source (Source'First + First + I) = 0 then return False; end if;
      end loop;
      return Source (Source'First + First + Length) = 0;
   end Valid_Source;
   procedure Put (Data : in out Bytes; Offset : Natural; Value : W.Word) is
   begin
      for I in 0 .. Word_Bytes - 1 loop
         Data (Data'First + Offset + I) := Firmware_Tables.Byte (Interfaces.Shift_Right (Value, I * 8) and 16#FF#);
      end loop;
   end Put;
   function Get (Data : Bytes; Offset : Natural) return W.Word is
      Value : W.Word := 0;
   begin
      for I in 0 .. Word_Bytes - 1 loop Value := Value or Interfaces.Shift_Left (W.Word (Data (Data'First + Offset + I)), I * 8); end loop;
      return Value;
   end Get;
   function Sized (Rows, Source : Natural) return Boolean is
     (Source <= Natural'Last - Header_Bytes and then Rows <= (Natural'Last - Header_Bytes - Source) / Row_Bytes);
   procedure Encode (Info : Header; Items : W.Rows; Source : Bytes;
      Packet : out Bytes; Result : out Report) is
      Position : Natural := Header_Bytes;
      H : Header_Data;
   begin
      Packet := [others => 0]; Result := (others => <>);
      if not Valid_Header (Info, Items'Length, Source'Length) then Result.Status := Invalid_Header; return; end if;
      if not Sized (Items'Length, Source'Length) then return; end if;
      Result.Required_Rows := Items'Length; Result.Required_Source_Bytes := Source'Length;
      Result.Required_Packet_Bytes := Header_Bytes + Items'Length * Row_Bytes + Source'Length;
      for I in Items'Range loop
         if not Valid_Row (Items (I), I - Items'First + 1, Source'Length)
           or else not Valid_Source (Items (I), Source)
         then Result.Status := Invalid_Row; return; end if;
      end loop;
      if Result.Required_Packet_Bytes > Packet'Length then Result.Status := Insufficient_Capacity; return; end if;
      H := [Signature => Magic, Format_Version => Version, Header_Size => W.Word (Header_Words), Descriptor_Size => W.Word (Row_Words),
        State_Code => M.State_Code (Info.State), Root => Info.Root_ID, Serial => Info.Sequence,
        Coordinate_Flags => Flag (Info.Segment.Present, Segment_Present) or Flag (Info.Base_Bus.Present, Bus_Present),
        Segment_Value => Info.Segment.Value, Bus_Value => Info.Base_Bus.Value, Descriptor_Count => W.Word (Items'Length),
        Required_Count => Info.Required_Descriptors, Source_Length => W.Word (Source'Length), Failure_Reason => M.Reason_Code (Info.Reason)];
      for Field in Header_Index loop Put (Packet, Header_Index'Pos (Field) * Word_Bytes, H (Field)); end loop;
      for Item of Items loop for Field in W.Column loop Put (Packet, Position, Item (Field)); Position := Position + Word_Bytes; end loop; end loop;
      for I in Source'Range loop Packet (Packet'First + Position) := Source (I); Position := Position + 1; end loop;
      Result.Used := Position; Result.Status := Done;
   end Encode;
   procedure Decode (Data : Bytes; Info : out Header; Items : out W.Rows;
      Source : out Bytes; Result : out Report) is
      H : Header_Data; Candidate : Header;
      Count, Bytes_Count, Position, Source_Start : Natural;
      Item : W.Row;
   begin
      Info := (others => <>); Items := [others => W.Empty_Row]; Source := [others => 0]; Result := (others => <>);
      if Data'Length < Header_Bytes then return; end if;
      for Field in Header_Index loop H (Field) := Get (Data, Header_Index'Pos (Field) * Word_Bytes); end loop;
      if H (Signature) /= Magic or else H (Format_Version) /= Version
        or else H (Header_Size) /= W.Word (Header_Words) or else H (Descriptor_Size) /= W.Word (Row_Words)
        or else not M.Valid_State_Code (H (State_Code)) or else not M.Valid_Reason_Code (H (Failure_Reason))
        or else H (Coordinate_Flags) > 3 then Result.Status := Invalid_Header; return; end if;
      if H (Descriptor_Count) > W.Word ((Data'Length - Header_Bytes) / Row_Bytes) then return; end if;
      Count := Natural (H (Descriptor_Count)); Source_Start := Header_Bytes + Count * Row_Bytes;
      if H (Source_Length) /= W.Word (Data'Length - Source_Start) then return; end if;
      Bytes_Count := Natural (H (Source_Length));
      Candidate := (State => M.State_From_Code (H (State_Code)), Root_ID => H (Root), Sequence => H (Serial),
        Segment => ((H (Coordinate_Flags) and Segment_Present) /= 0, H (Segment_Value)),
        Base_Bus => ((H (Coordinate_Flags) and Bus_Present) /= 0, H (Bus_Value)),
        Required_Descriptors => H (Required_Count), Reason => M.Reason_From_Code (H (Failure_Reason)));
      if not Valid_Header (Candidate, Count, Bytes_Count) then Result.Status := Invalid_Header; return; end if;
      Position := Header_Bytes;
      for I in 1 .. Count loop
         for Field in W.Column loop Item (Field) := Get (Data, Position); Position := Position + Word_Bytes; end loop;
         if not Valid_Row (Item, I, Bytes_Count) then Result.Status := Invalid_Row; return; end if;
         if not Valid_Source (Item, Data (Data'First + Source_Start .. Data'Last))
         then Result.Status := Invalid_Row; return; end if;
      end loop;
      Result.Required_Rows := Count; Result.Required_Source_Bytes := Bytes_Count; Result.Required_Packet_Bytes := Data'Length;
      if Count > Items'Length or else Bytes_Count > Source'Length then Result.Status := Insufficient_Capacity; return; end if;
      Position := Header_Bytes;
      for I in 0 .. Count - 1 loop
         for Field in W.Column loop Items (Items'First + I) (Field) := Get (Data, Position); Position := Position + Word_Bytes; end loop;
      end loop;
      for I in 0 .. Bytes_Count - 1 loop Source (Source'First + I) := Data (Data'First + Source_Start + I); end loop;
      Info := Candidate; Result.Used := Data'Length; Result.Status := Done;
   end Decode;
end ACPI_Host_Bridge_Snapshot;
