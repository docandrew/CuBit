pragma Ada_2022;

package body GPIO_Bootstrap_Payload with SPARK_Mode is

   subtype Width is Positive range 1 .. 8;

   --  The little-endian integer of Count bytes at From.
   function Get (Bytes : Byte_Array; From : Natural; Count : Width) return Unsigned_64
   with Pre => Bytes'First = 0 and then From <= Bytes'Last
               and then Count - 1 <= Bytes'Last - From
   is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Count - 1 loop
         Value := Shift_Left (Value, 8) or Unsigned_64 (Bytes (From + I));
      end loop;
      return Value;
   end Get;

   --  The same, for a 4-byte field.
   function Get_32 (Bytes : Byte_Array; From : Natural) return Unsigned_32
   with Pre => Bytes'First = 0 and then From <= Bytes'Last and then 3 <= Bytes'Last - From
   is
      Value : Unsigned_32 := 0;
   begin
      for I in reverse 0 .. 3 loop
         Value := Shift_Left (Value, 8) or Unsigned_32 (Bytes (From + I));
      end loop;
      return Value;
   end Get_32;

   procedure Put (Bytes : in out Byte_Array; From : Natural; Count : Width; Value : Unsigned_64)
   with Pre => Bytes'First = 0 and then From <= Bytes'Last
               and then Count - 1 <= Bytes'Last - From
   is
      V : Unsigned_64 := Value;
   begin
      for I in 0 .. Count - 1 loop
         Bytes (From + I) := Unsigned_8 (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end Put;

   function Char (B : Unsigned_8) return Character is (Character'Val (B));
   function Byte (C : Character) return Unsigned_8 is (Unsigned_8 (Character'Pos (C)));

   procedure Decode (Bytes : Byte_Array; R : out Request; Result : out Decode_Result) is

      procedure Fail (At_Offset : Payload_Length) is
      begin
         Result := (Status => Malformed, Offset => At_Offset);
      end Fail;

      Count, Segments : Unsigned_64;
      Word : Unsigned_64;
   begin
      R := (others => <>);
      Result := (Status => Malformed, Offset => 0);
      if Bytes'Length < Header_Bytes then
         Fail (Bytes'Length);
         return;
      end if;
      if Get (Bytes, Version_Offset, 4) /= Format_Version then
         Fail (Version_Offset);
         return;
      end if;
      Count := Get (Bytes, Span_Count_Offset, 1);
      if Count not in 1 .. Max_Spans then
         Fail (Span_Count_Offset);
         return;
      elsif Bytes'Length /= Encoded_Length (Span_Count (Count)) then
         Fail (Span_Count_Offset);
         return;
      end if;
      R.Spans := Span_Count (Count);
      Segments := Get (Bytes, Path_Count_Offset, 1);
      if Segments not in 1 .. Max_Path_Segments then
         Fail (Path_Count_Offset);
         return;
      end if;
      R.Segments := Path_Count (Segments);

      Word := Get (Bytes, UID_Kind_Offset, 1);
      if Word = UID_Kind'Enum_Rep (No_UID) then
         R.UID_Type := No_UID;
      elsif Word = UID_Kind'Enum_Rep (Integer_UID) then
         R.UID_Type := Integer_UID;
      elsif Word = UID_Kind'Enum_Rep (String_UID) then
         R.UID_Type := String_UID;
      else
         Fail (UID_Kind_Offset);
         return;
      end if;
      Word := Get (Bytes, STA_Source_Offset, 1);
      if Word = STA_Source'Enum_Rep (Evaluated_STA) then
         R.STA_From := Evaluated_STA;
      elsif Word = STA_Source'Enum_Rep (Default_STA) then
         R.STA_From := Default_STA;
      else
         Fail (STA_Source_Offset);
         return;
      end if;

      for I in HID_Text'Range loop
         R.HID (I) := Char (Bytes (HID_Offset + I - 1));
      end loop;
      if not Valid_HID (R.HID) then
         Fail (HID_Offset);
         return;
      end if;
      R.UID := Get (Bytes, UID_Offset, UID_Bytes);
      if not Valid_UID (R.UID_Type, R.UID) then
         Fail (UID_Offset);
         return;
      end if;
      R.STA := Get_32 (Bytes, STA_Offset);
      if not Valid_STA (R.STA_From, R.STA) then
         Fail (STA_Offset);
         return;
      end if;
      if Get (Bytes, Phase_Offset, 1) /= Phase'Enum_Rep (Before_Device_INI) then
         Fail (Phase_Offset);
         return;
      end if;
      R.Observed := Before_Device_INI;

      --  The interrupt.
      Word := Get (Bytes, IRQ_Present_Offset, 1);
      if Word > 1 then
         Fail (IRQ_Present_Offset);
         return;
      end if;
      R.IRQ.Present := Word = 1;
      Word := Get (Bytes, Trigger_Offset, 1);
      if Word > Trigger'Enum_Rep (Edge) then
         Fail (Trigger_Offset);
         return;
      end if;
      R.IRQ.Mode := (if Word = Trigger'Enum_Rep (Edge) then Edge else Level);
      Word := Get (Bytes, Polarity_Offset, 1);
      if Word > Polarity'Enum_Rep (Active_Low) then
         Fail (Polarity_Offset);
         return;
      end if;
      R.IRQ.Level_Of := (if Word = Polarity'Enum_Rep (Active_Low) then Active_Low else Active_High);
      Word := Get (Bytes, Sharing_Offset, 1);
      if Word > Sharing'Enum_Rep (Shared) then
         Fail (Sharing_Offset);
         return;
      end if;
      R.IRQ.Share := (if Word = Sharing'Enum_Rep (Shared) then Shared else Exclusive);
      Word := Get (Bytes, Wake_Offset, 1);
      if Word > Wake'Enum_Rep (Wake_Capable) then
         Fail (Wake_Offset);
         return;
      end if;
      R.IRQ.Wakes := (if Word = Wake'Enum_Rep (Wake_Capable) then Wake_Capable else Not_Wake_Capable);
      R.IRQ.GSI := Get_32 (Bytes, GSI_Offset);
      if not Valid_Interrupt (R.IRQ) then
         Fail (IRQ_Present_Offset);
         return;
      end if;

      --  Provenance: the defining table and the epoch.
      for I in Table_Signature_Text'Range loop
         R.Table (I) := Char (Bytes (Table_Signature_Offset + I - 1));
      end loop;
      for I in Table_ID_Text'Range loop
         R.Table_ID (I) := Char (Bytes (Table_ID_Offset + I - 1));
      end loop;
      if not Valid_Table (R.Table, R.Table_ID) then
         Fail (Table_Signature_Offset);
         return;
      end if;
      R.Epoch := Get (Bytes, Epoch_Offset, 8);

      --  The namespace path.
      for S in Path_Index loop
         for C in Name_Segment'Range loop
            R.Path (S) (C) := Char (Bytes (Path_Offset + (S - 1) * Name_Segment_Bytes + C - 1));
         end loop;
      end loop;
      if not Valid_Path (R.Segments, R.Path) then
         Fail (Path_Offset);
         return;
      end if;

      --  The spans, in _CRS order.
      for I in 1 .. R.Spans loop
         pragma Loop_Invariant (R.Spans >= 1 and then Bytes'Length = Encoded_Length (R.Spans));
         pragma Loop_Invariant (Unused_Spans_Empty (R) and then R.Spans = Span_Count (Count)
                                and then Valid_Path (R.Segments, R.Path)
                                and then Valid_HID (R.HID)
                                and then Valid_UID (R.UID_Type, R.UID)
                                and then Valid_STA (R.STA_From, R.STA)
                                and then Valid_Interrupt (R.IRQ)
                                and then Valid_Table (R.Table, R.Table_ID)
                                and then (for all J in I .. Span_Index'Last =>
                                            R.Items (J) = (others => <>)));
         declare
            At_Span : constant Natural := Header_Bytes + (I - 1) * Span_Bytes;
            Base    : constant Unsigned_64 := Get (Bytes, At_Span + Span_Base_Offset, 8);
            Length  : constant Unsigned_64 := Get (Bytes, At_Span + Span_Length_Offset, 8);
            Kind    : constant Unsigned_64 := Get (Bytes, At_Span + Span_Descriptor_Offset, 1);
            Writes  : constant Unsigned_64 := Get (Bytes, At_Span + Span_Writable_Offset, 1);
            From    : Descriptor;
         begin
            if Kind = Descriptor'Enum_Rep (Memory32_Fixed) then
               From := Memory32_Fixed;
            elsif Kind = Descriptor'Enum_Rep (Memory32) then
               From := Memory32;
            elsif Kind = Descriptor'Enum_Rep (DWord_Address) then
               From := DWord_Address;
            elsif Kind = Descriptor'Enum_Rep (QWord_Address) then
               From := QWord_Address;
            else
               Fail (At_Span + Span_Descriptor_Offset);
               return;
            end if;
            if Writes > 1 then
               Fail (At_Span + Span_Writable_Offset);
               return;
            end if;
            R.Items (I) := (Base => Base, Length => Length, From => From, Writable => Writes = 1);
         end;
      end loop;
      pragma Assert (R.Spans >= 1 and then Bytes'Length = Encoded_Length (R.Spans));
      Result := (Status => Decoded, Offset => 0);
   end Decode;

   procedure Encode (R : Request; Bytes : out Byte_Array) is
   begin
      Bytes := [others => 0];
      Put (Bytes, Version_Offset, 4, Format_Version);
      Put (Bytes, Span_Count_Offset, 1, Unsigned_64 (R.Spans));
      Put (Bytes, Path_Count_Offset, 1, Unsigned_64 (R.Segments));
      Put (Bytes, UID_Kind_Offset, 1, UID_Kind'Enum_Rep (R.UID_Type));
      Put (Bytes, STA_Source_Offset, 1, STA_Source'Enum_Rep (R.STA_From));
      for I in HID_Text'Range loop
         Bytes (HID_Offset + I - 1) := Byte (R.HID (I));
      end loop;
      Put (Bytes, UID_Offset, UID_Bytes, R.UID);
      Put (Bytes, STA_Offset, 4, Unsigned_64 (R.STA));
      Put (Bytes, Phase_Offset, 1, Phase'Enum_Rep (R.Observed));
      Put (Bytes, IRQ_Present_Offset, 1, (if R.IRQ.Present then 1 else 0));
      Put (Bytes, Trigger_Offset, 1, Trigger'Enum_Rep (R.IRQ.Mode));
      Put (Bytes, Polarity_Offset, 1, Polarity'Enum_Rep (R.IRQ.Level_Of));
      Put (Bytes, Sharing_Offset, 1, Sharing'Enum_Rep (R.IRQ.Share));
      Put (Bytes, Wake_Offset, 1, Wake'Enum_Rep (R.IRQ.Wakes));
      Put (Bytes, GSI_Offset, 4, Unsigned_64 (R.IRQ.GSI));
      for I in Table_Signature_Text'Range loop
         Bytes (Table_Signature_Offset + I - 1) := Byte (R.Table (I));
      end loop;
      for I in Table_ID_Text'Range loop
         Bytes (Table_ID_Offset + I - 1) := Byte (R.Table_ID (I));
      end loop;
      Put (Bytes, Epoch_Offset, 8, R.Epoch);
      for S in Path_Index loop
         for C in Name_Segment'Range loop
            Bytes (Path_Offset + (S - 1) * Name_Segment_Bytes + C - 1) := Byte (R.Path (S) (C));
         end loop;
      end loop;
      for I in 1 .. R.Spans loop
         declare
            At_Span : constant Natural := Header_Bytes + (I - 1) * Span_Bytes;
         begin
            Put (Bytes, At_Span + Span_Base_Offset, 8, R.Items (I).Base);
            Put (Bytes, At_Span + Span_Length_Offset, 8, R.Items (I).Length);
            Put (Bytes, At_Span + Span_Descriptor_Offset, 1, Descriptor'Enum_Rep (R.Items (I).From));
            Put (Bytes, At_Span + Span_Writable_Offset, 1, (if R.Items (I).Writable then 1 else 0));
         end;
      end loop;
   end Encode;

end GPIO_Bootstrap_Payload;
