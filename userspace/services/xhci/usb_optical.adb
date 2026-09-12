package body USB_Optical with SPARK_Mode => On is
   subtype Word_Bytes is Bytes (1 .. 4);

   function Little_Endian (Data : Word_Bytes) return Unsigned_32 is
     (Unsigned_32 (Data (1)) or
      Shift_Left (Unsigned_32 (Data (2)), 8) or
      Shift_Left (Unsigned_32 (Data (3)), 16) or
      Shift_Left (Unsigned_32 (Data (4)), 24));

   function Big_Endian (Data : Word_Bytes) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Data (1)), 24) or
      Shift_Left (Unsigned_32 (Data (2)), 16) or
      Shift_Left (Unsigned_32 (Data (3)), 8) or
      Unsigned_32 (Data (4)));

   function LE_Bytes (Value : Unsigned_32) return Word_Bytes is
     ([Unsigned_8 (Value and 255),
       Unsigned_8 (Shift_Right (Value, 8) and 255),
       Unsigned_8 (Shift_Right (Value, 16) and 255),
       Unsigned_8 (Shift_Right (Value, 24))]);

   function BE_Bytes (Value : Unsigned_32) return Word_Bytes is
     ([Unsigned_8 (Shift_Right (Value, 24)),
       Unsigned_8 (Shift_Right (Value, 16) and 255),
       Unsigned_8 (Shift_Right (Value, 8) and 255),
       Unsigned_8 (Value and 255)]);

   function Probe (Kind : Probe_Kind) return Command is
     ((Kind => Kind, First_Block => 0, Count => 1));

   function Read_Request
     (First_Block : Unsigned_32; Count : Read_Block_Count) return Command is
     ((Read_Blocks, First_Block, Count));

   function Transfer_Bytes (Item : Command) return Unsigned_32 is
     (case Item.Kind is
         when Test_Unit_Ready => 0,
         when Inquiry => 36,
         when Request_Sense => 18,
         when Read_Capacity => 8,
         when Read_Blocks => Unsigned_32 (Item.Count) * Optical_Block_Bytes);

   function Read_Fits
     (First_Block : Unsigned_32; Count : Read_Block_Count;
      Media_Blocks : Unsigned_64) return Boolean
   is
     (Media_Blocks in 1 .. Unsigned_64 (Unsigned_32'Last) and then
      Unsigned_64 (First_Block) + Unsigned_64 (Count) <= Media_Blocks);

   function Encode
     (Item : Command; Tag : Unsigned_32; LUN : Logical_Unit)
      return Command_Wrapper
   is
      Result : Command_Wrapper := [others => 0];
   begin
      Result (1 .. 4) := [16#55#, 16#53#, 16#42#, 16#43#];
      Result (5 .. 8) := LE_Bytes (Tag);
      Result (9 .. 12) := LE_Bytes (Transfer_Bytes (Item));
      Result (13) := (if Item.Kind = Test_Unit_Ready then 0 else 16#80#);
      Result (14) := LUN;
      Result (15) :=
        (if Item.Kind in Read_Capacity | Read_Blocks then 10 else 6);
      case Item.Kind is
         when Test_Unit_Ready => Result (16) := 16#00#;
         when Inquiry =>
            Result (16) := 16#12#;
            Result (20) := 36;
         when Request_Sense =>
            Result (16) := 16#03#;
            Result (20) := 18;
         when Read_Capacity => Result (16) := 16#25#;
         when Read_Blocks =>
            Result (16) := 16#28#;
            Result (18 .. 21) := BE_Bytes (Item.First_Block);
            Result (23) := Unsigned_8 (Shift_Right (Item.Count, 8));
            Result (24) := Unsigned_8 (Item.Count and 255);
      end case;
      return Result;
   end Encode;

   function Decode_Status
     (Data : Bytes; Expected_Tag, Expected_Bytes, Received : Unsigned_32)
      return Status_Result
   is
   begin
      if Data'Length /= 13 then
         return Invalid_Status;
      end if;
      declare
         Frame : constant Bytes (1 .. 13) := Data;
         Residue : constant Unsigned_32 := Little_Endian (Frame (9 .. 12));
      begin
         if Frame (1 .. 4) /= [16#55#, 16#53#, 16#42#, 16#53#] or else
            Little_Endian (Frame (5 .. 8)) /= Expected_Tag
         then
            return Invalid_Status;
         elsif Frame (13) = 2 then
            --  BOT explicitly requires ignoring residue after a phase error.
            return Reset_Recovery_Required;
         elsif Frame (13) > 2 or else Residue > Expected_Bytes or else
            Received > Expected_Bytes or else
            Received < Expected_Bytes - Residue
         then
            return Invalid_Status;
         elsif Frame (13) = 1 then
            return Command_Failed;
         elsif Residue /= 0 or else Received /= Expected_Bytes then
            return Short_Data;
         else
            return Command_Passed;
         end if;
      end;
   end Decode_Status;

   function Is_Optical_Inquiry (Data : Bytes) return Boolean is
   begin
      if Data'Length < 36 then
         return False;
      end if;
      declare
         Header : constant Bytes (1 .. 5) := Data (Data'First .. Data'First + 4);
      begin
         --  Peripheral qualifier zero, CD/DVD device type 5. Do not accept a
         --  disconnected LUN, disk, or vendor-specific device as our boot CD.
         return Header (1) = 5 and then Header (5) >= 31;
      end;
   end Is_Optical_Inquiry;

   procedure Decode_Capacity
     (Data : Bytes; Block_Count : out Unsigned_64;
      Result : out Capacity_Result)
   is
   begin
      Block_Count := 0;
      if Data'Length /= 8 then
         Result := Capacity_Truncated;
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. 8) := Data;
         Last_Block : constant Unsigned_32 := Big_Endian (Frame (1 .. 4));
      begin
         if Last_Block = Unsigned_32'Last then
            Result := Capacity_16_Required;
         elsif Big_Endian (Frame (5 .. 8)) /= Optical_Block_Bytes then
            Result := Unsupported_Block_Size;
         else
            Block_Count := Unsigned_64 (Last_Block) + 1;
            Result := Capacity_Valid;
         end if;
      end;
   end Decode_Capacity;

   function Decode_Sense (Data : Bytes) return Sense_Result is
      Key : Unsigned_8;
   begin
      if Data'Length < 8 then
         return Sense_Invalid;
      end if;
      declare
         Header : constant Bytes (1 .. 8) := Data (Data'First .. Data'First + 7);
      begin
         case Header (1) and 16#7F# is
            when 16#70# | 16#71# =>
               if Data'Length < 18 or else Header (8) < 10 then
                  return Sense_Invalid;
               end if;
               Key := Header (3) and 15;
            when 16#72# | 16#73# => Key := Header (2) and 15;
            when others => return Sense_Invalid;
         end case;
      end;
      return (case Key is
                 when 2 => Sense_Not_Ready,
                 when 6 => Sense_Unit_Attention,
                 when others => Sense_Other);
   end Decode_Sense;

   procedure Decode_Max_LUN
     (Outcome : Control_Result; Data : Bytes;
      Last_LUN : out Logical_Unit; Valid : out Boolean)
   is
   begin
      Last_LUN := 0;
      Valid := False;
      case Outcome is
         when Control_Stall => Valid := True;
         when Control_Error => null;
         when Control_Success =>
            if Data'Length = 1 and then Data (Data'First) <= Logical_Unit'Last then
               Last_LUN := Data (Data'First);
               Valid := True;
            end if;
      end case;
   end Decode_Max_LUN;
end USB_Optical;
