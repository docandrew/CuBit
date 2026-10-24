package body USB_Optical with SPARK_Mode => On is
   use type SCSI.Data_Direction;
   use type SCSI.Operation_Code;
   use type SCSI.CDB_Size;
   use type SCSI.Peripheral_Qualifier;
   use type SCSI.Peripheral_Device_Type;

   subtype Word_Bytes is Bytes (1 .. 4);
   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   --  Command block wrapper (BOT 5.1), 1-based.
   CBW_Signature  : constant Word_Bytes := [16#55#, 16#53#, 16#42#, 16#43#];
   CBW_Tag        : constant := 5;    --  bytes 5 .. 8
   CBW_Length     : constant := 9;    --  bytes 9 .. 12
   CBW_Flags      : constant := 13;
   CBW_LUN        : constant := 14;
   CBW_CB_Length  : constant := 15;
   Data_In_Flag   : constant Unsigned_8 := 16#80#;

   --  Command status wrapper (BOT 5.2), 1-based.
   CSW_Bytes      : constant := 13;
   CSW_Signature  : constant Word_Bytes := [16#55#, 16#53#, 16#42#, 16#53#];
   CSW_Tag        : constant := 5;    --  bytes 5 .. 8
   CSW_Residue    : constant := 9;    --  bytes 9 .. 12
   CSW_Status     : constant := 13;
   Status_Failed      : constant Unsigned_8 := 1;
   Status_Phase_Error : constant Unsigned_8 := 2;

   function Little_Endian (Data : Word_Bytes) return Unsigned_32 is
     (Unsigned_32 (Data (1)) or
      Shift_Left (Unsigned_32 (Data (2)), Byte_Bits) or
      Shift_Left (Unsigned_32 (Data (3)), 2 * Byte_Bits) or
      Shift_Left (Unsigned_32 (Data (4)), 3 * Byte_Bits));

   function LE_Bytes (Value : Unsigned_32) return Word_Bytes is
     ([Unsigned_8 (Value and Byte_Mask),
       Unsigned_8 (Shift_Right (Value, Byte_Bits) and Byte_Mask),
       Unsigned_8 (Shift_Right (Value, 2 * Byte_Bits) and Byte_Mask),
       Unsigned_8 (Shift_Right (Value, 3 * Byte_Bits))]);

   function Probe (Kind : Probe_Kind) return Command is
     ((Kind => Kind, First_Block => 0, Count => 1, Format => Sector_2048));

   function Read_Request
     (First_Block : Unsigned_32; Count : Read_Block_Count;
      Format : Sector_Format := Sector_2048) return Command is
     ((Read_Blocks, First_Block, Count, Format));

   --  The device's native sector for each format.
   function Sector_Bytes (Format : Sector_Format) return SCSI.Block_Length is
     (Optical_Block_Bytes / Sectors_Per_Block (Format));

   function Command_Block (Item : Command) return SCSI.Command_Block is
     (case Item.Kind is
         when Test_Unit_Ready => SCSI.Test_Unit_Ready,
         when Inquiry => SCSI.Inquiry (SCSI.Standard_Inquiry_Bytes),
         when Request_Sense => SCSI.Request_Sense (SCSI.Fixed_Sense_Bytes),
         when Read_Capacity => SCSI.Read_Capacity_10,
         --  At most 16 ISO blocks = 64 native 512-byte sectors = 32 KiB.
         when Read_Blocks =>
            SCSI.Read_10
              (Item.First_Block * Sectors_Per_Block (Item.Format),
               SCSI.Block_Count_10
                 (Unsigned_32 (Item.Count) * Sectors_Per_Block (Item.Format)),
               Sector_Bytes (Item.Format)))
     with Post => SCSI.Well_Formed (Command_Block'Result)
                  and then Command_Block'Result.Size in SCSI.CDB_6 | SCSI.CDB_10
                  and then Command_Block'Result.Operation = Operation (Item)
                  and then Command_Block'Result.Expected = Transfer_Bytes (Item)
                  and then (Command_Block'Result.Direction = SCSI.From_Device)
                           = (Item.Kind /= Test_Unit_Ready);

   function Operation (Item : Command) return Non_Mutating is
     (case Item.Kind is
         when Test_Unit_Ready => SCSI.Test_Unit_Ready,
         when Inquiry => SCSI.Inquiry,
         when Request_Sense => SCSI.Request_Sense,
         when Read_Capacity => SCSI.Read_Capacity_10,
         when Read_Blocks => SCSI.Read_10);

   function Transfer_Bytes (Item : Command) return Unsigned_32 is
     (case Item.Kind is
         when Test_Unit_Ready => 0,
         when Inquiry => SCSI.Standard_Inquiry_Bytes,
         when Request_Sense => SCSI.Fixed_Sense_Bytes,
         when Read_Capacity => SCSI.Capacity_10_Bytes,
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
      Block : constant SCSI.Command_Block := Command_Block (Item);
      Result : Command_Wrapper := [others => 0];
   begin
      Result (1 .. 4) := CBW_Signature;
      Result (CBW_Tag .. CBW_Tag + 3) := LE_Bytes (Tag);
      Result (CBW_Length .. CBW_Length + 3) := LE_Bytes (Block.Expected);
      Result (CBW_Flags) :=
        (if Block.Direction = SCSI.From_Device then Data_In_Flag else 0);
      Result (CBW_LUN) := LUN;
      Result (CBW_CB_Length) := Unsigned_8 (SCSI.Length (Block.Size));
      for Index in 1 .. SCSI.Length (Block.Size) loop
         Result (CBW_Command_Block + Index - 1) := Block.Data (Index);
         pragma Loop_Invariant
           (Result (CBW_Command_Block) = SCSI.Code (Operation (Item)));
      end loop;
      return Result;
   end Encode;

   function Decode_Status
     (Data : Bytes; Expected_Tag, Expected_Bytes, Received : Unsigned_32)
      return Status_Result
   is
   begin
      if Data'Length /= CSW_Bytes then
         return Invalid_Status;
      end if;
      declare
         Frame : constant Bytes (1 .. CSW_Bytes) := Data;
         Residue : constant Unsigned_32 :=
           Little_Endian (Frame (CSW_Residue .. CSW_Residue + 3));
      begin
         if Frame (1 .. 4) /= CSW_Signature or else
            Little_Endian (Frame (CSW_Tag .. CSW_Tag + 3)) /= Expected_Tag
         then
            return Invalid_Status;
         elsif Frame (CSW_Status) = Status_Phase_Error then
            --  BOT explicitly requires ignoring residue after a phase error.
            return Reset_Recovery_Required;
         elsif Frame (CSW_Status) > Status_Phase_Error or else
            Residue > Expected_Bytes or else
            Received > Expected_Bytes or else
            Received < Expected_Bytes - Residue
         then
            return Invalid_Status;
         elsif Frame (CSW_Status) = Status_Failed then
            return Command_Failed;
         elsif Residue /= 0 or else Received /= Expected_Bytes then
            return Short_Data;
         else
            return Command_Passed;
         end if;
      end;
   end Decode_Status;

   function Is_Supported_Inquiry (Data : Bytes) return Boolean is
      Result : SCSI.Inquiry_Data;
      Valid : Boolean;
   begin
      SCSI.Decode_Inquiry (SCSI.Bytes (Data), Result, Valid);
      --  Connected CD/DVD or direct-access disk. The adapter must also
      --  validate ISO content before selecting a disk as a live volume.
      return Valid
        and then Result.Qualifier = SCSI.Unit_Connected
        and then Result.Device_Type in
          SCSI.Direct_Access_Device | SCSI.MMC_Device;
   end Is_Supported_Inquiry;

   procedure Decode_Capacity
     (Data : Bytes; Block_Count : out Unsigned_64;
      Result : out Capacity_Result; Format : out Sector_Format)
   is
      Capacity : SCSI.Capacity_Data;
      Valid, Needs_16 : Boolean;
   begin
      Block_Count := 0;
      Format := Sector_2048;
      SCSI.Decode_Capacity_10 (SCSI.Bytes (Data), Capacity, Valid, Needs_16);
      if Needs_16 then
         Result := Capacity_16_Required;
         return;
      elsif not Valid then
         Result := Capacity_Truncated;
         return;
      elsif Capacity.Length /= Sector_Bytes (Sector_512)
        and then Capacity.Length /= Sector_Bytes (Sector_2048)
      then
         Result := Unsupported_Block_Size;
         return;
      end if;
      Format := (if Capacity.Length = Sector_Bytes (Sector_512)
                 then Sector_512 else Sector_2048);
      Block_Count := (Capacity.Last_Block + 1) /
        Unsigned_64 (Sectors_Per_Block (Format));
      Result := (if Block_Count = 0 then Unsupported_Block_Size
                 else Capacity_Valid);
   end Decode_Capacity;

   function Decode_Sense (Data : Bytes) return Sense_Result is
      Sense : SCSI.Sense_Data;
      Valid : Boolean;
   begin
      SCSI.Decode_Sense (SCSI.Bytes (Data), Sense, Valid);
      if not Valid then
         return Sense_Invalid;
      end if;
      return (case Sense.Key is
                 when SCSI.Not_Ready => Sense_Not_Ready,
                 when SCSI.Unit_Attention => Sense_Unit_Attention,
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
