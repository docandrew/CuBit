------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body SCSI with SPARK_Mode is
   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   --  CDB field positions (1-based; SPC-4 / SBC-3 command tables).
   Operation_Byte : constant CDB_Index := 1;
   Flags_Byte     : constant CDB_Index := 2;   --  FUA, service action
   --  INQUIRY: allocation length, bytes 4 .. 5.
   Inquiry_Allocation : constant CDB_Index := 4;
   --  REQUEST SENSE: allocation length, byte 5.
   Sense_Allocation : constant CDB_Index := 5;
   --  (10) commands: LBA bytes 3 .. 6, transfer length bytes 8 .. 9.
   LBA_10   : constant CDB_Index := 3;
   Count_10 : constant CDB_Index := 8;
   --  (16) commands: LBA bytes 3 .. 10, transfer or allocation length
   --  bytes 11 .. 14.
   LBA_16   : constant CDB_Index := 3;
   Count_16 : constant CDB_Index := 11;
   Force_Unit_Access_Flag : constant Unsigned_8 := 16#08#;

   --  Big-endian field of Width bytes holding Value.
   procedure Put
     (Data : in out CDB_Bytes; First : CDB_Index; Width : Positive;
      Value : Unsigned_64)
     with Pre => Width <= 8 and then First <= Largest_CDB - Width + 1,
          Post => (for all Index in CDB_Index =>
                     (if Index not in First .. First + Width - 1 then
                        Data (Index) = Data'Old (Index)));

   procedure Put
     (Data : in out CDB_Bytes; First : CDB_Index; Width : Positive;
      Value : Unsigned_64) is
   begin
      for Offset in 0 .. Width - 1 loop
         Data (First + Offset) := Unsigned_8
           (Shift_Right (Value, (Width - 1 - Offset) * Byte_Bits)
            and Byte_Mask);
         pragma Loop_Invariant
           (for all Index in CDB_Index =>
              (if Index not in First .. First + Width - 1 then
                 Data (Index) = Data'Loop_Entry (Index)));
      end loop;
   end Put;

   function Start (Operation : Operation_Code) return CDB_Bytes is
     ([Operation_Byte => Code (Operation), others => 0]);

   function Test_Unit_Ready return Command_Block is
     ((Operation => Test_Unit_Ready, Size => CDB_6,
       Data => Start (Test_Unit_Ready), Direction => No_Data, Expected => 0));

   function Inquiry (Allocation : Allocation_16) return Command_Block is
      Data : CDB_Bytes := Start (Inquiry);
   begin
      Put (Data, Inquiry_Allocation, 2, Unsigned_64 (Allocation));
      return (Operation => Inquiry, Size => CDB_6, Data => Data,
              Direction => From_Device,
              Expected => Transfer_Length (Allocation));
   end Inquiry;

   function Request_Sense (Allocation : Allocation_8) return Command_Block is
      Data : CDB_Bytes := Start (Request_Sense);
   begin
      Data (Sense_Allocation) := Allocation;
      return (Operation => Request_Sense, Size => CDB_6, Data => Data,
              Direction => From_Device,
              Expected => Transfer_Length (Allocation));
   end Request_Sense;

   function Read_Capacity_10 return Command_Block is
     ((Operation => Read_Capacity_10, Size => CDB_10,
       Data => Start (Read_Capacity_10), Direction => From_Device,
       Expected => Capacity_10_Bytes));

   function Read_Capacity_16 return Command_Block is
      Data : CDB_Bytes := Start (Service_Action_In_16);
   begin
      Data (Flags_Byte) := Read_Capacity_16_Action;
      Put (Data, Count_16, 4, Capacity_16_Bytes);
      return (Operation => Service_Action_In_16, Size => CDB_16,
              Data => Data, Direction => From_Device,
              Expected => Capacity_16_Bytes);
   end Read_Capacity_16;

   function Block_10
     (Operation : Operation_Code; First : Unsigned_32;
      Count : Block_Count_10; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
     with Pre  => Operation in Read_10 | Write_10
                  and then Fits (Unsigned_32 (Count), Size),
          Post => Well_Formed (Block_10'Result)
                  and then Block_10'Result.Operation = Operation
                  and then Block_10'Result.Size = CDB_10
                  and then Block_10'Result.Direction =
                    (if Operation = Read_10 then From_Device else To_Device)
                  and then Block_10'Result.Expected =
                    Transfer_Length (Count) * Size;

   function Block_10
     (Operation : Operation_Code; First : Unsigned_32;
      Count : Block_Count_10; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
   is
      Data : CDB_Bytes := Start (Operation);
   begin
      if Force_Unit_Access then
         Data (Flags_Byte) := Force_Unit_Access_Flag;
      end if;
      Put (Data, LBA_10, 4, Unsigned_64 (First));
      Put (Data, Count_10, 2, Unsigned_64 (Count));
      return (Operation => Operation, Size => CDB_10, Data => Data,
              Direction =>
                (if Operation = Read_10 then From_Device else To_Device),
              Expected => Transfer_Length (Count) * Size);
   end Block_10;

   function Block_16
     (Operation : Operation_Code; First : Unsigned_64;
      Count : Block_Count_16; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
     with Pre  => Operation in Read_16 | Write_16 and then Fits (Count, Size),
          Post => Well_Formed (Block_16'Result)
                  and then Block_16'Result.Operation = Operation
                  and then Block_16'Result.Size = CDB_16
                  and then Block_16'Result.Direction =
                    (if Operation = Read_16 then From_Device else To_Device)
                  and then Block_16'Result.Expected = Count * Size;

   function Block_16
     (Operation : Operation_Code; First : Unsigned_64;
      Count : Block_Count_16; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block
   is
      Data : CDB_Bytes := Start (Operation);
   begin
      if Force_Unit_Access then
         Data (Flags_Byte) := Force_Unit_Access_Flag;
      end if;
      Put (Data, LBA_16, 8, First);
      Put (Data, Count_16, 4, Unsigned_64 (Count));
      return (Operation => Operation, Size => CDB_16, Data => Data,
              Direction =>
                (if Operation = Read_16 then From_Device else To_Device),
              Expected => Count * Size);
   end Block_16;

   function Read_10
     (First : Unsigned_32; Count : Block_Count_10; Size : Block_Length)
      return Command_Block is
     (Block_10 (Read_10, First, Count, Size, Force_Unit_Access => False));

   function Write_10
     (First : Unsigned_32; Count : Block_Count_10; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block is
     (Block_10 (Write_10, First, Count, Size, Force_Unit_Access));

   function Read_16
     (First : Unsigned_64; Count : Block_Count_16; Size : Block_Length)
      return Command_Block is
     (Block_16 (Read_16, First, Count, Size, Force_Unit_Access => False));

   function Write_16
     (First : Unsigned_64; Count : Block_Count_16; Size : Block_Length;
      Force_Unit_Access : Boolean) return Command_Block is
     (Block_16 (Write_16, First, Count, Size, Force_Unit_Access));

   function Synchronize_Cache return Command_Block is
     ((Operation => Synchronize_Cache_10, Size => CDB_10,
       Data => Start (Synchronize_Cache_10), Direction => No_Data,
       Expected => 0));

   procedure Decode_Status
     (Value : Unsigned_8; Status : out Status_Code; Valid : out Boolean) is
   begin
      Status := Good;
      Valid := False;
      for Candidate in Status_Code loop
         if Unsigned_8 (Status_Code'Enum_Rep (Candidate)) = Value then
            Status := Candidate;
            Valid := True;
            return;
         end if;
      end loop;
   end Decode_Status;

   ---------------------------------------------------------------------------
   --  Response decoding. Positions are 1-based within the response.
   ---------------------------------------------------------------------------
   function Big_Endian (Data : Bytes) return Unsigned_64
     with Pre => Data'Length <= 8;

   function Big_Endian (Data : Bytes) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for Index in Data'Range loop
         Result := Shift_Left (Result, Byte_Bits) or Unsigned_64 (Data (Index));
      end loop;
      return Result;
   end Big_Endian;

   function Big_Endian_32 (Data : Bytes) return Unsigned_32
     with Pre => Data'Length = 4;

   function Big_Endian_32 (Data : Bytes) return Unsigned_32 is
      F : constant Positive := Data'First;
   begin
      return Shift_Left (Unsigned_32 (Data (F)), 3 * Byte_Bits)
        or Shift_Left (Unsigned_32 (Data (F + 1)), 2 * Byte_Bits)
        or Shift_Left (Unsigned_32 (Data (F + 2)), Byte_Bits)
        or Unsigned_32 (Data (F + 3));
   end Big_Endian_32;

   --  INQUIRY (SPC-4 6.6.2).
   Inquiry_Peripheral : constant := 1;
   Inquiry_Removable  : constant := 2;
   Inquiry_Version    : constant := 3;
   Inquiry_Additional : constant := 5;
   Inquiry_Vendor     : constant := 9;
   Inquiry_Product    : constant := 17;
   Inquiry_Revision   : constant := 33;
   --  Bytes after the additional-length byte in the standard data.
   Inquiry_Header_Bytes : constant := 5;
   Device_Type_Mask   : constant Unsigned_8 := 16#1F#;
   Qualifier_Shift    : constant := 5;
   Removable_Bit      : constant Unsigned_8 := 16#80#;
   Printable_First    : constant := 16#20#;
   Printable_Last     : constant := 16#7E#;

   function Text_Of (Value : Unsigned_8) return Character is
     (if Value in Printable_First .. Printable_Last
      then Character'Val (Value) else '?');

   procedure Decode_Inquiry
     (Data : Bytes; Result : out Inquiry_Data; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := False;
      if Data'Length < Standard_Inquiry_Bytes then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Standard_Inquiry_Bytes) :=
           Data (Data'First .. Data'First + (Standard_Inquiry_Bytes - 1));
      begin
         if Frame (Inquiry_Additional) <
           Standard_Inquiry_Bytes - Inquiry_Header_Bytes
         then
            return;
         end if;
         Result.Qualifier := Peripheral_Qualifier
           (Shift_Right (Frame (Inquiry_Peripheral), Qualifier_Shift));
         Result.Device_Type := Peripheral_Device_Type
           (Frame (Inquiry_Peripheral) and Device_Type_Mask);
         Result.Removable := (Frame (Inquiry_Removable) and Removable_Bit) /= 0;
         Result.Version := Frame (Inquiry_Version);
         for Index in Vendor_Text'Range loop
            Result.Vendor (Index) := Text_Of (Frame (Inquiry_Vendor + Index - 1));
         end loop;
         for Index in Product_Text'Range loop
            Result.Product (Index) :=
              Text_Of (Frame (Inquiry_Product + Index - 1));
         end loop;
         for Index in Revision_Text'Range loop
            Result.Revision (Index) :=
              Text_Of (Frame (Inquiry_Revision + Index - 1));
         end loop;
         Valid := True;
      end;
   end Decode_Inquiry;

   --  READ CAPACITY(10) (SBC-3 5.16.2) and (16) (5.15.2).
   Capacity_Last_Block  : constant := 1;
   Capacity_10_Length   : constant := 5;
   Capacity_16_Length   : constant := 9;
   Capacity_16_Exponent : constant := 14;
   Exponent_Mask        : constant Unsigned_8 := 16#0F#;

   procedure Decode_Capacity_10
     (Data : Bytes; Result : out Capacity_Data;
      Valid : out Boolean; Needs_16 : out Boolean) is
   begin
      Result := (others => <>);
      Valid := False;
      Needs_16 := False;
      if Data'Length /= Capacity_10_Bytes then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Capacity_10_Bytes) := Data;
         Last  : constant Unsigned_32 :=
           Big_Endian_32 (Frame (Capacity_Last_Block .. Capacity_Last_Block + 3));
      begin
         if Last = Capacity_10_Overflow then
            Needs_16 := True;
            return;
         end if;
         Result.Last_Block := Unsigned_64 (Last);
         Result.Length :=
           Big_Endian_32 (Frame (Capacity_10_Length .. Capacity_10_Length + 3));
         Valid := True;
      end;
   end Decode_Capacity_10;

   procedure Decode_Capacity_16
     (Data : Bytes; Result : out Capacity_Data; Valid : out Boolean) is
   begin
      Result := (others => <>);
      Valid := False;
      if Data'Length < Capacity_16_Exponent then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Capacity_16_Exponent) :=
           Data (Data'First .. Data'First + (Capacity_16_Exponent - 1));
      begin
         Result.Last_Block :=
           Big_Endian (Frame (Capacity_Last_Block .. Capacity_Last_Block + 7));
         Result.Length :=
           Big_Endian_32 (Frame (Capacity_16_Length .. Capacity_16_Length + 3));
         Result.Blocks_Per_Physical_Exponent :=
           Frame (Capacity_16_Exponent) and Exponent_Mask;
         Valid := True;
      end;
   end Decode_Capacity_16;

   --  Sense data (SPC-4 4.5.2 descriptor, 4.5.3 fixed).
   Sense_Response      : constant := 1;
   Response_Code_Mask  : constant Unsigned_8 := 16#7F#;
   Sense_Key_Mask      : constant Unsigned_8 := 16#0F#;
   Fixed_Key           : constant := 3;
   Fixed_Additional    : constant := 8;
   Fixed_Code          : constant := 13;
   Fixed_Qualifier     : constant := 14;
   --  Additional bytes that cover the code and qualifier in fixed format.
   Fixed_Minimum_Additional : constant := 10;
   Descriptor_Key       : constant := 2;
   Descriptor_Code      : constant := 3;
   Descriptor_Qualifier : constant := 4;

   procedure Decode_Sense
     (Data : Bytes; Result : out Sense_Data; Valid : out Boolean)
   is
      Response : Unsigned_8;
      Key : Unsigned_8;
   begin
      Result := (others => <>);
      Valid := False;
      if Data'Length < Descriptor_Sense_Header_Bytes then
         return;
      end if;
      Response := Data (Data'First + (Sense_Response - 1)) and Response_Code_Mask;
      if Response = Sense_Format'Enum_Rep (Fixed_Current)
        or else Response = Sense_Format'Enum_Rep (Fixed_Deferred)
      then
         if Data'Length < Fixed_Sense_Bytes then
            return;
         end if;
         declare
            Frame : constant Bytes (1 .. Fixed_Sense_Bytes) :=
              Data (Data'First .. Data'First + (Fixed_Sense_Bytes - 1));
         begin
            if Frame (Fixed_Additional) < Fixed_Minimum_Additional then
               return;
            end if;
            Result.Format :=
              (if Response = Sense_Format'Enum_Rep (Fixed_Current)
               then Fixed_Current else Fixed_Deferred);
            Key := Frame (Fixed_Key) and Sense_Key_Mask;
            Result.Code := Frame (Fixed_Code);
            Result.Qualifier := Frame (Fixed_Qualifier);
         end;
      elsif Response = Sense_Format'Enum_Rep (Descriptor_Current)
        or else Response = Sense_Format'Enum_Rep (Descriptor_Deferred)
      then
         declare
            Frame : constant Bytes (1 .. Descriptor_Sense_Header_Bytes) :=
              Data (Data'First .. Data'First + (Descriptor_Sense_Header_Bytes - 1));
         begin
            Result.Format :=
              (if Response = Sense_Format'Enum_Rep (Descriptor_Current)
               then Descriptor_Current else Descriptor_Deferred);
            Key := Frame (Descriptor_Key) and Sense_Key_Mask;
            Result.Code := Frame (Descriptor_Code);
            Result.Qualifier := Frame (Descriptor_Qualifier);
         end;
      else
         return;
      end if;
      --  Every four-bit value names a key.
      for Candidate in Sense_Key loop
         if Unsigned_8 (Sense_Key'Enum_Rep (Candidate)) = Key then
            Result.Key := Candidate;
         end if;
      end loop;
      Valid := True;
   end Decode_Sense;
end SCSI;
