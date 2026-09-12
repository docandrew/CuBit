with Ada.Text_IO;
with Interfaces; use Interfaces;
with USB_Optical; use USB_Optical;

procedure Main is
   Status : Bytes (1 .. 13) :=
     [16#55#, 16#53#, 16#42#, 16#53#, 16#78#, 16#56#, 16#34#, 16#12#,
      0, 0, 0, 0, 0];
   Tag : constant Unsigned_32 := 16#1234_5678#;
   Capacity : Bytes (1 .. 8) := [0, 0, 0, 99, 0, 0, 8, 0];
   Count : Unsigned_64;
   Result : Capacity_Result;
   LUN : Logical_Unit;
   Valid : Boolean;
   Inquiry_Data : Bytes (1 .. 36) := [others => 0];
   Sense : Bytes (1 .. 18) := [others => 0];
   Empty : constant Bytes (1 .. 0) := [];
begin
   pragma Assert (Read_Fits (0, 1, 1));
   pragma Assert (Read_Fits (99, 1, 100));
   pragma Assert (not Read_Fits (100, 1, 100));
   pragma Assert (not Read_Fits (0, 1, 0));
   pragma Assert (not Read_Fits (99, 2, 100));
   pragma Assert (not Read_Fits (Unsigned_32'Last, 16, Unsigned_64'Last));
   pragma Assert (not Read_Fits
     (Unsigned_32'Last, 1, Unsigned_64 (Unsigned_32'Last)));
   for Unit in Logical_Unit loop
      for Kind in Command_Kind loop
         declare
            Item : constant Command :=
              (if Kind = Read_Blocks then Read_Request (16#1234_5678#, 16)
               else Probe (Kind));
            Frame : constant Command_Wrapper := Encode (Item, Tag, Unit);
         begin
            pragma Assert (Frame (1 .. 8) =
              [16#55#, 16#53#, 16#42#, 16#43#,
               16#78#, 16#56#, 16#34#, 16#12#]);
            pragma Assert (Frame (14) = Unit);
            pragma Assert (Frame (25 .. 31) = [0, 0, 0, 0, 0, 0, 0]);
            pragma Assert
              (Frame (13) = (if Kind = Test_Unit_Ready then 0 else 128));
            if Kind = Read_Blocks then
               pragma Assert (Frame (9 .. 12) = [0, 128, 0, 0]);
               pragma Assert (Frame (15 .. 24) =
                 [10, 16#28#, 0, 16#12#, 16#34#, 16#56#, 16#78#, 0, 0, 16]);
               pragma Assert (Transfer_Bytes (Item) = 32768);
            end if;
         end;
      end loop;
   end loop;

   pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Command_Passed);
   pragma Assert (Decode_Status (Status, Tag + 1, 2048, 2048) = Invalid_Status);
   pragma Assert (Decode_Status (Status, Tag, 2048, 2047) = Invalid_Status);
   pragma Assert (Decode_Status (Status, Tag, 2048, 2049) = Invalid_Status);
   pragma Assert (Decode_Status (Status, Tag, 0, 0) = Command_Passed);
   for Length in 0 .. 12 loop
      pragma Assert
        (Decode_Status (Status (1 .. Length), Tag, 2048, 2048) = Invalid_Status);
   end loop;
   pragma Assert
     (Decode_Status (Status & Unsigned_8'(0), Tag, 2048, 2048) = Invalid_Status);
   Status (1) := 0;
   pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Invalid_Status);
   Status (1) := 16#55#;
   Status (9) := 1;
   pragma Assert (Decode_Status (Status, Tag, 2048, 2047) = Short_Data);
   pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Short_Data);
   pragma Assert (Decode_Status (Status, Tag, 2048, 2046) = Invalid_Status);
   Status (10) := 8;
   pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Invalid_Status);
   Status (13) := 2;
   pragma Assert
     (Decode_Status (Status, Tag, 2048, 0) = Reset_Recovery_Required);
   Status (9 .. 12) := [0, 0, 0, 0];
   Status (13) := 1;
   pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Command_Failed);
   for Invalid in Unsigned_8 range 3 .. 255 loop
      Status (13) := Invalid;
      pragma Assert (Decode_Status (Status, Tag, 2048, 2048) = Invalid_Status);
   end loop;

   Decode_Capacity (Capacity, Count, Result);
   pragma Assert (Result = Capacity_Valid and Count = 100);
   Capacity (4) := 0;
   Decode_Capacity (Capacity, Count, Result);
   pragma Assert (Result = Capacity_Valid and Count = 1);
   Capacity (1 .. 4) := [255, 255, 255, 254];
   Decode_Capacity (Capacity, Count, Result);
   pragma Assert (Result = Capacity_Valid and Count = 16#FFFF_FFFF#);
   Capacity (4) := 255;
   Decode_Capacity (Capacity, Count, Result);
   pragma Assert (Result = Capacity_16_Required and Count = 0);
   Capacity (1 .. 4) := [0, 0, 0, 99];
   Capacity (7) := 2;
   Decode_Capacity (Capacity, Count, Result);
   pragma Assert (Result = Unsupported_Block_Size and Count = 0);
   for Length in 0 .. 7 loop
      Decode_Capacity (Capacity (1 .. Length), Count, Result);
      pragma Assert (Result = Capacity_Truncated and Count = 0);
   end loop;

   Inquiry_Data (1) := 5;
   Inquiry_Data (5) := 31;
   pragma Assert (Is_Optical_Inquiry (Inquiry_Data));
   for First_Byte in Unsigned_8 loop
      Inquiry_Data (1) := First_Byte;
      pragma Assert
        (Is_Optical_Inquiry (Inquiry_Data) = (First_Byte = 5));
   end loop;
   pragma Assert (not Is_Optical_Inquiry (Empty));

   Sense (1) := 16#70#;
   Sense (3) := 2;
   Sense (8) := 10;
   pragma Assert (Decode_Sense (Sense) = Sense_Not_Ready);
   Sense (3) := 6;
   pragma Assert (Decode_Sense (Sense) = Sense_Unit_Attention);
   Sense (3) := 0;
   pragma Assert (Decode_Sense (Sense) = Sense_Other);
   Sense (1) := 16#72#;
   Sense (2) := 6;
   pragma Assert (Decode_Sense (Sense (1 .. 8)) = Sense_Unit_Attention);
   pragma Assert (Decode_Sense (Empty) = Sense_Invalid);

   for Value in Unsigned_8 loop
      Decode_Max_LUN (Control_Success, [Value], LUN, Valid);
      pragma Assert (Valid = (Value <= 15));
      if Valid then
         pragma Assert (LUN = Value);
      end if;
   end loop;
   Decode_Max_LUN (Control_Stall, Empty, LUN, Valid);
   pragma Assert (Valid and LUN = 0);
   Decode_Max_LUN (Control_Error, [0], LUN, Valid);
   pragma Assert (not Valid);
   Decode_Max_LUN (Control_Success, Empty, LUN, Valid);
   pragma Assert (not Valid);
   Decode_Max_LUN (Control_Success, [0, 0], LUN, Valid);
   pragma Assert (not Valid);

   --  Decoders must not rely on a particular caller array origin.
   declare
      Shifted : constant Bytes (101 .. 113) :=
        [16#55#, 16#53#, 16#42#, 16#53#, 16#78#, 16#56#, 16#34#, 16#12#,
         0, 0, 0, 0, 0];
   begin
      pragma Assert (Decode_Status (Shifted, Tag, 2048, 2048) = Command_Passed);
   end;
   Ada.Text_IO.Put_Line ("USB-OPTICAL: PASS read-only BOT/SCSI wire checks");
end Main;
