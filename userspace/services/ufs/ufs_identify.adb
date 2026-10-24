------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with System;
with SCSI; use SCSI;
with UFS_UPIU; use UFS_UPIU;
with UFS_Descriptors; use UFS_Descriptors;
with UFS_Commands; use UFS_Commands;
with UFS_Controller;
with UFS_Layout;
with UFS_Log; use UFS_Log;
with UFS_Text; use UFS_Text;
with Partition_Tables;

package body UFS_Identify is
   --  Deadlines, in microseconds. NOP OUT: Intel ADL needs 200 ms
   --  (Linux ufshcd-pci nop_out_timeout); fDeviceInit may take a second.
   NOP_Microseconds        : constant := 200_000;
   NOP_Attempts            : constant := 10;
   Query_Microseconds      : constant := 1_500_000;
   Device_Init_Attempts    : constant := 1_000;
   SCSI_Microseconds       : constant := 5_000_000;
   Ready_Attempts          : constant := 10;
   --  REQUEST SENSE allocation: fixed sense data rounded up to whole
   --  32-bit words (UFSHCI region byte counts are word multiples).
   Sense_Allocation        : constant := 20;
   --  Units examined at most.
   Units_Examined          : constant := 8;

   function Version_Text (BCD : Unsigned_16) return String is
     (Hex (Unsigned_64 (BCD / 2 ** 8), 1) & "." &
      Hex (Unsigned_64 ((BCD / 2 ** 4) mod 2 ** 4), 1));

   function Quoted (Text : String) return String is (" """ & Text & """");

   procedure Initialize_Device (Ready : out Boolean) is
      Answered, Valid, Pending : Boolean := False;
      Attempts : Natural := 0;
   begin
      for Attempt in 1 .. NOP_Attempts loop
         Attempts := Attempt;
         Run_NOP (NOP_Microseconds, Answered);
         exit when Answered;
      end loop;
      if not Answered then
         Failed (NOP, "no-nop-in",
                 Field ("tries", Unsigned_64 (Attempts)) & Last_Request_Failure);
         Ready := False;
         return;
      end if;
      Passed (NOP, Field ("tries", Unsigned_64 (Attempts)));
      Run_Flag (Set_Flag, Device_Init, Query_Microseconds, Pending, Valid);
      if not Valid then
         Failed (Device_Init, "set-flag", Last_Request_Failure);
         Ready := False;
         return;
      end if;
      Pending := True;
      Attempts := 0;
      for Attempt in 1 .. Device_Init_Attempts loop
         Attempts := Attempt;
         Run_Flag (Read_Flag, Device_Init, Query_Microseconds, Pending, Valid);
         exit when not Valid or else not Pending;
      end loop;
      Ready := Valid and then not Pending;
      if Ready then
         Passed (Device_Init, Field ("polls", Unsigned_64 (Attempts)));
      elsif not Valid then
         Failed (Device_Init, "read-flag", Last_Request_Failure);
      else
         Failed (Device_Init, "still-set",
                 Field ("polls", Unsigned_64 (Attempts)));
      end if;
   end Initialize_Device;

   procedure Describe_Device (Units : out Natural; Ready : out Boolean) is
      Data : Descriptor_Buffer;
      Length : Natural;
      Valid : Boolean;
      Device : Device_Information;
      Geometry_Info : Geometry_Information;
      Name : Text;
   begin
      Units := 0;
      Run_Read_Descriptor (Device_Descriptor, 0, Query_Microseconds, Data,
                           Length, Valid);
      if not Valid then
         Failed (Device_Descriptor, "query", Last_Request_Failure);
         Ready := False;
         return;
      end if;
      Decode_Device (Data (1 .. Length), Device, Valid);
      if not Valid then
         Failed (Device_Descriptor, "malformed",
                 Field ("len", Unsigned_64 (Length)) &
                 Hex_Field ("blen", Unsigned_64 (Data (1)), 2) &
                 Hex_Field ("idn", Unsigned_64 (Data (2)), 2));
         Ready := False;
         return;
      end if;
      Units := Natural (Device.Logical_Units);
      Name := (others => <>);
      if Device.Product_Name /= 0 then
         Run_Read_Descriptor (String_Descriptor, Device.Product_Name,
                              Query_Microseconds, Data, Length, Valid);
         if Valid then
            Decode_String (Data (1 .. Length), Name, Valid);
         end if;
      end if;
      Passed (Device_Descriptor,
              " spec=" & Version_Text (Device.Specification) &
              Hex_Field ("mfr", Unsigned_64 (Device.Manufacturer), 4) &
              Field ("luns", Unsigned_64 (Device.Logical_Units)) &
              Quoted (Name.Value (1 .. Name.Length)));
      Ready := True;
      Run_Read_Descriptor (Geometry_Descriptor, 0, Query_Microseconds, Data,
                           Length, Valid);
      if not Valid then
         Failed (Geometry, "query", Last_Request_Failure);
         return;
      end if;
      Decode_Geometry (Data (1 .. Length), Geometry_Info, Valid);
      if Valid then
         Passed (Geometry,
                 " raw=" & Size (Geometry_Info.Raw_Capacity_Sectors *
                                 Partition_Tables.Sector_Bytes) &
                 Field ("maxlun", Unsigned_64 (Geometry_Info.Maximum_Units)));
      else
         Failed (Geometry, "malformed", Field ("len", Unsigned_64 (Length)));
      end if;
   end Describe_Device;

   --  The first enabled unit is served.
   procedure Choose_Unit
     (Units : Natural; Chosen : out User_Unit; Found : out Boolean)
   is
      Data : Descriptor_Buffer;
      Length : Natural;
      Valid : Boolean;
      Unit : Unit_Information;
      Enabled : Natural := 0;
   begin
      Chosen := User_Unit'First;
      Found := False;
      for Index in 0 .. Natural'Min (Units, Units_Examined) - 1 loop
         Run_Read_Descriptor (Unit_Descriptor, Descriptor_Index (Index),
                              Query_Microseconds, Data, Length, Valid);
         if Valid then
            Decode_Unit (Data (1 .. Length), Unsigned_8 (Index), Unit, Valid);
         end if;
         if Valid and then Unit.Enabled then
            Enabled := Enabled + 1;
            if not Found then
               Chosen := User_Unit (Index);
               Found := True;
               Passed (LUN,
                       Field ("lun", Unsigned_64 (Index)) &
                       Field ("blocks", Unit.Block_Count) &
                       Field ("bs", 2 ** Natural (Unit.Block_Exponent)) &
                       (if Unit.Write_Protect /= 0
                        then Hex_Field ("wp", Unsigned_64 (Unit.Write_Protect), 2)
                        else ""));
            end if;
         end if;
      end loop;
      if not Found then
         Failed (LUN, "none-enabled", Field ("luns", Unsigned_64 (Units)));
      elsif Enabled > 1 then
         Note ("lun: " & Decimal (Unsigned_64 (Enabled)) &
               " enabled, serving the first");
      end if;
   end Choose_Unit;

   --  TEST UNIT READY until the unit stops reporting a reset or start-up.
   procedure Wait_Until_Ready (Unit : Logical_Unit; Ready : out Boolean) is
      Outcome : SCSI_Outcome;
      Sense : constant Command_Block := Request_Sense (Sense_Allocation);
      Sense_Outcome : SCSI_Outcome;
      Attempts : Natural := 0;
   begin
      Ready := False;
      for Attempt in 1 .. Ready_Attempts loop
         Attempts := Attempt;
         Run_SCSI (Unit, Test_Unit_Ready, SCSI_Microseconds, Outcome);
         if Outcome.Result = Passed then
            Ready := True;
            Passed (Unit_Ready, Field ("tries", Unsigned_64 (Attempts)));
            return;
         elsif Outcome.Result /= Check_Condition then
            exit;
         elsif not Outcome.Sense_Present then
            --  The device kept the sense data: fetch it to clear it.
            Run_SCSI (Unit, Sense, SCSI_Microseconds, Sense_Outcome);
         end if;
      end loop;
      Failed (Unit_Ready, "tur", Field ("tries", Unsigned_64 (Attempts)) &
              Failure_Text (Outcome));
   end Wait_Until_Ready;

   procedure Identify (Result : out Identity; Ready : out Boolean) is
      Units : Natural;
      Chosen : User_Unit;
      Found : Boolean;
      Outcome : SCSI_Outcome;
      Inquiry_Result : Inquiry_Data;
      Capacity_Info : Capacity_Data;
      Valid : Boolean;
      Buffer : constant System.Address :=
        UFS_Controller.Buffer (Bring_Up_Slot);
   begin
      Result := (others => <>);
      Initialize_Device (Ready);
      if not Ready then
         return;
      end if;
      Describe_Device (Units, Ready);
      if not Ready then
         return;
      end if;
      Choose_Unit (Units, Chosen, Found);
      if not Found then
         Ready := False;
         return;
      end if;
      Result.Unit := Logical_Unit (Chosen);
      Wait_Until_Ready (Result.Unit, Ready);
      if not Ready then
         return;
      end if;

      Run_SCSI (Result.Unit, SCSI.Inquiry (Standard_Inquiry_Bytes),
                SCSI_Microseconds, Outcome);
      Valid := Outcome.Result = Passed;
      if Valid then
         declare
            Data : SCSI.Bytes (1 .. Standard_Inquiry_Bytes)
              with Import, Address => Buffer;
         begin
            Decode_Inquiry (Data (1 .. Natural (Outcome.Moved)),
                            Inquiry_Result, Valid);
         end;
      end if;
      if Valid then
         Passed (Inquiry, Quoted (Inquiry_Result.Vendor & " " &
                                  Inquiry_Result.Product & " " &
                                  Inquiry_Result.Revision));
      else
         --  Not needed to serve the unit: logged, then carry on.
         Failed (Inquiry, "inquiry", Failure_Text (Outcome));
      end if;

      Run_SCSI (Result.Unit, Read_Capacity_16, SCSI_Microseconds, Outcome);
      Valid := Outcome.Result = Passed;
      if Valid then
         declare
            Data : SCSI.Bytes (1 .. Capacity_16_Bytes)
              with Import, Address => Buffer;
         begin
            Decode_Capacity_16 (Data (1 .. Natural (Outcome.Moved)),
                                Capacity_Info, Valid);
         end;
      end if;
      Ready := Valid
        and then Capacity_Info.Last_Block < Unsigned_64'Last
        and then Capacity_Info.Length in Block_Length
        and then Capacity_Info.Length <= UFS_Layout.Buffer_Bytes;
      if not Ready then
         Failed (Capacity, (if Valid then "unusable" else "read-capacity"),
                 Failure_Text (Outcome) &
                 Hex_Field ("last", Capacity_Info.Last_Block, 16) &
                 Hex_Field ("bs", Unsigned_64 (Capacity_Info.Length), 8));
         return;
      end if;
      Result.Blocks := Capacity_Info.Last_Block + 1;
      Result.Block_Size := Capacity_Info.Length;
      Passed (Capacity,
              Field ("blocks", Result.Blocks) &
              Field ("bs", Unsigned_64 (Result.Block_Size)) &
              " size=" & Size (Result.Blocks * Unsigned_64 (Result.Block_Size)));
   end Identify;

   ---------------------------------------------------------------------------
   --  Partition table (read only).
   ---------------------------------------------------------------------------
   --  Reads Count blocks from First into the bring-up slot's buffer.
   procedure Read_Blocks
     (Device : Identity; First : Unsigned_64; Count : Positive;
      Outcome : out SCSI_Outcome; OK : out Boolean)
   is
      Size_Bytes : constant Block_Length := Device.Block_Size;
   begin
      Outcome := (others => <>);
      OK := Unsigned_64 (Count) * Unsigned_64 (Size_Bytes) <=
          UFS_Layout.Buffer_Bytes
        and then First < Device.Blocks
        and then Unsigned_64 (Count) <= Device.Blocks - First;
      if not OK then
         return;
      end if;
      Run_SCSI
        (Device.Unit,
         (if First <= Unsigned_64 (Unsigned_32'Last)
          then Read_10 (Unsigned_32 (First), Block_Count_10 (Count), Size_Bytes)
          else Read_16 (First, Block_Count_16 (Count), Size_Bytes)),
         SCSI_Microseconds, Outcome);
      OK := Outcome.Result = Passed
        and then Outcome.Moved = Unsigned_32 (Count) * Size_Bytes;
   end Read_Blocks;

   procedure Report_Partitions (Device : Identity) is
      use Partition_Tables;
      Buffer : constant System.Address :=
        UFS_Controller.Buffer (Bring_Up_Slot);
      Region : Partition_Tables.Bytes (1 .. UFS_Layout.Buffer_Bytes)
        with Import, Address => Buffer;
      OK : Boolean;
      Outcome : SCSI_Outcome;
      Header : GPT_Header;
      Result : Header_Result;
      GPT_Location : constant := 1;
      --  Partitions listed on the summary line at most (keeps one line).
      Listed_At_Most : constant := 8;
   begin
      Read_Blocks (Device, 0, 1, Outcome, OK);
      if not OK then
         Failed (Partitions, "read-lba0", Failure_Text (Outcome));
         return;
      end if;
      case Classify_MBR (Region (1 .. Sector_Bytes)) is
         when No_Table =>
            Passed (Partitions, " none");
            return;
         when Legacy_MBR =>
            Passed (Partitions, " mbr (not decoded)");
            return;
         when Protective_GPT =>
            null;
      end case;
      Read_Blocks (Device, GPT_Location, 1, Outcome, OK);
      if not OK then
         Failed (Partitions, "read-gpt", Failure_Text (Outcome));
         return;
      end if;
      Decode_Header (Region (1 .. Natural (Device.Block_Size)), GPT_Location,
                     Device.Blocks, Header, Result);
      if Result /= Header_Valid then
         Failed (Partitions,
                 (case Result is
                    when Header_Valid  => "valid",
                    when Bad_Signature => "gpt-signature",
                    when Bad_Size      => "gpt-size",
                    when Bad_Checksum  => "gpt-crc",
                    when Bad_Location  => "gpt-location",
                    when Bad_Entries   => "gpt-entries"));
         return;
      end if;
      declare
         Array_Bytes : constant Unsigned_64 :=
           Unsigned_64 (Header.Entry_Count) * Unsigned_64 (Header.Entry_Bytes);
         Block : constant Unsigned_64 := Unsigned_64 (Device.Block_Size);
         Blocks : constant Unsigned_64 := (Array_Bytes + Block - 1) / Block;
         Used : Natural := 0;
         Listing : String (1 .. 256);
         Listed : Natural := 0;
         Outside : Natural := 0;

         procedure Append (Text : String) is
         begin
            if Listed + Text'Length <= Listing'Length then
               Listing (Listed + 1 .. Listed + Text'Length) := Text;
               Listed := Listed + Text'Length;
            end if;
         end Append;
      begin
         if Blocks * Block > UFS_Layout.Buffer_Bytes then
            Failed (Partitions, "gpt-array-too-large",
                    Field ("entries", Unsigned_64 (Header.Entry_Count)) &
                    Field ("size", Unsigned_64 (Header.Entry_Bytes)));
            return;
         end if;
         Read_Blocks (Device, Header.Entries_LBA, Positive (Blocks), Outcome, OK);
         if not OK then
            Failed (Partitions, "read-entries", Failure_Text (Outcome));
            return;
         end if;
         if CRC32 (Region (1 .. Natural (Array_Bytes))) /= Header.Entries_CRC
         then
            Failed (Partitions, "entries-crc",
                    Hex_Field ("want", Unsigned_64 (Header.Entries_CRC), 8) &
                    Hex_Field ("got", Unsigned_64
                      (CRC32 (Region (1 .. Natural (Array_Bytes)))), 8));
            return;
         end if;
         for Index in 0 .. Natural (Header.Entry_Count) - 1 loop
            declare
               Item : constant Partition_Entry :=
                 Decode_Entry (Region (1 .. Natural (Array_Bytes)),
                               Index * Natural (Header.Entry_Bytes));
            begin
               if not Is_Unused (Item) then
                  Used := Used + 1;
                  if not Entry_In_Range (Header, Item) then
                     Outside := Outside + 1;
                  end if;
                  if Used <= Listed_At_Most then
                     Append (" p" & Decimal (Unsigned_64 (Index + 1)) & "=" &
                             Type_Name (Item.Kind) & ":" &
                             (if Item.Last >= Item.First
                              then Size ((Item.Last - Item.First + 1) * Block)
                              else "?"));
                  end if;
               end if;
            end;
         end loop;
         Passed (Partitions, " gpt" & Field ("n", Unsigned_64 (Used)) &
                 Listing (1 .. Listed) &
                 (if Outside > 0
                  then Field ("outside", Unsigned_64 (Outside)) else ""));
      end;
   end Report_Partitions;
end UFS_Identify;
