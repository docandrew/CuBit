------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Hosted tests for USB mass-storage disks (SELF-003 step 4): the BOT
--  wrappers (USB_Bulk_Only), the SCSI disk decoders (SCSI_Disks, MODE
--  SENSE in SCSI) and the probe (USB_Disk_Probe), against byte-exact
--  fixtures from BOT 1.0 / SPC-4 / SBC-3 and adversarial devices: wrong
--  signatures, tags, lengths, residues and status codes, lying capacities,
--  ISO media, endless unit attentions, and random answers.
------------------------------------------------------------------------------
with Ada.Text_IO;
with Interfaces; use Interfaces;
with SCSI; use SCSI;
with SCSI_Disks; use SCSI_Disks;
with USB_Bulk_Only; use USB_Bulk_Only;
with USB_Disk_Probe; use USB_Disk_Probe;

procedure Main is
   Checks : Natural := 0;
   Failures : Natural := 0;

   procedure Expect (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL " & What);
      end if;
   end Expect;

   function LE (Value : Unsigned_32) return SCSI.Bytes is
     ([Unsigned_8 (Value and 16#FF#), Unsigned_8 (Shift_Right (Value, 8) and 16#FF#),
       Unsigned_8 (Shift_Right (Value, 16) and 16#FF#), Unsigned_8 (Shift_Right (Value, 24))]);
   function BE (Value : Unsigned_32) return SCSI.Bytes is
     ([Unsigned_8 (Shift_Right (Value, 24)), Unsigned_8 (Shift_Right (Value, 16) and 16#FF#),
       Unsigned_8 (Shift_Right (Value, 8) and 16#FF#), Unsigned_8 (Value and 16#FF#)]);
   function BE64 (Value : Unsigned_64) return SCSI.Bytes is
     (BE (Unsigned_32 (Shift_Right (Value, 32))) & BE (Unsigned_32 (Value and 16#FFFF_FFFF#)));

   --  A CSW: "USBS", tag, residue, status.
   function CSW (Tag, Residue : Unsigned_32; Status : Unsigned_8) return SCSI.Bytes is
     ([16#55#, 16#53#, 16#42#, 16#53#] & LE (Tag) & LE (Residue) & [1 => Status]);

   procedure Wrappers is
      Block : SCSI.Command_Block;
      W : Command_Wrapper;
      R : Status_Report;
   begin
      --  BOT 5.1: a READ(10) of 8 blocks of 512 at LBA 0x12345678, tag 7.
      Block := SCSI.Read_10 (16#1234_5678#, 8, 512);
      W := Encode (Block, 7, 0);
      Expect (W (1 .. 4) = [16#55#, 16#53#, 16#42#, 16#43#], "cbw signature");
      Expect (W (5 .. 8) = [7, 0, 0, 0], "cbw tag");
      Expect (W (9 .. 12) = [0, 16#10#, 0, 0], "cbw length 4096");
      Expect (W (13) = 16#80# and W (14) = 0 and W (15) = 10, "cbw flags/lun/length");
      Expect (W (16 .. 25) = [16#28#, 0, 16#12#, 16#34#, 16#56#, 16#78#, 0, 0, 8, 0],
              "cbw read(10) cdb");
      Expect ((for all I in 26 .. 31 => W (I) = 0), "cbw cdb tail zero");
      --  A WRITE goes host-to-device: flags 0; the LUN field.
      Block := SCSI.Write_16 (16#0102_0304_0506_0708#, 2, 4096, False);
      W := Encode (Block, 16#DEAD_BEEF#, 15);
      Expect (W (5 .. 8) = [16#EF#, 16#BE#, 16#AD#, 16#DE#], "cbw tag le");
      Expect (W (9 .. 12) = [0, 16#20#, 0, 0], "cbw write length 8192");
      Expect (W (13) = 0 and W (14) = 15 and W (15) = 16, "cbw write flags/lun");
      Expect (W (16) = 16#8A# and W (18 .. 25) = [1, 2, 3, 4, 5, 6, 7, 8], "cbw write(16)");
      --  No data: length 0, flags 0.
      W := Encode (SCSI.Test_Unit_Ready, 1, 3);
      Expect (W (9 .. 13) = [0, 0, 0, 0, 0] and W (14) = 3 and W (15) = 6, "cbw tur");
      W := Encode (SCSI.Synchronize_Cache, 2, 0);
      Expect (W (13) = 0 and W (16) = 16#35#, "cbw sync cache");

      --  BOT 6.3: a good CSW.
      R := Decode_Status (CSW (7, 0, 0), 7, 4096, 4096);
      Expect (R.Kind = Good and Complete (R, 4096), "csw good");
      --  Wrong length, signature, tag.
      Expect (Decode_Status (CSW (7, 0, 0) (1 .. 12), 7, 4096, 4096).Kind = Invalid, "csw short");
      Expect (Decode_Status (CSW (7, 0, 0) & [1 => 0], 7, 4096, 4096).Kind = Invalid, "csw long");
      declare
         Bad : SCSI.Bytes := CSW (7, 0, 0);
      begin
         Bad (4) := 16#43#;   --  "USBC": a CBW echoed back
         Expect (Decode_Status (Bad, 7, 0, 0).Kind = Invalid, "csw signature");
      end;
      Expect (Decode_Status (CSW (8, 0, 0), 7, 0, 0).Kind = Invalid, "csw tag");
      --  Every status byte: 0 good, 1 failed, 2 phase error, others invalid.
      for S in Unsigned_8 loop
         R := Decode_Status (CSW (9, 0, S), 9, 512, 512);
         Expect (R.Kind = (case S is when 0 => Good, when 1 => Check_Condition,
                                     when 2 => Phase_Error, when others => Invalid),
                 "csw status" & S'Image);
      end loop;
      --  A phase error carries nothing, whatever its residue says.
      R := Decode_Status (CSW (9, 16#FFFF_FFFF#, 2), 9, 512, 100);
      Expect (R = (Kind => Phase_Error, Residue => 0, Moved => 0), "csw phase error");
      --  Not meaningful: residue above the CBW's length.
      Expect (Decode_Status (CSW (9, 513, 0), 9, 512, 0).Kind = Invalid, "csw residue > length");
      Expect (Decode_Status (CSW (9, 513, 1), 9, 512, 0).Kind = Invalid, "csw failed residue > length");
      --  The host count can never exceed the CBW.
      Expect (Decode_Status (CSW (9, 0, 0), 9, 512, 513).Kind = Invalid, "csw moved > length");
      --  A device claiming success for data the host never saw: Good,
      --  but not Complete (Moved is the host's count).
      R := Decode_Status (CSW (9, 0, 0), 9, 512, 100);
      Expect (R.Kind = Good and R.Moved = 100 and not Complete (R, 512), "csw lie about data");
      --  A residue on a whole transfer: not complete either.
      R := Decode_Status (CSW (9, 1, 0), 9, 512, 512);
      Expect (R.Kind = Good and not Complete (R, 512), "csw residue on whole transfer");
      --  A short INQUIRY with a residue: Good, Moved as received.
      R := Decode_Status (CSW (9, 31, 0), 9, 36, 5);
      Expect (R.Kind = Good and R.Residue = 31 and R.Moved = 5, "csw short data");
      --  A failed write that moved everything is still a failure.
      R := Decode_Status (CSW (9, 0, 1), 9, 4096, 4096);
      Expect (R.Kind = Check_Condition and not Complete (R, 4096), "csw failed");
      --  A slice that does not start at 1.
      declare
         Frame : constant SCSI.Bytes (50 .. 62) := CSW (77, 0, 0);
      begin
         Expect (Decode_Status (Frame, 77, 0, 0).Kind = Good, "csw slice");
      end;
   end Wrappers;

   procedure Max_LUN is
      Last : USB_Bulk_Only.Logical_Unit;
      Valid : Boolean;
      Empty : constant SCSI.Bytes (1 .. 0) := [];
   begin
      Decode_Max_LUN (USB_Bulk_Only.Control_Stall, Empty, Last, Valid);
      Expect (Valid and Last = 0, "max lun stall");
      Decode_Max_LUN (USB_Bulk_Only.Control_Error, [1 => 3], Last, Valid);
      Expect (not Valid and Last = 0, "max lun error");
      for V in Unsigned_8 loop
         Decode_Max_LUN (USB_Bulk_Only.Control_Success, [1 => V], Last, Valid);
         Expect (Valid = (V <= 15) and Last = (if V <= 15 then V else 0), "max lun" & V'Image);
      end loop;
      Decode_Max_LUN (USB_Bulk_Only.Control_Success, Empty, Last, Valid);
      Expect (not Valid, "max lun empty");
      Decode_Max_LUN (USB_Bulk_Only.Control_Success, [0, 0], Last, Valid);
      Expect (not Valid, "max lun two bytes");
   end Max_LUN;

   procedure Disks is
      G : Geometry;
      O : Capacity_Outcome;
      H : Mode_Header;
      Valid : Boolean;
      Block : SCSI.Command_Block;
   begin
      --  MODE SENSE(6) (SPC-4 6.11): DBD, page 3Fh, allocation.
      Block := SCSI.Mode_Sense_6 (192);
      Expect (Wire (Block) = [16#1A#, 16#08#, 16#3F#, 0, 192, 0] and Block.Expected = 192
              and Block.Direction = From_Device, "mode sense cdb");
      Decode_Mode_Header_6 ([3, 0, 16#80#, 0], H, Valid);
      Expect (Valid and H.Write_Protected, "mode wp");
      Decode_Mode_Header_6 ([16#23#, 0, 16#10#, 8, 1, 2], H, Valid);
      Expect (Valid and not H.Write_Protected and H.Descriptor_Length = 8, "mode not wp");
      Decode_Mode_Header_6 ([3, 0, 16#80#], H, Valid);
      Expect (not Valid and not H.Write_Protected, "mode short");
      Decode_Mode_Header_6 ([2, 0, 16#80#, 0], H, Valid);
      Expect (not Valid and not H.Write_Protected, "mode length too small");

      --  READ CAPACITY(10) (SBC-3 5.16.2).
      From_Capacity_10 (BE (16#0007_FFFF#) & BE (512), G, O);
      Expect (O = Capacity_Valid and G.Blocks = 16#8_0000# and G.Size = Block_512, "cap10 512");
      From_Capacity_10 (BE (0) & BE (4096), G, O);
      Expect (O = Capacity_Valid and G.Blocks = 1 and G.Size = Block_4096, "cap10 one 4096");
      From_Capacity_10 (BE (16#FFFF_FFFF#) & BE (512), G, O);
      Expect (O = Capacity_16_Required, "cap10 overflow");
      From_Capacity_10 (BE (100) & BE (16#FFFF_FFFE#), G, O);
      Expect (O = Unsupported_Block_Size, "cap10 huge length");
      for Length of SCSI.Bytes'[0, 1, 2] loop
         From_Capacity_10 (BE (100) & BE (Unsigned_32 (Length) * 1024), G, O);
         Expect (O = Unsupported_Block_Size, "cap10 length" & Length'Image);
      end loop;
      From_Capacity_10 (BE (100) & BE (2048), G, O);
      Expect (O = Unsupported_Block_Size, "cap10 2048 (optical) unsupported");
      From_Capacity_10 (BE (100) & BE (520), G, O);
      Expect (O = Unsupported_Block_Size, "cap10 520");
      From_Capacity_10 (SCSI.Bytes'(BE (100) & BE (512)) (1 .. 7), G, O);
      Expect (O = Malformed_Capacity, "cap10 short");
      From_Capacity_10 (BE (100) & BE (512) & [1 => 0], G, O);
      Expect (O = Malformed_Capacity, "cap10 long");

      --  READ CAPACITY(16) (SBC-3 5.15.2).
      From_Capacity_16 (BE64 (16#0000_0002_0000_0000#) & BE (4096) & [0, 0, 0, 0], G, O);
      Expect (O = Capacity_Valid and G.Blocks = 16#2_0000_0001# and G.Size = Block_4096, "cap16");
      From_Capacity_16 (BE64 (Unsigned_64'Last) & BE (512), G, O);
      Expect (O = Malformed_Capacity, "cap16 max lba");
      From_Capacity_16 (BE64 (16#1_0000_0000#) & BE (512) & [1 .. 20 => 0], G, O);
      Expect (O = Capacity_Valid and G.Blocks = 16#1_0000_0001#, "cap16 full response");
      From_Capacity_16 (SCSI.Bytes'(BE64 (5) & BE (512)) (1 .. 11), G, O);
      Expect (O = Malformed_Capacity, "cap16 short");
      From_Capacity_16 (BE64 (5) & BE (1024), G, O);
      Expect (O = Unsupported_Block_Size, "cap16 1024");

      --  Sense meanings (SPC-4 Annex D).
      Expect (Meaning ((Fixed_Current, Not_Ready, 16#3A#, 0)) = Medium_Absent, "sense no medium");
      Expect (Meaning ((Fixed_Current, Not_Ready, 16#04#, 1)) = Retry_Later, "sense becoming ready");
      Expect (Meaning ((Fixed_Current, Not_Ready, 16#05#, 0)) = Medium_Failure, "sense not ready other");
      Expect (Meaning ((Fixed_Current, Unit_Attention, 16#28#, 0)) = SCSI_Disks.Medium_Changed,
              "sense changed");
      Expect (Meaning ((Fixed_Current, Unit_Attention, 16#29#, 0)) = Retry_Later, "sense reset");
      Expect (Meaning ((Fixed_Current, Unit_Attention, 16#3A#, 0)) = Medium_Absent, "sense ua absent");
      Expect (Meaning ((Fixed_Current, Data_Protect, 16#27#, 0)) = SCSI_Disks.Write_Protected,
              "sense wp");
      Expect (Meaning ((Fixed_Current, Illegal_Request, 16#20#, 0)) = Command_Refused, "sense illegal");
      Expect (Meaning ((Fixed_Current, Medium_Error, 16#11#, 0)) = Medium_Failure, "sense medium error");
      Expect (Meaning ((Fixed_Current, No_Sense, 0, 0)) = SCSI_Disks.No_Error, "sense none");
      Expect (Meaning ((Fixed_Current, Recovered_Error, 0, 0)) = SCSI_Disks.No_Error, "sense recovered");
   end Disks;

   ---------------------------------------------------------------------------
   --  The probe against scripted devices.
   ---------------------------------------------------------------------------
   type Device_Kind is
     (Stick, Protected_Stick, ISO_Stick, Empty_Reader, CD_Drive, Huge_Disk,
      Endless_Attention, Short_Inquiry, Mode_Stall, Tiny_Disk, Disk_4K,
      Capacity_Lie, ISO_Read_Fails, Hard_Failure, Failing_Medium, Slow_Stick,
      Capacity_Refused);

   function Inquiry_Bytes (Peripheral : Unsigned_8) return SCSI.Bytes is
      Data : SCSI.Bytes (1 .. 36) := [others => Character'Pos (' ')];
   begin
      Data (1) := Peripheral;
      Data (2) := 16#80#;   --  removable
      Data (3) := 6;
      Data (4) := 2;
      Data (5) := 31;
      Data (6 .. 8) := [0, 0, 0];
      return Data;
   end Inquiry_Bytes;

   function Fixed_Sense (Key, Code : Unsigned_8) return SCSI.Bytes is
      Data : SCSI.Bytes (1 .. 18) := [others => 0];
   begin
      Data (1) := 16#70#;
      Data (3) := Key;
      Data (8) := 10;
      Data (13) := Code;
      return Data;
   end Fixed_Sense;

   ISO_Block_Bytes : constant SCSI.Bytes :=
     [1, Character'Pos ('C'), Character'Pos ('D'), Character'Pos ('0'),
      Character'Pos ('0'), Character'Pos ('1'), 1] & [1 .. 505 => 0];
   FAT_Block_Bytes : constant SCSI.Bytes := [1 .. 512 => 16#F6#];

   --  The device's answer to P's next command.
   procedure Answer
     (Kind : Device_Kind; P : Probe; TUR_Count : in out Natural;
      Status : out Status_Kind; Data : out SCSI.Bytes; Length : out Natural)
   is
      procedure Give (Bytes : SCSI.Bytes) is
      begin
         Data (1 .. Bytes'Length) := Bytes;
         Length := Bytes'Length;
      end Give;
   begin
      Status := Good;
      Length := 0;
      Data := [others => 0];
      if Kind = Hard_Failure then
         Status := Invalid;
         return;
      end if;
      case P.Current is
         when Inquire =>
            if Kind = CD_Drive then
               Give (Inquiry_Bytes (16#05#));
            elsif Kind = Short_Inquiry then
               Give (Inquiry_Bytes (0) (1 .. 20));
            else
               Give (Inquiry_Bytes (0));
            end if;
         when Test_Ready =>
            TUR_Count := TUR_Count + 1;
            if Kind in Empty_Reader | Endless_Attention | Failing_Medium or else TUR_Count = 1
              or else (Kind = Slow_Stick and then TUR_Count <= 12)
            then
               Status := Check_Condition;
            end if;
         when Read_Sense =>
            if Kind = Empty_Reader then
               Give (Fixed_Sense (2, 16#3A#));
            elsif Kind = Failing_Medium then
               Give (Fixed_Sense (3, 16#11#));
            elsif Kind = Slow_Stick then
               Give (Fixed_Sense (2, 16#04#));
            else
               Give (Fixed_Sense (6, 16#29#));
            end if;
         when Capacity_10 =>
            if Kind = Capacity_Refused then
               Status := Check_Condition;
            end if;
            case Kind is
               when Huge_Disk => Give (BE (16#FFFF_FFFF#) & BE (512));
               when Tiny_Disk => Give (BE (31) & BE (512));
               when Disk_4K => Give (BE (16#3_FFFF#) & BE (4096));
               when Capacity_Lie => Give (BE (100) & BE (2048));
               when others => Give (BE (16#0003_FFFF#) & BE (512));
            end case;
         when Capacity_16 =>
            Give (BE64 (16#1_FFFF_FFFF#) & BE (512) & [1 .. 20 => 0]);
         when Mode_Sense =>
            if Kind = Mode_Stall then
               Status := Invalid;
            else
               Give ([3, 0, (if Kind = Protected_Stick then 16#80# else 0), 0]);
            end if;
         when Check_ISO =>
            if Kind = ISO_Read_Fails then
               Status := Check_Condition;
            elsif Kind = ISO_Stick then
               Give (ISO_Block_Bytes);
            elsif Kind = Disk_4K then
               Give ([1 .. 4096 => 0]);
            else
               Give (FAT_Block_Bytes);
            end if;
         when Finished => null;
      end case;
   end Answer;

   function Run (Kind : Device_Kind; Commands : out Natural) return Probe is
      P : Probe := Start;
      TUR_Count : Natural := 0;
      Status : Status_Kind;
      Data : SCSI.Bytes (1 .. Largest_Probe_Bytes);
      Length : Natural;
      Block : SCSI.Command_Block;
   begin
      Commands := 0;
      while P.Current in Active loop
         Block := Command (P);
         Commands := Commands + 1;
         if P.Current = Check_ISO then
            Expect (Block.Operation = SCSI.Read_10 and Block.Expected = Bytes_Of (P.Geometry.Size)
                    and Wire (Block) (3 .. 6) =
                      BE (32_768 / Bytes_Of (P.Geometry.Size)),
                    "iso check reads the block of byte 32768");
         end if;
         Answer (Kind, P, TUR_Count, Status, Data, Length);
         Advance (P, Status, Data (1 .. Length));
      end loop;
      return P;
   end Run;

   procedure Probes is
      P : Probe;
      N : Natural;
   begin
      P := Run (Stick, N);
      Expect (P.Current = Ready and P.Geometry = (16#4_0000#, Block_512) and not P.Write_Protected
              and P.Removable and not P.Mode_Unknown, "probe stick");
      Expect (N = 7, "probe stick commands" & N'Image);
      P := Run (Protected_Stick, N);
      Expect (P.Current = Ready and P.Write_Protected, "probe write protected");
      P := Run (ISO_Stick, N);
      Expect (P.Current = ISO_Medium, "probe iso medium is not a disk");
      P := Run (Empty_Reader, N);
      Expect (P.Current = No_Medium and P.Sense = Medium_Absent, "probe empty reader");
      P := Run (Failing_Medium, N);
      Expect (P.Current = Failed and P.Sense = Medium_Failure, "probe medium error");
      P := Run (CD_Drive, N);
      Expect (P.Current = Not_A_Disk and N = 1, "probe cd drive");
      P := Run (Huge_Disk, N);
      Expect (P.Current = Ready and P.Geometry.Blocks = 16#2_0000_0000#, "probe capacity 16");
      P := Run (Endless_Attention, N);
      Expect (P.Current = Failed and N = 1 + 2 * (Ready_Attempt_Limit + 1),
              "probe bounded" & N'Image);
      P := Run (Short_Inquiry, N);
      Expect (P.Current = Not_A_Disk, "probe short inquiry");
      P := Run (Mode_Stall, N);
      Expect (P.Current = Ready and P.Mode_Unknown and not P.Write_Protected, "probe mode stall");
      P := Run (Tiny_Disk, N);
      Expect (P.Current = Ready and P.Geometry.Blocks = 32 and N = 6, "probe tiny disk skips iso");
      P := Run (Disk_4K, N);
      Expect (P.Current = Ready and P.Geometry.Size = Block_4096, "probe 4k");
      P := Run (Capacity_Lie, N);
      Expect (P.Current = Failed, "probe 2048 capacity");
      P := Run (ISO_Read_Fails, N);
      Expect (P.Current = Failed, "probe unreadable iso block is not served");
      P := Run (Slow_Stick, N);
      Expect (P.Current = Ready and P.Attempts = 12, "probe waits for a slow stick");
      P := Run (Capacity_Refused, N);
      Expect (P.Current = Failed, "probe refused capacity with plausible bytes");
      P := Run (Hard_Failure, N);
      Expect (P.Current = Failed and N = 1, "probe transport failure");
      --  ISO signature variants.
      Expect (ISO_Signature (ISO_Block_Bytes), "iso pvd");
      Expect (ISO_Signature ([16#FF#] & ISO_Block_Bytes (2 .. 7)), "iso terminator");
      Expect (not ISO_Signature ([4] & ISO_Block_Bytes (2 .. 7)), "iso type 4");
      Expect (not ISO_Signature (ISO_Block_Bytes (1 .. 6)), "iso short");
      Expect (not ISO_Signature (ISO_Block_Bytes (1 .. 6) & [1 => 2]), "iso version 2");
   end Probes;

   --  Random answers: the probe ends within its bound, and serves a disk
   --  only after a clean ISO check.
   procedure Fuzz is
      Seed : Unsigned_32 := 16#1234_5678#;
      function Next return Unsigned_32 is
      begin
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 17);
         Seed := Seed xor Shift_Left (Seed, 5);
         return Seed;
      end Next;
      Data : SCSI.Bytes (1 .. Largest_Probe_Bytes);
      Readies, ISOs : Natural := 0;
   begin
      for Round in 1 .. 20_000 loop
         declare
            P : Probe := Start;
            Steps : Natural := 0;
            Bound : constant Natural := Remaining (P);
            Kind : Status_Kind;
            Length : Natural;
            Was : Step;
         begin
            while P.Current in Active loop
               Kind := Status_Kind'Val (Next mod 4);
               if Next mod 3 = 0 then
                  Kind := Good;
               end if;
               Length := Natural (Next mod 64);
               for I in 1 .. Length loop
                  Data (I) := Unsigned_8 (Next and 16#FF#);
               end loop;
               --  Plausible answers now and then, to go deep.
               case P.Current is
                  when Inquire =>
                     if Next mod 2 = 0 then
                        Data (1 .. 36) := Inquiry_Bytes (0);
                        Length := 36;
                     end if;
                  when Capacity_10 =>
                     if Next mod 2 = 0 then
                        Data (1 .. 8) := BE (Next mod 4096) & BE ((if Next mod 2 = 0 then 512 else 4096));
                        Length := 8;
                     end if;
                  when Check_ISO =>
                     if Next mod 2 = 0 then
                        Data (1 .. 7) := ISO_Block_Bytes (1 .. 7);
                        Length := 512;
                     end if;
                  when others => null;
               end case;
               Was := P.Current;
               declare
                  Before : constant Natural := Remaining (P);
               begin
                  Advance (P, Kind, Data (1 .. Length));
                  Expect (Remaining (P) < Before, "fuzz progress");
               end;
               if P.Current = Ready then
                  Readies := Readies + 1;
                  Expect ((Was = Check_ISO and Kind = Good and not ISO_Signature (Data (1 .. Length)))
                          or else (Was = Mode_Sense and ISO_Block (P.Geometry) >= P.Geometry.Blocks),
                          "fuzz ready only after the iso check");
               elsif P.Current = ISO_Medium then
                  ISOs := ISOs + 1;
               end if;
               Steps := Steps + 1;
            end loop;
            Expect (Steps <= Bound, "fuzz bound");
         end;
      end loop;
      Expect (Readies > 0 and ISOs > 0, "fuzz reached ready and iso" & Readies'Image & ISOs'Image);
   end Fuzz;
begin
   Wrappers;
   Max_LUN;
   Disks;
   Probes;
   Fuzz;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("usb-disk: PASS" & Checks'Image & " checks");
   else
      Ada.Text_IO.Put_Line ("usb-disk: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
