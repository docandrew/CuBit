--  Hosted regression tests for IOMMU-001 (tests/iommu/run.sh).
--
--  1. DMAR admission (Firmware_Tables.Remapping): the IdeaPad Slim 3i's
--     136-byte DMAR rebuilt from its dump facts, a QEMU intel-iommu table,
--     every truncation, every single-byte mutation (checksum refixed), and
--     each typed rejection.
--  2. VT-d page-table arithmetic (IO_Page_Tables).
--  3. The walker over a model store against the domain ledger
--     (IO_Page_Walk, IOMMU_Domains): after every operation the leaves a
--     domain's tables reach are exactly the ledger's Mapped runs of that
--     domain, identity mapped, and the unmap -> flush -> release order is
--     enforced.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables; use Firmware_Tables;
with Firmware_Tables.Remapping;
with IO_Page_Tables; use IO_Page_Tables;
with IOMMU_Domains;
with Table_Model;

procedure Main is
   package R renames Firmware_Tables.Remapping;
   package ID renames IOMMU_Domains;
   use type R.Outcome;
   use type R.Rejection;
   use type R.Scope_Kind;
   use type R.Scope_Owner;
   use type ID.Mapping_Kind;
   use type ID.Owner_Key;
   use type ID.Mapping_Table;
   use type ID.Map_Status;
   use type ID.Release_Status;
   use type ID.Domain_Status;
   use type ID.Bind_Status;
   use type ID.Admit_Status;
   use type ID.Mapping_State;

   Checks : Natural := 0;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  Table building
   ---------------------------------------------------------------------------
   Max_Table : constant := 1024;
   type Buffer is record
      Data : Bytes (1 .. Max_Table) := [others => 0];
      Length : Natural := 0;
   end record;

   procedure Put_Byte (B : in out Buffer; V : Unsigned_64) is
   begin
      B.Length := B.Length + 1;
      B.Data (B.Length) := Byte (V and 16#FF#);
   end Put_Byte;

   procedure Put_LE (B : in out Buffer; V : Unsigned_64; Width : Positive) is
   begin
      for I in 0 .. Width - 1 loop
         Put_Byte (B, Shift_Right (V, 8 * I));
      end loop;
   end Put_LE;

   procedure Set_LE (B : in out Buffer; Offset : Natural; V : Unsigned_64; Width : Positive) is
   begin
      for I in 0 .. Width - 1 loop
         B.Data (Offset + 1 + I) := Byte (Shift_Right (V, 8 * I) and 16#FF#);
      end loop;
   end Set_LE;

   --  SDT header with a placeholder length/checksum.
   procedure Begin_Table (B : out Buffer; Host_Width_Field, Flags : Unsigned_64) is
   begin
      B := (others => <>);
      for C of String'("DMAR") loop
         Put_Byte (B, Character'Pos (C));
      end loop;
      Put_LE (B, 0, 4);              --  length
      Put_Byte (B, 1);               --  revision
      Put_Byte (B, 0);               --  checksum
      for C of String'("LENOVO") loop
         Put_Byte (B, Character'Pos (C));
      end loop;
      for C of String'("CB-01   ") loop
         Put_Byte (B, Character'Pos (C));
      end loop;
      Put_LE (B, 1, 4);              --  OEM revision
      Put_LE (B, 16#4C4E5443#, 4);   --  creator
      Put_LE (B, 1, 4);              --  creator revision
      Put_Byte (B, Host_Width_Field);
      Put_Byte (B, Flags);
      for I in 1 .. 10 loop
         Put_Byte (B, 0);
      end loop;
   end Begin_Table;

   procedure Seal (B : in out Buffer; Declared : Natural) is
      Sum : Unsigned_8 := 0;
   begin
      Set_LE (B, 4, Unsigned_64 (Declared), 4);
      B.Data (10) := 0;
      for I in 1 .. Declared loop
         Sum := Sum + B.Data (I);
      end loop;
      B.Data (10) := 0 - Sum;
   end Seal;

   procedure Seal (B : in out Buffer) is
   begin
      Seal (B, B.Length);
   end Seal;

   procedure Unit (B : in out Buffer; Flags, Size_Field : Unsigned_64;
                   Base : Unsigned_64; Scope_Bytes : Natural) is
   begin
      Put_LE (B, 0, 2);
      Put_LE (B, Unsigned_64 (16 + Scope_Bytes), 2);
      Put_Byte (B, Flags);
      Put_Byte (B, Size_Field);
      Put_LE (B, 0, 2);              --  segment
      Put_LE (B, Base, 8);
   end Unit;

   procedure Region (B : in out Buffer; Base, Limit : Unsigned_64; Scope_Bytes : Natural) is
   begin
      Put_LE (B, 1, 2);
      Put_LE (B, Unsigned_64 (24 + Scope_Bytes), 2);
      Put_LE (B, 0, 2);              --  reserved
      Put_LE (B, 0, 2);              --  segment
      Put_LE (B, Base, 8);
      Put_LE (B, Limit, 8);
   end Region;

   procedure Scope (B : in out Buffer; Kind, Enumeration, Bus, Device, Func : Unsigned_64) is
   begin
      Put_Byte (B, Kind);
      Put_Byte (B, 8);
      Put_LE (B, 0, 2);
      Put_Byte (B, Enumeration);
      Put_Byte (B, Bus);
      Put_Byte (B, Device);
      Put_Byte (B, Func);
   end Scope;

   Endpoint : constant := 1;
   Bridge : constant := 2;
   IOAPIC_Scope : constant := 3;
   HPET_Scope : constant := 4;

   --  The IdeaPad Slim 3i (i3-N305) DMAR from its 136-byte dump: host
   --  address width 39 (field 38), flags 0x5, DRHD 0xFED90000 scoped to the
   --  iGPU 00:02.0, DRHD 0xFED91000 INCLUDE_PCI_ALL (I/O APIC and HPET
   --  scopes), RMRR 0x4C000000-0x503FFFFF for 00:02.0.
   function Laptop return Buffer is
      B : Buffer;
   begin
      Begin_Table (B, 38, 16#05#);
      Unit (B, 0, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      Unit (B, 1, 0, 16#FED9_1000#, 16);
      Scope (B, IOAPIC_Scope, 2, 16#F0#, 16#1F#, 0);
      Scope (B, HPET_Scope, 0, 0, 16#1E#, 6);
      Region (B, 16#4C00_0000#, 16#503F_FFFF#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      Seal (B);
      return B;
   end Laptop;

   --  QEMU q35 + intel-iommu,aw-bits=48,intremap=on: one INCLUDE_PCI_ALL
   --  unit with the I/O APIC scope.
   function QEMU return Buffer is
      B : Buffer;
   begin
      Begin_Table (B, 47, 16#01#);
      Unit (B, 1, 0, 16#FED9_0000#, 8);
      Scope (B, IOAPIC_Scope, 0, 16#FF#, 0, 0);
      Seal (B);
      return B;
   end QEMU;

   type Test_Result is record
      Status : R.Outcome := R.Rejected;
      Why    : R.Rejection := R.Header_Rejected;
      Offset : Natural := 0;
      Value  : R.Facts;
   end record;

   procedure Decode_Bytes (Data : Bytes; Result : out Test_Result) is
   begin
      R.Decode (Data, Result.Value, Result.Status, Result.Why, Result.Offset);
   end Decode_Bytes;

   procedure Decode (B : Buffer; Result : out Test_Result) is
   begin
      Decode_Bytes (B.Data (1 .. B.Length), Result);
   end Decode;

   function Rejected_As (B : Buffer; Why : R.Rejection) return Boolean is
      Result : Test_Result;
   begin
      Decode (B, Result);
      if Result.Status = R.Accepted then
         return False;
      end if;
      if Result.Why /= Why then
         Put_Line ("  got " & R.Rejection'Image (Result.Why) & " at" &
                   Natural'Image (Result.Offset));
      end if;
      return Result.Why = Why;
   end Rejected_As;

   procedure Test_Laptop is
      B : constant Buffer := Laptop;
      Result : Test_Result;
   begin
      Check (B.Length = 136, "the laptop DMAR fixture is 136 bytes");
      Decode (B, Result);
      Check (Result.Status = R.Accepted, "laptop DMAR accepted");
      if Result.Status /= R.Accepted then
         return;
      end if;
      declare
         F : constant R.Facts := Result.Value;
      begin
         Check (F.Width = 39, "host address width 39");
         Check (F.Interrupt_Remapping, "flags: interrupt remapping");
         Check (not F.X2APIC_Opt_Out, "flags: no x2APIC opt-out");
         Check (F.DMA_Control_Opt_In, "flags: DMA_CTRL_PLATFORM_OPT_IN");
         Check (F.Unit_Total = 2, "two DRHD units");
         Check (F.Units (1).Register_Base = 16#FED9_0000#, "unit 1 base");
         Check (not F.Units (1).Include_All, "unit 1 device-scoped");
         Check (F.Units (1).Scopes = 1, "unit 1 one scope");
         Check (F.Scopes (F.Units (1).First_Scope).Kind = R.PCI_Endpoint
                and then F.Scopes (F.Units (1).First_Scope).Start_Bus = 0
                and then F.Scopes (F.Units (1).First_Scope).Hops = 1
                and then F.Scopes (F.Units (1).First_Scope).Path (1).Device = 2
                and then F.Scopes (F.Units (1).First_Scope).Path (1).Func = 0,
                "unit 1 scope is endpoint 00:02.0");
         Check (F.Units (2).Register_Base = 16#FED9_1000#, "unit 2 base");
         Check (F.Units (2).Include_All, "unit 2 INCLUDE_PCI_ALL");
         Check (F.Units (2).Scopes = 2, "unit 2 has the I/O APIC and HPET scopes");
         Check (F.Scopes (F.Units (2).First_Scope).Kind = R.IOAPIC, "unit 2 I/O APIC scope");
         Check (F.Scopes (F.Units (2).First_Scope + 1).Kind = R.HPET, "unit 2 HPET scope");
         Check (F.Units (1).Register_Pages = 1 and then F.Units (2).Register_Pages = 1,
                "one register page per unit");
         Check (F.Region_Total = 1, "one RMRR");
         Check (F.Regions (1).Base = 16#4C00_0000# and then F.Regions (1).Limit = 16#503F_FFFF#,
                "RMRR range");
         Check (F.Regions (1).Scopes = 1
                and then F.Scopes (F.Regions (1).First_Scope).Owner = R.Region_Scope
                and then F.Scopes (F.Regions (1).First_Scope).Path (1).Device = 2,
                "RMRR scope is 00:02.0");
         Check (F.Scope_Total = 4, "four scopes stored");
      end;
   end Test_Laptop;

   procedure Test_QEMU is
      B : constant Buffer := QEMU;
      Result : Test_Result;
   begin
      Decode (B, Result);
      Check (Result.Status = R.Accepted, "QEMU DMAR accepted");
      if Result.Status = R.Accepted then
         Check (Result.Value.Width = 48 and then Result.Value.Unit_Total = 1
                and then Result.Value.Units (1).Include_All
                and then Result.Value.Region_Total = 0, "QEMU facts");
      end if;
   end Test_QEMU;

   procedure Test_Truncation is
      Full : constant Buffer := Laptop;
      Result : Test_Result;
      Wrong_Rejections : Natural := 0;
   begin
      --  A shorter copy with the original header: the extent check fails.
      for N in 0 .. Full.Length - 1 loop
         Decode_Bytes (Full.Data (1 .. N), Result);
         Check (Result.Status = R.Rejected, "prefix" & Natural'Image (N) & " rejected");
      end loop;
      --  A shorter table that declares its own length and checksum: the
      --  structure walk must refuse every cut that is not at a structure end
      --  (48, 72, 104 are structure ends; 48 has no unit).
      for N in R.Fixed_Size .. Full.Length - 1 loop
         declare
            B : Buffer := Full;
         begin
            B.Length := N;
            Seal (B);
            Decode (B, Result);
            if N = 72 or else N = 104 then
               Check (Result.Status = R.Accepted, "cut at structure end" & Natural'Image (N));
            else
               Check (Result.Status = R.Rejected, "cut" & Natural'Image (N) & " rejected");
               if Result.Status = R.Rejected and then
                 Result.Why not in R.Structure_Truncated | R.No_Units | R.Scope_Truncated
               then
                  Wrong_Rejections := Wrong_Rejections + 1;
               end if;
            end if;
         end;
      end loop;
      Check (Wrong_Rejections = 0, "cuts are rejected as truncation");
   end Test_Truncation;

   procedure Test_Mutations is
      Full : constant Buffer := Laptop;
      Result : Test_Result;
      Accepted, Rejected : Natural := 0;
   begin
      --  Every value of every byte after the checksum, checksum refixed:
      --  each decode terminates; an accepted one satisfies Consistent (the
      --  postcondition, checked with -gnata).
      for Offset in 11 .. Full.Length loop
         for V in Byte'Range loop
            if V /= Full.Data (Offset) then
               declare
                  B : Buffer := Full;
               begin
                  B.Data (Offset) := V;
                  Seal (B);
                  Decode (B, Result);
                  if Result.Status = R.Accepted then
                     Accepted := Accepted + 1;
                  else
                     Rejected := Rejected + 1;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      Put_Line ("  mutations: accepted" & Natural'Image (Accepted) &
                ", rejected" & Natural'Image (Rejected));
      Check (Accepted > 0 and then Rejected > 0, "mutations both accept and reject");
   end Test_Mutations;

   procedure Test_Rejections is
      B : Buffer;
   begin
      --  Header and fixed part.
      B := Laptop;
      B.Data (1) := Character'Pos ('X');
      Seal (B);
      Check (Rejected_As (B, R.Header_Rejected), "wrong signature");
      B := Laptop;
      B.Data (10) := B.Data (10) + 1;
      Check (Rejected_As (B, R.Header_Rejected), "bad checksum");
      Begin_Table (B, 30, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Host_Width_Invalid), "31-bit host width");
      Begin_Table (B, 38, 0);
      Seal (B);
      Check (Rejected_As (B, R.No_Units), "no DRHD");

      --  Structure lengths.
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Set_LE (B, 50, 15, 2);
      Seal (B);
      Check (Rejected_As (B, R.Structure_Too_Short), "DRHD length 15");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Set_LE (B, 50, 17, 2);
      Seal (B);
      Check (Rejected_As (B, R.Structure_Truncated), "DRHD length past the end");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Put_LE (B, 1, 2);
      Put_LE (B, 0, 2);              --  zero-length RMRR
      Seal (B);
      Check (Rejected_As (B, R.Structure_Too_Short), "zero-length structure");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Put_Byte (B, 0);               --  three stray bytes
      Put_Byte (B, 0);
      Put_Byte (B, 0);
      Seal (B);
      Check (Rejected_As (B, R.Structure_Truncated), "trailing partial header");

      --  Scopes.
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      B.Data (B.Length - 6) := 5;
      Seal (B);
      Check (Rejected_As (B, R.Scope_Too_Short), "scope length 5");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      B.Data (B.Length - 6) := 9;
      Seal (B);
      Check (Rejected_As (B, R.Scope_Truncated), "scope past its structure");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 7);
      Scope (B, Endpoint, 0, 0, 2, 0);
      B.Length := B.Length - 1;
      B.Data (B.Length - 5) := 7;
      Seal (B);
      Check (Rejected_As (B, R.Scope_Path_Invalid), "odd path");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 32, 0);
      Seal (B);
      Check (Rejected_As (B, R.Scope_Path_Invalid), "device 32");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 2, 8);
      Seal (B);
      Check (Rejected_As (B, R.Scope_Path_Invalid), "function 8");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 6);
      Put_Byte (B, Endpoint);
      Put_Byte (B, 6);
      Put_LE (B, 0, 4);
      Seal (B);
      Check (Rejected_As (B, R.Scope_Path_Invalid), "endpoint with no path");
      Begin_Table (B, 38, 0);
      Unit (B, 0, 0, 16#FED9_0000#, 6 + 2 * (R.Maximum_Path_Hops + 1));
      Put_Byte (B, Bridge);
      Put_Byte (B, Unsigned_64 (6 + 2 * (R.Maximum_Path_Hops + 1)));
      Put_LE (B, 0, 4);
      for I in 1 .. R.Maximum_Path_Hops + 1 loop
         Put_Byte (B, 1);
         Put_Byte (B, 0);
      end loop;
      Seal (B);
      Check (Rejected_As (B, R.Path_Too_Long), "path over the ceiling");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 16);
      Scope (B, 16#77#, 0, 0, 2, 0);   --  undefined scope type: skipped
      Scope (B, IOAPIC_Scope, 0, 0, 2, 0);
      Seal (B);
      declare
         Result : Test_Result;
      begin
         Decode (B, Result);
         Check (Result.Status = R.Accepted and then Result.Value.Scope_Total = 1,
                "undefined scope type skipped");
      end;

      --  Units.
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 0, 0);
      Seal (B);
      Check (Rejected_As (B, R.Unit_Base_Invalid), "zero register base");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0800#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Unit_Base_Invalid), "unaligned register base");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 1, 16#FFFF_FFFF_FFFF_F000#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Unit_Base_Invalid), "wrapping register set");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 5, 16#FED9_0000#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Unit_Size_Unsupported), "register size 32 pages");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Unit (B, 1, 0, 16#FED9_1000#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Include_All_Invalid), "two INCLUDE_PCI_ALL units");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 8);
      Scope (B, Endpoint, 0, 0, 3, 0);
      Seal (B);
      Check (Rejected_As (B, R.Include_All_Invalid), "INCLUDE_PCI_ALL naming an endpoint");
      Begin_Table (B, 38, 0);
      for I in 1 .. R.Maximum_Units + 1 loop
         Unit (B, 0, 0, 16#FED0_0000# + Unsigned_64 (I) * 16#1000#, 0);
      end loop;
      Seal (B);
      Check (Rejected_As (B, R.Too_Many_Units), "units over the ceiling");

      --  Regions.
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Region (B, 16#4C00_0800#, 16#503F_FFFF#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      Seal (B);
      Check (Rejected_As (B, R.Region_Range_Invalid), "unaligned RMRR base");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Region (B, 16#4C00_0000#, 16#503F_F000#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      Seal (B);
      Check (Rejected_As (B, R.Region_Range_Invalid), "RMRR limit not ending a page");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Region (B, 16#5000_0000#, 16#4FFF_FFFF#, 8);
      Scope (B, Endpoint, 0, 0, 2, 0);
      Seal (B);
      Check (Rejected_As (B, R.Region_Range_Invalid), "reversed RMRR");
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Region (B, 16#4C00_0000#, 16#503F_FFFF#, 0);
      Seal (B);
      Check (Rejected_As (B, R.Region_Without_Scope), "RMRR naming no device");

      --  Unknown structure types are skipped by length.
      Begin_Table (B, 38, 0);
      Unit (B, 1, 0, 16#FED9_0000#, 0);
      Put_LE (B, 16#99#, 2);
      Put_LE (B, 6, 2);
      Put_LE (B, 0, 2);
      Seal (B);
      declare
         Result : Test_Result;
      begin
         Decode (B, Result);
         Check (Result.Status = R.Accepted, "unknown structure skipped");
      end;
   end Test_Rejections;

   ---------------------------------------------------------------------------
   --  Page-table arithmetic
   ---------------------------------------------------------------------------
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed * 6364136223846793005 + 1442695040888963407;
      return Seed;
   end Next;

   procedure Test_Arithmetic is
      A : constant Unsigned_64 := 16#0000_0041_2345_6789#;
   begin
      Check (Levels (Width_39) = 3 and then Levels (Width_48) = 4, "levels");
      Check (Width_Bits (Width_39) = 39 and then Width_Bits (Width_48) = 48, "widths");
      Check (Index_Of (A, 1) = Natural (Shift_Right (A, 12) and 511), "index level 1");
      Check (Index_Of (A, 2) = Natural (Shift_Right (A, 21) and 511), "index level 2");
      Check (Index_Of (A, 3) = Natural (Shift_Right (A, 30) and 511), "index level 3");
      Check (Index_Of (A, 4) = 0, "index level 4");
      for I in 1 .. 100_000 loop
         declare
            V48 : constant Unsigned_64 := Next and (2 ** 48 - 1);
            V39 : constant Unsigned_64 := V48 and (2 ** 39 - 1);
         begin
            Check (Compose (V48, Width_48) = V48, "recompose 48");
            Check (Compose (V39, Width_39) = V39, "recompose 39");
         end;
      end loop;
      Check (Leaf_Entry (16#1234_5000#, False) = 16#1234_5001#, "read-only leaf");
      Check (Leaf_Entry (16#1234_5000#, True) = 16#1234_5003#, "writable leaf");
      Check (Table_Entry (16#8000#) = 16#8003#, "table entry");
      Check (not Present (Not_Present), "not present");
      Check (Requester (3, 1, 2) = 16#030A#, "requester ID");
      Check (Bus_Of (16#030A#) = 3 and then Device_Of (16#030A#) = 1
             and then Function_Of (16#030A#) = 2 and then Context_Of (16#030A#) = 16#0A#,
             "requester fields");
      Check (Context_High (Width_39, 5) = 16#0501#, "context high 39");
      Check (Context_High (Width_48, 16#1234#) = 16#12_3402#, "context high 48");
      Check (Root_Entry (16#7000#) = 16#7001#, "root entry");
      Check (Context_Low (16#9000#) = 16#9001#, "context low");
      declare
         C : Width_Choice;
      begin
         C := Choose_Width (SAGAW_39, 39, 39);
         Check (C.Found and then C.Width = Width_39, "39-bit unit, 39-bit host");
         C := Choose_Width (SAGAW_39 or SAGAW_48, 48, 39);
         Check (C.Found and then C.Width = Width_39, "narrowest width first");
         C := Choose_Width (SAGAW_39 or SAGAW_48, 48, 46);
         Check (C.Found and then C.Width = Width_48, "46-bit host needs 48");
         C := Choose_Width (SAGAW_39, 39, 46);
         Check (not C.Found, "39-bit unit cannot cover a 46-bit host");
         C := Choose_Width (SAGAW_48, 39, 39);
         Check (not C.Found, "MGAW below the only width");
         C := Choose_Width (0, 48, 39);
         Check (not C.Found, "no supported width");
      end;
      Check (Run_In_Width (2 ** 27 - 1, 1, Width_39), "last page in 39 bits");
      Check (not Run_In_Width (2 ** 39 - 4096, 2, Width_39), "run past 39 bits");
   end Test_Arithmetic;

   ---------------------------------------------------------------------------
   --  Walker against the ledger
   ---------------------------------------------------------------------------
   Domains_Under_Test : constant := 3;
   Frame_Space : constant := 2048;      --  frames 4096 .. 4096 + Frame_Space
   Frame_Base : constant := 16#1000#;   --  frame numbers (16 MiB up)
   type Root_Array is array (1 .. Domains_Under_Test) of Page_Address;
   Roots : Root_Array;
   W : constant Address_Width := Width_39;

   function Frame_Address (F : Unsigned_64) return Page_Address is (F * Page_Bytes);

   --  Every frame in the space: present in domain D's tables exactly when
   --  the ledger has a Mapped run of D covering it, and identity mapped.
   procedure Check_Agreement (L : ID.Ledger; What : String) is
      Wrong : Natural := 0;
   begin
      for D in 1 .. Domains_Under_Test loop
         for F in Unsigned_64 (Frame_Base) .. Frame_Base + Frame_Space - 1 loop
            declare
               E : constant Unsigned_64 :=
                 Table_Model.Walk.Lookup (Roots (D), W, Frame_Address (F));
               Expected : Boolean := False;
            begin
               for M in ID.Mapping_Index loop
                  if L.Mappings (M).State = ID.Mapped and then L.Mappings (M).Domain = D
                    and then F >= L.Mappings (M).First
                    and then F < L.Mappings (M).First + L.Mappings (M).Pages
                  then
                     Expected := True;
                  end if;
               end loop;
               if Present (E) /= Expected
                 or else (Expected and then Entry_Address (E) /= Frame_Address (F))
               then
                  Wrong := Wrong + 1;
               end if;
            end;
         end loop;
      end loop;
      Check (Wrong = 0, "tables agree with the ledger after " & What);
   end Check_Agreement;

   procedure Map_Run (D : Positive; First, Pages : Unsigned_64; Writable : Boolean;
                      OK : out Boolean) is
   begin
      OK := True;
      for P in 0 .. Pages - 1 loop
         declare
            Done : Boolean;
         begin
            Table_Model.Walk.Map_Page
              (Roots (D), W, Frame_Address (First + P), Frame_Address (First + P), Writable, Done);
            OK := OK and then Done;
         end;
      end loop;
   end Map_Run;

   procedure Unmap_Run (D : Positive; First, Pages : Unsigned_64) is
   begin
      for P in 0 .. Pages - 1 loop
         Table_Model.Walk.Unmap_Page (Roots (D), W, Frame_Address (First + P));
      end loop;
   end Unmap_Run;

   procedure Test_Walker_And_Ledger is
      L : ID.Ledger;
      Owner : constant array (1 .. Domains_Under_Test) of ID.Owner_Key := [101, 202, 303];
      Admit : ID.Admit_Status;
      DS : ID.Domain_Status;
      BS : ID.Bind_Status;
      MS : ID.Map_Status;
      RS : ID.Release_Status;
      M : ID.Mapping_Index;
      D : ID.Domain_Index;
      Created : Boolean;
      OK : Boolean;
      Granted_Runs, Refused_Overlaps, Released_Runs : Natural := 0;
      Reserved_First : constant Unsigned_64 := Frame_Base + Frame_Space - 64;
   begin
      Table_Model.Reset;
      for I in Roots'Range loop
         Table_Model.New_Table (Roots (I), OK);
         Check (OK, "root table");
      end loop;

      --  Domain 1 is the requester an RMRR names (64 frames at the top).
      ID.Admit_Reserved (L, 1, 16#0010#, Reserved_First, 64, Admit);
      Check (Admit = ID.Admitted, "RMRR admitted");
      for I in 1 .. Domains_Under_Test loop
         ID.Create_Domain (L, 1, IO_Page_Tables.Requester_ID (16#0008# * I), D, Created, DS);
         Check (DS = ID.Ready and then Created and then D = I, "domain created");
      end loop;
      ID.Create_Domain (L, 1, 16#0010#, D, Created, DS);
      Check (DS = ID.Ready and then not Created and then D = 2, "same requester, same domain");
      ID.Map_Reserved (L, 2, Reserved_First, 64, M, MS);
      Check (MS = ID.Added, "RMRR mapped for its requester");
      Map_Run (2, Reserved_First, 64, True, OK);
      Check (OK, "RMRR pages mapped");
      ID.Map_Reserved (L, 1, Reserved_First, 64, M, MS);
      Check (MS = ID.Not_Reserved, "RMRR refused for another requester");
      Check_Agreement (L, "RMRR");

      --  Grants need the domain's owner.
      ID.Map_Grant (L, 1, Owner (1), Frame_Base, 4, True, M, MS);
      Check (MS = ID.Not_Owner, "grant before binding refused");
      for I in 1 .. Domains_Under_Test loop
         ID.Bind_Owner (L, I, Owner (I), BS);
         Check (BS = ID.Bound, "bound");
      end loop;
      ID.Bind_Owner (L, 1, Owner (2), BS);
      Check (BS = ID.Busy, "a bound domain refuses another owner");
      ID.Map_Grant (L, 1, Owner (2), Frame_Base, 4, True, M, MS);
      Check (MS = ID.Not_Owner, "another owner cannot map into a domain");
      ID.Map_Grant (L, 3, Owner (3), Reserved_First + 8, 4, True, M, MS);
      Check (MS = ID.Overlaps, "a grant may not overlap an RMRR");

      --  Random grants and retirements.
      for Step in 1 .. 4_000 loop
         declare
            Choice : constant Unsigned_64 := Next mod 3;
            Dom : constant Positive := Natural (Next mod Domains_Under_Test) + 1;
         begin
            if Choice < 2 then
               declare
                  Pages : constant Unsigned_64 := Shift_Left (1, Natural (Next mod 4));
                  First : constant Unsigned_64 :=
                    Frame_Base + (Next mod ((Frame_Space - 64) / Pages)) * Pages;
               begin
                  ID.Map_Grant (L, Dom, Owner (Dom), First, Pages, (Next mod 2) = 0, M, MS);
                  if MS = ID.Added then
                     Map_Run (Dom, First, Pages, L.Mappings (M).Writable, OK);
                     Check (OK, "granted run mapped");
                     Granted_Runs := Granted_Runs + 1;
                  else
                     Check (MS in ID.Overlaps | ID.Capacity, "only overlap or capacity refuses");
                     if MS = ID.Overlaps then
                        Refused_Overlaps := Refused_Overlaps + 1;
                     end if;
                  end if;
               end;
            else
               --  Retire one live grant of the domain, in order.
               for K in ID.Mapping_Index loop
                  if L.Mappings (K).State = ID.Mapped and then L.Mappings (K).Domain = Dom
                    and then L.Mappings (K).Kind = ID.Granted
                  then
                     declare
                        Stamp : Unsigned_64;
                        First : constant Unsigned_64 := L.Mappings (K).First;
                        Pages : constant Unsigned_64 := L.Mappings (K).Pages;
                     begin
                        ID.Release (L, K, RS);
                        Check (RS = ID.Not_Flushed, "a mapped run cannot be released");
                        Unmap_Run (Dom, First, Pages);
                        ID.Begin_Unmap (L, K, Stamp);
                        ID.Release (L, K, RS);
                        Check (RS = ID.Not_Flushed, "a stale run cannot be released");
                        Check (not ID.Reusable (L, First, Pages), "stale frames not reusable");
                        ID.Complete_Flush (L, Dom, Stamp);
                        Check (L.Mappings (K).State = ID.Flushed, "flushed");
                        ID.Release (L, K, RS);
                        Check (RS = ID.Released, "released after flush");
                        Check (ID.Reusable (L, First, Pages), "released frames reusable");
                        Released_Runs := Released_Runs + 1;
                     end;
                     exit;
                  end if;
               end loop;
            end if;
            if Step mod 200 = 0 then
               Check_Agreement (L, "step" & Positive'Image (Step));
            end if;
         end;
      end loop;
      Put_Line ("  ledger: granted" & Natural'Image (Granted_Runs) &
                ", overlap refusals" & Natural'Image (Refused_Overlaps) &
                ", released" & Natural'Image (Released_Runs) &
                ", tables" & Natural'Image (Table_Model.Used));
      Check (Granted_Runs > 100 and then Refused_Overlaps > 10 and then Released_Runs > 100,
             "the random walk exercised grants, overlaps and releases");

      --  A stamp taken before a later unmap does not flush it.
      declare
         Stamp_A, Stamp_B : Unsigned_64;
         A, B : ID.Mapping_Index;
      begin
         --  Clear the domain first.
         for K in ID.Mapping_Index loop
            if L.Mappings (K).State = ID.Mapped and then L.Mappings (K).Domain = 3 then
               declare
                  S : Unsigned_64;
               begin
                  Unmap_Run (3, L.Mappings (K).First, L.Mappings (K).Pages);
                  ID.Begin_Unmap (L, K, S);
                  ID.Complete_Flush (L, 3, S);
                  ID.Release (L, K, RS);
               end;
            end if;
         end loop;
         ID.Map_Grant (L, 3, Owner (3), Frame_Base, 1, True, A, MS);
         Check (MS = ID.Added, "grant A");
         ID.Map_Grant (L, 3, Owner (3), Frame_Base + 1, 1, True, B, MS);
         Check (MS = ID.Added, "grant B");
         ID.Begin_Unmap (L, A, Stamp_A);
         ID.Begin_Unmap (L, B, Stamp_B);
         ID.Complete_Flush (L, 3, Stamp_A);
         Check (L.Mappings (A).State = ID.Flushed and then L.Mappings (B).State = ID.Stale,
                "an invalidation stamped before B's unmap does not flush B");
      end;

      --  Owner ends: its domains lose the owner, keep the mappings.
      declare
         Before : constant ID.Mapping_Table := L.Mappings;
      begin
         ID.Release_Owner (L, Owner (2));
         Check (L.Domains (2).Owner = ID.No_Owner and then L.Mappings = Before,
                "release owner keeps mappings");
         ID.Bind_Owner (L, 2, 999, BS);
         Check (BS = ID.Bound, "a released domain binds a new owner");
      end;
   end Test_Walker_And_Ledger;

begin
   Test_Laptop;
   Test_QEMU;
   Test_Truncation;
   Test_Mutations;
   Test_Rejections;
   Test_Arithmetic;
   Test_Walker_And_Ledger;
   Put_Line ("iommu:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) &
             " failures");
   if Failures > 0 then
      raise Program_Error with "iommu tests failed";
   end if;
end Main;
