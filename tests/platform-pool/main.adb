--  ACPI-POOL-001 hosted tests: the pool's range set, claims, mapping
--  tables and admission (kernel Platform_Pool_Sets), sized from discovery,
--  with exclusive (Device_BAR) and shared (Chipset_Shared) members, against
--  a page-level oracle too; devmgr's discovery (Devmgr_Platform_Pool) and
--  its sideband layout (Devmgr_Sideband_Layout).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Platform_Pool_Sets; use Platform_Pool_Sets;
with Platform_Pool_ABI;
with Devmgr_Platform_Pool;
with Devmgr_Sideband_Layout;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function I (First, Last : Page_Number) return Interval is ((First, Last));
   function Bar (First, Last : Page_Number) return Member is ((I (First, Last), Device_BAR));
   function Shared (First, Last : Page_Number) return Member is
     ((I (First, Last), Chipset_Shared));

   Driver  : constant Identity := 16#0000_0001_0000_0007#;
   ACPI    : constant Identity := 16#0000_0001_0000_0009#;
   Other   : constant Identity := 16#0000_0002_0000_000B#;

   procedure Members_And_Admission is
      S  : State (Members_Room => 8, Claims_Room => 8);
      St : Insert_Status;
      C  : Claim_Status;
      D  : Decision;
   begin
      Insert (S, Bar (100, 131), St);            --  a device BAR, 32 pages
      Check (St = Inserted, "first member inserted");
      Insert (S, Bar (10, 19), St);
      Check (St = Inserted, "member below inserted");
      Insert (S, Shared (200, 200), St);
      Check (St = Inserted, "member above inserted");
      Check (S.Member_Total = 3 and then S.Members (1) = Bar (10, 19)
             and then S.Members (2) = Bar (100, 131) and then S.Members (3) = Shared (200, 200),
             "members kept sorted, with their kinds");
      Insert (S, Bar (131, 140), St);
      Check (St = Overlaps_Member and then S.Member_Total = 3, "overlapping member refused");
      Insert (S, Shared (9, 10), St);
      Check (St = Overlaps_Member, "overlap at the low edge refused (whatever its kind)");
      Insert (S, Bar (5, 4), St);
      Check (St = Invalid_Interval, "empty interval refused");
      Insert (S, Bar (20, 99), St);
      Check (St = Inserted, "adjacent member between two inserted");
      Check (Well_Formed (S), "still sorted and disjoint");

      D := Decide (S, ACPI, I (100, 100));
      Check (D.Kind = Admitted and then S.Members (D.Member) = Bar (100, 131)
             and then D.Claim = 0,
             "a page inside a member is admitted");
      D := Decide (S, ACPI, I (128, 131));
      Check (D.Kind = Admitted, "the member's last pages are admitted");
      D := Decide (S, ACPI, I (130, 132));
      Check (D.Kind = Outside_Pool, "a request past a member's end is outside");
      D := Decide (S, ACPI, I (99, 100));
      Check (D.Kind = Outside_Pool, "a request straddling two members is outside");
      D := Decide (S, ACPI, I (150, 150));
      Check (D.Kind = Outside_Pool, "a page in no member is outside");
      D := Decide (S, ACPI, I (7, 3));
      Check (D.Kind = Invalid_Request, "an empty request is invalid");

      --  A bound driver's BAR inside the Device_BAR member: exclusive.
      Add_Claim (S, I (110, 111), Driver, C);
      Check (C = Recorded, "the driver's claim is recorded");
      D := Decide (S, ACPI, I (108, 115));
      Check (D.Kind = Claimed and then S.Claims (D.Claim).Claimant = Driver
             and then S.Claims (D.Claim).Pages = I (110, 111)
             and then S.Members (D.Member) = Bar (100, 131),
             "exclusive: a request overlapping a driver's BAR is refused, naming the driver");
      D := Decide (S, Driver, I (108, 115));
      Check (D.Kind = Admitted, "the claimant's own claim never refuses it");
      D := Decide (S, ACPI, I (112, 131));
      Check (D.Kind = Admitted, "the rest of the member is still admitted");
      Add_Claim (S, I (110, 111), Driver, C);
      Check (C = Duplicate and then S.Claim_Total = 1, "the same claim again is a duplicate");
      Add_Claim (S, I (150, 160), Driver, C);
      Check (C = Outside_Members and then S.Claim_Total = 1,
             "a claim over no member is not recorded (it can refuse nothing)");

      --  A driver claiming a chipset block: shared.
      Add_Claim (S, I (200, 200), Other, C);
      Check (C = Recorded, "a claim over a shared member is recorded");
      D := Decide (S, ACPI, I (200, 200));
      Check (D.Kind = Admitted and then D.Claim /= 0
             and then S.Claims (D.Claim).Claimant = Other,
             "shared: a request in a claimed Chipset_Shared member is still admitted, naming the sharer");

      --  Unbind (the driver exits): its claims go, the range returns.
      Release_Claimant (S, Driver);
      D := Decide (S, ACPI, I (108, 115));
      Check (D.Kind = Admitted and then D.Claim = 0, "after unbind the range is admitted again");
      Check (S.Claim_Total = 1 and then S.Claims (1).Claimant = Other,
             "other claimants' claims stay");
   end Members_And_Admission;

   procedure Claim_After_Map is
      S  : State (Members_Room => 4, Claims_Room => 4);
      T  : Mapping_Table (Room => 8);
      U  : Mapping_Table (Room => 8);   --  the driver's own table
      St : Insert_Status;
      A  : Add_Status;
      C  : Claim_Status;
      Found : Boolean;
      Taken : Mapping;
      M1 : constant Mapping := (I (100, 100), ACPI, 16#4000#, Device_BAR);
      M2 : constant Mapping := (I (120, 121), ACPI, 16#4010#, Device_BAR);
      M3 : constant Mapping := (I (105, 105), Driver, 16#9000#, Device_BAR);
      M4 : constant Mapping := (I (300, 300), ACPI, 16#4020#, Chipset_Shared);
   begin
      Insert (S, Bar (100, 131), St);
      Insert (S, Shared (300, 315), St);
      Add_Mapping (S, T, M1, A);
      Check (A = Added, "first mapping recorded");
      Add_Mapping (S, T, M1, A);
      Check (A = Already_Recorded and then T.Total = 1, "the same mapping recorded once");
      Add_Mapping (S, T, M2, A);
      Add_Mapping (S, T, M4, A);
      Add_Mapping (S, U, M3, A);
      Check (T.Total = 3 and then U.Total = 1 and then Consistent (S, T) and then Consistent (S, U),
             "per-holder tables, consistent");
      --  The driver binds and claims a BAR over M2 after it was mapped, and
      --  a chipset block over M4.
      Add_Claim (S, I (121, 125), Driver, C);
      Add_Claim (S, I (300, 301), Driver, C);
      Check (not Consistent (S, T), "a claim over a device-BAR mapping makes it conflict");
      Take_Conflicting (S, T, Found, Taken);
      Check (Found and then Taken = M2 and then Taken.Kind = Device_BAR,
             "the overlapping ACPI device-BAR mapping is taken");
      Take_Conflicting (S, T, Found, Taken);
      Check (not Found and then Consistent (S, T) and then T.Total = 2,
             "nothing else conflicts: M1 and the shared M4 stay");
      Check ((for some K in 1 .. T.Total => T.Items (K) = M4),
             "shared: the claim over the chipset block never revoked ACPI's mapping");
      Take_Conflicting (S, U, Found, Taken);
      Check (not Found and then U.Total = 1, "the driver's own mapping is never its own conflict");
   end Claim_After_Map;

   --  Sized from discovery: the declared rooms bound the tables; past them
   --  a typed refusal, and nothing else changes (no self-disable).
   procedure Derived_Capacity is
      package P renames Devmgr_Platform_Pool;
      Found_Members : constant := 3;
      Device_Extents : constant := 2;
      S  : State (Members_Room => Natural (P.Member_Room (Found_Members)),
                  Claims_Room  => Natural (P.Claim_Room (Found_Members, Device_Extents)));
      T  : Mapping_Table (Room => 2);
      Grown : Mapping_Table (Room => 4);
      St : Insert_Status;
      C  : Claim_Status;
      A  : Add_Status;
      D  : Decision;
      Found : Boolean;
      Taken : Mapping;
   begin
      Check (S.Members_Room = 3 and then S.Claims_Room = 5,
             "devmgr's rooms: 3 members found, 3 + 2 device BAR extents of claims");
      Check (P.Member_Room (0) = 1 and then P.Claim_Room (0, 0) = 1,
             "a room is never zero");
      Check (P.Member_Room (Natural'Last) = Platform_Pool_ABI.Maximum_Members
             and then P.Claim_Room (Natural'Last, Natural'Last) = Platform_Pool_ABI.Maximum_Claims,
             "a room never passes its ceiling");
      Insert (S, Bar (0, 99), St);
      Insert (S, Shared (200, 299), St);
      Insert (S, Bar (400, 499), St);
      Check (S.Member_Total = 3, "three members");
      Insert (S, Bar (600, 600), St);
      Check (St = Members_Full and then S.Member_Total = 3,
             "a fourth member past the declared room: Members_Full (CAPACITY)");

      Add_Mapping (S, T, (I (50, 50), ACPI, 1, Device_BAR), A);
      for K in 1 .. 5 loop
         Add_Claim (S, I (Page_Number (K), Page_Number (K)), Other, C);
         Check (C = Recorded, "claims fill the declared room");
      end loop;
      Add_Claim (S, I (60, 60), Other, C);
      Check (C = Claims_Full and then S.Claim_Total = 5,
             "a claim past the room: Claims_Full (the delegation is refused)");
      D := Decide (S, ACPI, I (50, 50));
      Check (D.Kind = Admitted, "no self-disable: the pool still admits what it did");
      D := Decide (S, ACPI, I (60, 60));
      Check (D.Kind = Admitted,
             "the refused claim was never recorded (its delegation never happened)");
      Take_Conflicting (S, T, Found, Taken);
      Check (not Found and then T.Total = 1, "and no mapping is taken for it");

      --  A holder's table grows (Copy into a larger one).
      Add_Mapping (S, T, (I (51, 51), ACPI, 2, Device_BAR), A);
      Add_Mapping (S, T, (I (52, 52), ACPI, 3, Device_BAR), A);
      Check (A = Table_Full and then T.Total = 2, "a full table refuses: Table_Full");
      Copy (T, Grown);
      Check (Same_Items (T, Grown) and then Grown.Total = 2, "the grown table holds the same mappings");
      Add_Mapping (S, Grown, (I (52, 52), ACPI, 3, Device_BAR), A);
      Check (A = Added and then Grown.Total = 3 and then Consistent (S, Grown),
             "the mapping fits after growth");
   end Derived_Capacity;

   --  Random states against a page-level model: admitted iff the request
   --  lies in one member and, unless that member is shared, no other
   --  process's claim touches it.
   procedure Oracle is
      Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
      Universe : constant := 64;
      function Next (Bound : Unsigned_64) return Unsigned_64 is
      begin
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 7);
         Seed := Seed xor Shift_Left (Seed, 17);
         return Seed mod Bound;
      end Next;
      Mismatches : Natural := 0;
      Revoked_Shared : Natural := 0;
   begin
      for Round in 1 .. 2000 loop
         declare
            S  : State (Members_Room => 6, Claims_Room => 3);
            T  : Mapping_Table (Room => 4);
            St : Insert_Status;
            C  : Claim_Status;
            A  : Add_Status;
            Found : Boolean;
            Taken : Mapping;
            Member_Of : array (Page_Number range 0 .. Universe - 1) of Natural := [others => 0];
            Shared_Page : array (Page_Number range 0 .. Universe - 1) of Boolean := [others => False];
            Claimed_By_Other : array (Page_Number range 0 .. Universe - 1) of Boolean :=
              [others => False];
            Holder : constant Identity := (if Next (2) = 0 then ACPI else Driver);
         begin
            for K in 1 .. 6 loop
               declare
                  F : constant Page_Number := Next (Universe);
                  L : constant Page_Number := Unsigned_64'Min (Universe - 1, F + Next (8));
                  Kind : constant Pool_Member_Kind := (if Next (2) = 0 then Device_BAR else Chipset_Shared);
               begin
                  Insert (S, (I (F, L), Kind), St);
                  if St = Inserted then
                     for P in F .. L loop
                        Member_Of (P) := K;
                        Shared_Page (P) := Kind = Chipset_Shared;
                     end loop;
                  end if;
               end;
            end loop;
            --  The holder maps a few pages before the claims.
            for K in 1 .. 4 loop
               declare
                  F : constant Page_Number := Next (Universe);
                  D : constant Decision := Decide (S, Holder, I (F, F));
               begin
                  if D.Kind = Admitted then
                     Add_Mapping (S, T, (I (F, F), Holder, Unsigned_64 (K),
                                         S.Members (D.Member).Kind), A);
                  end if;
               end;
            end loop;
            for K in 1 .. 3 loop
               declare
                  F : constant Page_Number := Next (Universe);
                  L : constant Page_Number := Unsigned_64'Min (Universe - 1, F + Next (4));
                  Who : constant Identity := (if Next (2) = 0 then Driver else Other);
               begin
                  Add_Claim (S, I (F, L), Who, C);
                  if C = Recorded and then Who /= Holder then
                     for P in F .. L loop
                        Claimed_By_Other (P) := True;
                     end loop;
                  end if;
               end;
            end loop;
            loop
               Take_Conflicting (S, T, Found, Taken);
               exit when not Found;
               if Shared_Page (Taken.Pages.First) then
                  Revoked_Shared := Revoked_Shared + 1;
               end if;
            end loop;
            for K in 1 .. T.Total loop
               if not Shared_Page (T.Items (K).Pages.First)
                 and then Claimed_By_Other (T.Items (K).Pages.First)
               then
                  Mismatches := Mismatches + 1;   --  an exclusive page kept
               end if;
            end loop;
            declare
               F : constant Page_Number := Next (Universe);
               L : constant Page_Number := Unsigned_64'Min (Universe - 1, F + Next (6));
               Expected : Boolean := Member_Of (F) /= 0;
               D : constant Decision := Decide (S, Holder, I (F, L));
            begin
               for P in F .. L loop
                  Expected := Expected and then Member_Of (P) = Member_Of (F)
                    and then (Shared_Page (P) or else not Claimed_By_Other (P));
               end loop;
               if (D.Kind = Admitted) /= Expected then
                  Mismatches := Mismatches + 1;
               end if;
            end;
         end;
      end loop;
      Check (Mismatches = 0, "2000 random states agree with the page model (exclusive and shared)");
      Check (Revoked_Shared = 0, "no shared mapping was ever revoked");
   end Oracle;

   procedure Discovery is
      package P renames Devmgr_Platform_Pool;
      use type P.Bridge_Kind;
      use type P.Block_Status;
      use type P.Span;
      B : P.Block;
      Lo, Hi : P.Span;
   begin
      Check (P.Bridge_Of (16#8086#, 16#461C#, False) = P.Alder_Lake_N,
             "the IdeaPad's host bridge 8086:461C is Alder Lake-N");
      Check (P.Bridge_Of (16#8086#, 16#4601#, False) = P.Unknown,
             "an Alder Lake-P bridge is not assumed to share the layout here");
      Check (P.Bridge_Of (16#8086#, 16#29C0#, False) = P.Unknown,
             "q35 without the test profile is an unknown bridge");
      Check (P.Bridge_Of (16#8086#, 16#29C0#, True) = P.QEMU_Q35_Test,
             "q35 with the test profile");
      Check (P.Bridge_Of (16#8086#, 16#461C#, True) = P.Alder_Lake_N,
             "the test profile never changes real hardware");
      Check (P.Bridge_Of (16#1022#, 16#14D8#, True) = P.Unknown,
             "another vendor's bridge is unknown even with the profile");

      B := P.Discover (P.Alder_Lake_N, 16#FEDC_0001#, 0, 0, 0);
      Check (B.Status = P.Found and then B.Base = 16#FEDC_0000# and then B.Bytes = 16#2_0000#,
             "MCHBAR 0xFEDC0001: 128 KiB at 0xFEDC0000");
      B := P.Discover (P.Alder_Lake_N, 16#FEDC_0000#, 0, 0, 0);
      Check (B.Status = P.Disabled, "MCHBAREN clear: disabled");
      B := P.Discover (P.Alder_Lake_N, 1, 0, 0, 0);
      Check (B.Status = P.Not_Placed, "an enabled zero base is not placed");
      B := P.Discover (P.Alder_Lake_N, 16#FEDD_8001#, 0, 0, 0);
      Check (B.Status = P.Found and then B.Base = 16#FEDC_0000#,
             "bits below 17 are not base bits");
      B := P.Discover (P.Alder_Lake_N, 16#0002_0001#, 16#0000_FC00#, 0, 0);
      Check (B.Status = P.Found and then B.Base = 16#0000_0000_0002_0000#,
             "bits above 41 are not base bits");
      B := P.Discover (P.Alder_Lake_N, 16#FFFE_0001#, 16#FFFF_FFFF#, 0, 0);
      Check (B.Status = P.Found and then B.Base = 16#0000_03FF_FFFE_0000#,
             "the highest base the mask allows is below the physical limit");
      B := P.Discover (P.Unknown, 16#FEDC_0001#, 0, 0, 0);
      Check (B.Status = P.Unknown_Host_Bridge, "an unknown bridge's 0x48 is never read as MCHBAR");
      B := P.Discover (P.QEMU_Q35_Test, 0, 0, 16#FEA0_0000#, 16#10_0000#);
      Check (B.Status = P.Found and then B.Base = 16#FEA0_0000# and then B.Bytes = 16#2_0000#,
             "the test block is the first 128 KiB of edu's BAR0");
      B := P.Discover (P.QEMU_Q35_Test, 0, 0, 16#FEA0_0000#, 16#8_0000#);
      Check (B.Status = P.Not_Placed, "a test BAR of another size is refused");
      B := P.Discover (P.QEMU_Q35_Test, 0, 0, 16#FEA0_0010#, 16#10_0000#);
      Check (B.Status = P.Not_Placed, "a misaligned test BAR is refused");

      P.Outside ((16#1000#, 16#8000#), (16#3000#, 16#2000#), Lo, Hi);
      Check (Lo = (16#1000#, 16#2000#) and then Hi = (16#5000#, 16#4000#),
             "a span around the block splits in two");
      P.Outside ((16#1000#, 16#1000#), (16#3000#, 16#2000#), Lo, Hi);
      Check (Lo = (16#1000#, 16#1000#) and then Hi.Bytes = 0, "a disjoint span is kept whole");
      P.Outside ((16#3000#, 16#1000#), (16#2000#, 16#4000#), Lo, Hi);
      Check (Lo.Bytes = 0 and then Hi.Bytes = 0, "a span inside the block is dropped");
   end Discovery;

   --  The IdeaPad's own LPIT (laptop-acpi-1-tables/lpit.dat, 204 bytes):
   --  UID 0 is an FFH counter (MSR 0x632), UID 1 a SystemMemory counter at
   --  0xFE00193C, UID 2 disabled (flags 1).
   Laptop_LPIT : constant Devmgr_Platform_Pool.Byte_Array (0 .. 203) :=
     [16#4C#, 16#50#, 16#49#, 16#54#, 16#CC#, 16#00#, 16#00#, 16#00#, 16#01#, 16#54#, 16#4C#, 16#45#,
      16#4E#, 16#4F#, 16#56#, 16#4F#, 16#43#, 16#42#, 16#2D#, 16#30#, 16#31#, 16#20#, 16#20#, 16#20#,
      16#01#, 16#00#, 16#00#, 16#00#, 16#41#, 16#43#, 16#50#, 16#49#, 16#00#, 16#00#, 16#04#, 16#00#,
      --  descriptor UID 0 (0x24)
      16#00#, 16#00#, 16#00#, 16#00#, 16#38#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#7F#, 16#01#, 16#02#, 16#00#, 16#60#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#30#, 16#75#, 16#00#, 16#00#, 16#B8#, 16#0B#, 16#00#, 16#00#,
      16#7F#, 16#40#, 16#00#, 16#00#, 16#32#, 16#06#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      --  descriptor UID 1 (0x5C)
      16#00#, 16#00#, 16#00#, 16#00#, 16#38#, 16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#7F#, 16#01#, 16#02#, 16#00#, 16#60#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#30#, 16#75#, 16#00#, 16#00#, 16#B8#, 16#0B#, 16#00#, 16#00#,
      16#00#, 16#20#, 16#00#, 16#03#, 16#3C#, 16#19#, 16#00#, 16#FE#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#05#, 16#20#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      --  descriptor UID 2 (0x94)
      16#00#, 16#00#, 16#00#, 16#00#, 16#38#, 16#00#, 16#00#, 16#00#, 16#02#, 16#00#, 16#00#, 16#00#,
      16#01#, 16#00#, 16#00#, 16#00#, 16#7F#, 16#01#, 16#02#, 16#00#, 16#60#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#30#, 16#75#, 16#00#, 16#00#, 16#B8#, 16#0B#, 16#00#, 16#00#,
      16#00#, 16#20#, 16#00#, 16#03#, 16#FF#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#];

   procedure Platform_Blocks is
      package P renames Devmgr_Platform_Pool;
      package L renames Devmgr_Sideband_Layout;
      use type P.Block_Status;
      use type L.P2SB_Action;
      B, W : P.Block;
      Short : P.Byte_Array (0 .. 99) := Laptop_LPIT (0 .. 99);
      Zero_Length : P.Byte_Array := Laptop_LPIT;
      Disabled_Memory : P.Byte_Array := Laptop_LPIT;
   begin
      --  PMC from the LPIT.
      Check (P.LPIT_Memory_Counter (Laptop_LPIT) = 16#FE00_193C#,
             "the laptop's LPIT names its SystemMemory counter 0xFE00193C (UID 1)");
      B := P.PWRM_Block (P.LPIT_Memory_Counter (Laptop_LPIT));
      Check (B.Status = P.Found and then B.Base = 16#FE00_0000# and then B.Bytes = 16#1_0000#,
             "PWRMBASE 0xFE000000, 64 KiB (coreboot PCH_PWRM_BASE_ADDRESS)");
      Check (P.PWRM_Block (0).Status = P.No_Source, "no LPIT counter: no PMC block, no guess");
      Check (P.PWRM_Block (16#FE00_1000#).Status = P.Not_Placed,
             "a counter not at 0x193C into an aligned window is refused");
      Check (P.PWRM_Block (16#100#).Status = P.Not_Placed, "a counter below the offset is refused");
      Short (4) := 100;
      Check (P.LPIT_Memory_Counter (Short) = 0, "a truncated LPIT names no counter");
      Zero_Length (16#24# + 4) := 0;
      Check (P.LPIT_Memory_Counter (Zero_Length) = 0,
             "a descriptor shorter than a native one ends the walk (no loop)");
      Disabled_Memory (16#5C# + 12) := 1;
      Check (P.LPIT_Memory_Counter (Disabled_Memory) = 0,
             "a disabled SystemMemory descriptor is skipped (Linux: flags must be 0)");

      --  P2SB.
      Check (L.Action_Of (16#FFFF#, 16#FFFF_FFFF#) = L.Unhide_Read,
             "hidden P2SB (all ones): unhide, read, hide");
      Check (L.Action_Of (16#0580#, 0) = L.Read_Visible, "a visible P2SB is read directly");
      Check (L.Action_Of (16#0580#, L.P2SB_Hide) = L.Unhide_Read,
             "a P2SB with HIDE set is unhidden for the read");
      Check (L.Action_Of (16#0C05#, 16#FFFF_FFFF#) = L.Leave_Alone,
             "another class at 00:1F.1 is never touched");
      Check (L.Action_Of (16#FFFF#, 0) = L.Leave_Alone, "nothing there and HIDE clear: left alone");
      W := L.Decode_SBREG (16#FD00_0004#, 0);
      Check (W.Status = P.Found and then W.Base = 16#FD00_0000# and then W.Bytes = 16#100_0000#,
             "SBREG 64-bit BAR 0xFD000004: 16 MiB at 0xFD000000");
      Check (L.Decode_SBREG (16#FD00_0000#, 0).Status = P.Found, "a 32-bit SBREG BAR");
      Check (L.Decode_SBREG (16#FFFF_FFFF#, 16#FFFF_FFFF#).Status = P.Not_Placed,
             "a still-hidden P2SB's all-ones BAR is refused");
      Check (L.Decode_SBREG (16#FD80_0004#, 0).Status = P.Not_Placed,
             "an SBREG not 16 MiB aligned is refused");
      Check (L.Decode_SBREG (16#FD00_0001#, 0).Status = P.Not_Placed, "an I/O BAR is refused");
      Check (L.Decode_SBREG (16#0000_0004#, 0).Status = P.Not_Placed, "a zero SBREG is refused");
      Check (L.Decode_SBREG (16#0000_0004#, 16#0001_0000#).Status = P.Beyond_Limit,
             "an SBREG beyond the physical limit is refused");
      --  The laptop's DSDT GPI0._CRS: CML0 = SBRG + 0x6E0000 ...
      Check (L.Port_Block (W, 16#6E#).Base = 16#FD6E_0000#
             and then L.Port_Block (W, 16#6D#).Base = 16#FD6D_0000#
             and then L.Port_Block (W, 16#6A#).Base = 16#FD6A_0000#
             and then L.Port_Block (W, 16#69#).Base = 16#FD69_0000#
             and then L.Port_Block (W, 16#69#).Bytes = 16#1_0000#,
             "ADL-N GPIO communities 0, 1, 4, 5 at SBREG + port << 16, 64 KiB each");
      Check (L.ADL_N_GPIO_Ports'Length = 4, "four communities on Alder Lake-N (Linux adln_communities)");

      --  The test profile's stand-ins.
      B := L.Test_Window (16#FEA0_0000#);
      Check (B.Status = P.Found and then B.Base = 16#FEA2_0000# and then B.Bytes = 16#E_0000#,
             "test sideband window: edu BAR0 past its first 128 KiB");
      B := L.Test_Community (16#FEA0_0000#);
      Check (B.Status = P.Found and then B.Base = 16#FEAC_0000# and then B.Bytes = 16#1_0000#,
             "test community: 64 KiB at edu BAR0 + 0xC0000");
   end Platform_Blocks;

   --  The pool devmgr builds on the IdeaPad, as members of the kernel's
   --  set, and the image-10 witness: AML's MBAR = MCHBAR + 0x7110, 4 KiB,
   --  so two pages MCHBAR+0x7000 .. +0x9000 (beyond the 32 KiB the
   --  firmware's PDRC reserves, inside the 128 KiB window).
   procedure Laptop_Pool is
      package P renames Devmgr_Platform_Pool;
      package L renames Devmgr_Sideband_Layout;
      Members : constant := 9;
      S  : State (Members_Room => Natural (P.Member_Room (Members)),
                  Claims_Room  => Natural (P.Claim_Room (Members, 40)));
      T  : Mapping_Table (Room => 4);
      St : Insert_Status;
      D  : Decision;
      A  : Add_Status;
      C  : Claim_Status;
      Found : Boolean;
      Taken : Mapping;
      Page : constant := 4096;
      MCHBAR : constant P.Block := P.Decode_MCHBAR (16#FEDC_0001#, 0);
      PMC : constant P.Block := P.PWRM_Block (P.LPIT_Memory_Counter (Laptop_LPIT));
      SBREG : constant P.Block := L.Decode_SBREG (16#FD00_0004#, 0);
      function Pages (B : P.Block) return Interval is
        ((B.Base / Page, (B.Base + B.Bytes) / Page - 1));
      function Pages (First, Bytes : Unsigned_64) return Interval is
        ((First / Page, (First + Bytes - 1) / Page));
      function Chip (B : P.Block) return Member is ((Pages (B), Chipset_Shared));
      function Chip (First, Bytes : Unsigned_64) return Member is
        ((Pages (First, Bytes), Chipset_Shared));
      Community_4 : constant P.Block := L.Port_Block (SBREG, 16#6A#);
      GPIO_Driver : constant Identity := 16#0000_0003_0000_0011#;
      MBAR : constant Unsigned_64 := 16#FEDC_7110#;
      MBAR_Bytes : constant := 16#1000#;
      Pad : constant Mapping :=
        (Pages (Community_4.Base + 16#700#, 16#100#), ACPI, 16#5000#, Chipset_Shared);
   begin
      Insert (S, Chip (MCHBAR), St);
      Check (St = Inserted, "laptop: MCHBAR member (shared)");
      Insert (S, Chip (PMC), St);
      Check (St = Inserted, "laptop: PMC member (shared)");
      --  The sideband window: communities, then what lies between them.
      for Port of L.ADL_N_GPIO_Ports loop
         Insert (S, Chip (L.Port_Block (SBREG, Port)), St);
         Check (St = Inserted, "laptop: GPIO community member (shared)");
      end loop;
      Insert (S, Chip (16#FD00_0000#, 16#69_0000#), St);
      Check (St = Inserted, "laptop: sideband below community 5");
      Insert (S, Chip (16#FD6B_0000#, 16#2_0000#), St);
      Check (St = Inserted, "laptop: sideband between communities 4 and 1");
      Insert (S, Chip (16#FD6F_0000#, 16#91_0000#), St);
      Check (St = Inserted, "laptop: sideband above community 0");
      Insert (S, Chip (16#FD00_0000#, 16#100_0000#), St);
      Check (St = Overlaps_Member, "laptop: the whole window again overlaps its pieces");
      Check (Well_Formed (S) and then S.Member_Total = 9 and then S.Members_Room = 9,
             "laptop: nine disjoint members, the declared room exactly");

      --  The witness (regression): two pages inside MCHBAR's window.
      D := Decide (S, ACPI, (MBAR / Page, (MBAR + MBAR_Bytes - 1) / Page));
      Check (D.Kind = Admitted and then S.Members (D.Member) = Chip (MCHBAR)
             and then Pages (MBAR, MBAR_Bytes) = Pages (16#FEDC_7000#, 16#2000#),
             "image-10 MBAR 0xFEDC7110 +4 KiB: two pages at MCHBAR+0x7000 admitted");
      D := Decide (S, ACPI, Pages (16#FEDD_F000#, 16#2000#));
      Check (D.Kind = Outside_Pool, "past MCHBAR's 128 KiB is outside");
      D := Decide (S, ACPI, Pages (16#FE00_1000#, 16#1000#));
      Check (D.Kind = Admitted and then S.Members (D.Member) = Chip (PMC),
             "the PMC's PWMR region (PWRM, 0x1E30) admitted");
      D := Decide (S, ACPI, Pages (16#FD6A_0000# + 16#F000#, 16#2000#));
      Check (D.Kind = Outside_Pool, "a request straddling community 4 and the next port is outside");
      D := Decide (S, ACPI, Pages (16#FD8C_8000#, 16#1000#));
      Check (D.Kind = Admitted, "a sideband port (ICLK-like) is admitted");

      --  A future GPIO driver binds community 4 after ACPI mapped a pad
      --  there: the claim is shared (Linux lax-style), ACPI's mapping
      --  stays, new mappings are still admitted, naming the sharer.
      Add_Mapping (S, T, Pad, A);
      Check (A = Added, "laptop: ACPI maps a pad in community 4");
      Add_Claim (S, Pages (Community_4), GPIO_Driver, C);
      Check (C = Recorded, "laptop: the GPIO driver's claim of community 4 is recorded");
      Take_Conflicting (S, T, Found, Taken);
      Check (not Found and then T.Total = 1 and then T.Items (1) = Pad and then Consistent (S, T),
             "shared: the GPIO driver's claim of community 4 keeps ACPI's mapping");
      D := Decide (S, ACPI, Pages (16#FD6A_0000#, 16#1000#));
      Check (D.Kind = Admitted and then D.Claim /= 0
             and then S.Claims (D.Claim).Claimant = GPIO_Driver,
             "community 4 is still admitted, sharing with the GPIO driver");
      Release_Claimant (S, GPIO_Driver);
      D := Decide (S, ACPI, Pages (16#FD6A_0000#, 16#1000#));
      Check (D.Kind = Admitted and then D.Claim = 0,
             "after the GPIO driver exits community 4 is unshared");
   end Laptop_Pool;

begin
   Members_And_Admission;
   Claim_After_Map;
   Derived_Capacity;
   Oracle;
   Discovery;
   Platform_Blocks;
   Laptop_Pool;
   Put_Line ("platform-pool:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
