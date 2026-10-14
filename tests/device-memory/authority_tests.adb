pragma Ada_2022;
--  CAP-001 hosted tests: the physical classification, device-memory
--  authorization (MAP_DEVICE / MAP_INTO / MAPFB), the POLICY_MINT range bound
--  and attenuation. Hostile ranges: one page into RAM, sub-page RAM
--  fragments, wrapping, zero size, huge, and exactly at every boundary.
--  An independent page-by-page oracle checks the classification on many
--  generated maps.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Device_Memory_Admission;
with Device_Memory_Authority; use Device_Memory_Authority;
with Physical_Ranges; use Physical_Ranges;
with Device_Memory_Publication;

procedure Authority_Tests is
   package DMA renames Device_Memory_Admission;
   Read_Write : constant DMA.Access_Mode := DMA.Read_Write;
   Read_Only  : constant DMA.Access_Mode := DMA.Read_Only;
   Checks : Natural := 0;
   Failures : Natural := 0;

   procedure Expect (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Expect;

   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;
   GiB : constant Unsigned_64 := 1024 * MiB;
   Page : constant := Page_Bytes;

   --  A q35/UEFI-like machine: 2 GiB low RAM, 2 GiB high RAM, the usual
   --  reserved islands, ECAM, firmware framebuffer in the PCI hole, plus the
   --  kernel's own descriptions.
   Q : Map := Empty_Map;

   RAM_Low_Last  : constant Unsigned_64 := 16#7FFD_FFFF#;
   High_RAM_First : constant Unsigned_64 := 4 * GiB;
   High_RAM_Last  : constant Unsigned_64 := 6 * GiB - 1;
   Fragment_First : constant Unsigned_64 := 16#2_0000_0800#;  -- stray sub-page RAM
   Fragment_Last  : constant Unsigned_64 := 16#2_0000_17FF#;
   ECAM_First     : constant Unsigned_64 := 16#B000_0000#;
   ECAM_Last      : constant Unsigned_64 := 16#BFFF_FFFF#;
   FB_First       : constant Unsigned_64 := 16#FD00_0000#;
   FB_Last        : constant Unsigned_64 := 16#FDFF_FFFF#;
   IOAPIC         : constant Unsigned_64 := 16#FEC0_0000#;
   HPET           : constant Unsigned_64 := 16#FED0_0000#;
   MSI_First      : constant Unsigned_64 := 16#FEE0_0000#;
   MSI_Last       : constant Unsigned_64 := 16#FEEF_FFFF#;
   Module_First   : constant Unsigned_64 := 16#0300_0000#;
   Module_Last    : constant Unsigned_64 := 16#03FF_FFFF#;
   NVS_First      : constant Unsigned_64 := 16#7FFE_0000#;
   NVS_Last       : constant Unsigned_64 := 16#7FFE_FFFF#;
   Tables_First   : constant Unsigned_64 := 16#7FFF_0000#;
   Tables_Last    : constant Unsigned_64 := 16#7FFF_7FFF#;
   Bad_First      : constant Unsigned_64 := 16#1_4000_0000#;  -- inside high RAM
   Bad_Last       : constant Unsigned_64 := 16#1_4000_0FFF#;

   procedure Build_Q is
   begin
      Add (Q, 0, 16#9_FBFF#, Usable_RAM);
      Add (Q, 16#9_FC00#, 16#9_FFFF#, Firmware_Reserved);
      Add (Q, 16#F_0000#, 16#F_FFFF#, Firmware_Reserved);
      Add (Q, 16#10_0000#, RAM_Low_Last, Usable_RAM);
      Add (Q, NVS_First, NVS_Last, ACPI_NVS);
      Add (Q, Tables_First, Tables_Last, ACPI_Tables);
      Add (Q, 16#7FFF_8000#, 16#7FFF_FFFF#, Firmware_Reserved);
      Add (Q, ECAM_First, ECAM_Last, Firmware_Reserved);
      Add (Q, 16#FED1_C000#, 16#FED1_FFFF#, Firmware_Reserved);
      Add (Q, 16#FFFC_0000#, 16#FFFF_FFFF#, Firmware_Reserved);
      Add (Q, High_RAM_First, High_RAM_Last, Usable_RAM);
      Add (Q, Bad_First, Bad_Last, Defective_RAM);
      Add (Q, Fragment_First, Fragment_Last, Usable_RAM);
      --  Kernel descriptions.
      Add (Q, 0, Low_Memory_Limit - 1, Low_Memory);
      Add (Q, 16#10_0000#, 16#01FF_FFFF#, Kernel_Image);
      Add (Q, Module_First, Module_Last, Boot_Module);
      Add (Q, FB_First, FB_Last, Firmware_Framebuffer);
      Add (Q, MSI_First, MSI_Last, Kernel_MMIO);
      Add (Q, IOAPIC, IOAPIC + Page - 1, Kernel_MMIO);
      Add (Q, HPET, HPET + Page - 1, Kernel_MMIO);
      --  Firmware that reports bytes beyond the physical limit.
      Add (Q, Address_Limit - Page, Unsigned_64'Last, Firmware_Reserved);
      Seal (Q);
   end Build_Q;

   function Dev (First, Last : Unsigned_64) return Boolean is
     (First <= Last and then Last < Address_Limit
      and then Device_Range (Q, First, Last));

   procedure Classification is
   begin
      Expect (Q.Complete and then Q.Sealed, "q map complete and sealed");
      Expect (Class_Of (Q, Page_Of (16#20_0000#)) = Kernel_Image, "kernel image class");
      Expect (Class_Of (Q, Page_Of (16#9_F000#)) = Low_Memory, "low memory dominates");
      Expect (Class_Of (Q, Page_Of (16#0400_0000#)) = Usable_RAM, "usable ram class");
      Expect (Class_Of (Q, Page_Of (Module_First)) = Boot_Module, "boot module class");
      Expect (Class_Of (Q, Page_Of (NVS_First)) = ACPI_NVS, "nvs class");
      Expect (Class_Of (Q, Page_Of (Tables_First)) = ACPI_Tables, "acpi tables class");
      Expect (Class_Of (Q, Page_Of (Bad_First)) = Usable_RAM, "overlap takes the minimum class");
      Expect (Class_Of (Q, Page_Of (Bad_First) + 1) = Usable_RAM, "defective page ends");
      Expect (Class_Of (Q, Page_Of (ECAM_First)) = Firmware_Reserved, "ecam reserved");
      Expect (Class_Of (Q, Page_Of (FB_First)) = Firmware_Framebuffer, "framebuffer class");
      Expect (Class_Of (Q, Page_Of (MSI_First)) = Kernel_MMIO, "interrupt window");
      Expect (Class_Of (Q, Page_Of (IOAPIC)) = Kernel_MMIO, "io apic");
      Expect (Class_Of (Q, Page_Of (HPET)) = Kernel_MMIO, "hpet");
      Expect (Class_Of (Q, Page_Of (16#C000_0000#)) = Unreported, "pci hole unreported");
      Expect (Class_Of (Q, Page_Of (High_RAM_Last + 1)) = Unreported, "above ram unreported");
      Expect (Class_Of (Q, Page_Of (Fragment_First)) = Usable_RAM, "sub-page fragment, first page");
      Expect (Class_Of (Q, Page_Of (Fragment_Last)) = Usable_RAM, "sub-page fragment, second page");
      Expect (Class_Of (Q, Page_Of (Fragment_Last) + 1) = Unreported, "page after the fragment");
      Expect (Class_Of (Q, Page_Of (Fragment_First) - 1) = Unreported, "page before the fragment");
      Expect (Class_Of (Q, Page_Of (Address_Limit - 1)) = Firmware_Reserved, "clipped top entry");
   end Classification;

   procedure Hostile_Ranges is
   begin
      --  Legitimate device ranges.
      Expect (Dev (16#FEB0_0000#, 16#FEB0_3FFF#), "bar in pci hole");
      Expect (Dev (ECAM_First, ECAM_Last), "ecam");
      Expect (Dev (FB_First, FB_Last), "firmware framebuffer");
      Expect (Dev (16#60_0000_0000#, 16#60_00FF_FFFF#), "64-bit bar above 4 GiB");
      Expect (Dev (16#80_0000_0000#, 16#80_0000_0FFF#), "bar far above ram");
      Expect (Dev (16#FFFC_0000#, 16#FFFF_FFFF#), "firmware rom (reserved)");
      --  Exactly at boundaries.
      Expect (Dev (16#8000_0000#, ECAM_First - 1), "hole exactly between reserved and ecam");
      Expect (Dev (16#7FFF_8000#, 16#7FFF_FFFF#), "reserved page exactly after acpi tables");
      Expect (not Dev (16#7FFF_7000#, 16#7FFF_8FFF#), "one page into acpi tables");
      Expect (Dev (High_RAM_Last + 1, High_RAM_Last + Page), "first page after high ram");
      Expect (not Dev (High_RAM_Last + 1 - Page, High_RAM_Last + Page), "one page into high ram (below)");
      Expect (not Dev (16#FFFF_F000#, High_RAM_First + Page - 1), "one page into high ram (above)");
      Expect (Dev (16#FFFF_F000#, High_RAM_First - 1), "last page before high ram");
      Expect (not Dev (RAM_Low_Last + 1 - Page, RAM_Low_Last + 1 + Page - 1), "one page into low ram");
      Expect (Dev (Address_Limit - Page, Address_Limit - 1), "last page below the physical limit");
      --  Overlapping RAM by one byte / sub-page RAM sharing a page.
      Expect (not Dev (16#FEB0_0000#, 16#FEDF_FFFF#), "bar run into hpet");
      Expect (not Dev (Fragment_First - 16#800#, Fragment_First - 1),
              "byte-disjoint but page-shared with ram");
      Expect (not Dev (MSI_First - Page, MSI_First), "one byte into interrupt window");
      Expect (not Dev (IOAPIC, IOAPIC), "io apic register");
      Expect (not Dev (HPET + Page - 1, HPET + Page), "last hpet byte");
      Expect (Dev (HPET + Page, HPET + 2 * Page - 1), "page after hpet");
      --  Memory the kernel manages.
      Expect (not Dev (16#20_0000#, 16#20_0FFF#), "kernel image");
      Expect (not Dev (0, Page - 1), "page zero");
      Expect (not Dev (16#A_0000#, 16#B_FFFF#), "legacy vga window (low memory)");
      Expect (not Dev (Module_First, Module_First + Page - 1), "boot module");
      Expect (not Dev (16#0400_0000#, 16#0400_0FFF#), "usable ram");
      Expect (not Dev (NVS_First, NVS_First + Page - 1), "acpi nvs");
      Expect (not Dev (Tables_First, Tables_First + Page - 1), "acpi tables");
      Expect (not Dev (Bad_First, Bad_Last), "defective ram");
      --  Huge, wrapping, empty and out of range.
      Expect (not Dev (0, Address_Limit - 1), "all of physical memory");
      Expect (not Dev (ECAM_First, 16#8_0000_0000#), "ecam to 32 GiB");
      Expect (not Device_Bytes (Q, 16#C000_0000#, 0), "zero size");
      Expect (not Device_Bytes (Q, Unsigned_64'Last - 4095, 8192), "wrapping");
      Expect (not Device_Bytes (Q, Unsigned_64'Last, 1), "last 64-bit byte");
      Expect (not Device_Bytes (Q, Address_Limit, Page), "at the physical limit");
      Expect (not Device_Bytes (Q, Address_Limit - Page, 2 * Page), "across the physical limit");
      Expect (Device_Bytes (Q, Address_Limit - Page, Page), "ending exactly at the limit");
      Expect (not Device_Bytes (Q, 16#80_0000_0000#, Unsigned_64'Last), "huge size");
      Expect (Device_Bytes (Q, 16#C000_0000#, 1), "one byte in the hole");
   end Hostile_Ranges;

   procedure Map_States is
      M : Map := Empty_Map;
   begin
      Expect (not Device_Range (M, 16#C000_0000#, 16#C000_0FFF#), "unsealed map authorizes nothing");
      Seal (M);
      Expect (Device_Range (M, 16#C000_0000#, 16#C000_0FFF#), "sealed empty map: bus space");
      M := Empty_Map;
      for I in 1 .. Max_Spans loop
         Add (M, 0, 0, Usable_RAM);
      end loop;
      Expect (M.Complete and then M.Count = Max_Spans, "full map still complete");
      Add (M, 16#C000_0000#, 16#C000_0FFF#, Usable_RAM);
      Seal (M);
      Expect (not M.Complete, "overflow marks incomplete");
      Expect (not Device_Range (M, 16#D000_0000#, 16#D000_0FFF#), "incomplete map authorizes nothing");
      M := Empty_Map;
      Invalidate (M);
      Seal (M);
      Expect (not Device_Range (M, 16#D000_0000#, 16#D000_0FFF#), "invalidated map authorizes nothing");
      M := Empty_Map;
      Add (M, 5, 4, Usable_RAM);
      Add (M, Address_Limit, Unsigned_64'Last, Usable_RAM);
      Expect (M.Count = 0, "empty and out-of-range descriptions ignored");
   end Map_States;

   procedure Runs is
      R : Run_Array;
      N : Root_Count;
      Found_Hole, Found_High : Boolean := False;
   begin
      Largest_Device_Runs (Q, R, N);
      Expect (N > 0, "device runs exist");
      for I in 1 .. N loop
         Expect (Device_Range (Q, R (I).First, R (I).Last), "run is device range");
         Expect (R (I).First mod Page = 0 and then R (I).Last mod Page = Page - 1, "run page aligned");
         if I > 1 then
            Expect (R (I - 1).Last - R (I - 1).First >= R (I).Last - R (I).First, "largest first");
         end if;
         --  Maximal: the pages just outside are not device pages.
         if R (I).First > 0 then
            Expect (not Device_Range (Q, R (I).First - Page, R (I).First - 1), "run maximal below");
         end if;
         if R (I).Last < Address_Limit - 1 then
            Expect (not Device_Range (Q, R (I).Last + 1, R (I).Last + Page), "run maximal above");
         end if;
         if R (I).First <= ECAM_First and then ECAM_Last <= R (I).Last then
            Found_Hole := True;
         end if;
         if R (I).First <= 16#60_0000_0000# and then Address_Limit - 1 = R (I).Last then
            Found_High := True;
         end if;
      end loop;
      Expect (Found_Hole, "pci hole with ecam is a root");
      Expect (Found_High, "everything above ram (64-bit bars) is a root");
   end Runs;

   procedure Authorization is
      --  The one capability a mapping call names (CAP-002: by handle).
      T : Capability := NULL_CAPABILITY;
      procedure Set (Base, Bytes : Unsigned_64; Rights : CapabilityRights) is
      begin
         T := (capType => CAP_DEVICE_MEM, rights => Rights,
                   authorityTag => NO_AUTHORITY_TAG,
                   object => (ref => Base, param => Bytes), gen => INITIAL_GENERATION);
      end Set;
   begin
      Set (16#FEB0_0000#, 16#4000#, Capabilities.READ_WRITE);
      Expect (Authorize (T, Q, 16#FEB0_0000#, 16#4000#, Read_Write), "bar rw");
      Expect (Authorize (T, Q, 16#FEB0_1000#, Page, Read_Only), "bar subrange ro");
      Expect (not Authorize (T, Q, 16#FEB0_0000#, 16#5000#, Read_Write), "beyond cap");
      Expect (not Authorize (T, Q, 16#FEAF_F000#, Page, Read_Only), "page below cap");
      Expect (not Authorize (T, Q, 16#FEAF_FFFF#, 2, Read_Only), "byte below cap");
      Set (16#FEB0_0000#, 16#4000#, Capabilities.READ_ONLY);
      Expect (not Authorize (T, Q, 16#FEB0_0000#, Page, Read_Write), "ro cap, rw request");
      Expect (Authorize (T, Q, 16#FEB0_0000#, Page, Read_Only), "ro cap, ro request");
      --  The old devmgr root (0 .. 4 GiB) no longer reaches RAM.
      Set (0, 4 * GiB, Capabilities.READ_WRITE);
      Expect (not Authorize (T, Q, 16#0400_0000#, Page, Read_Write), "old root: ram refused");
      Expect (not Authorize (T, Q, 16#20_0000#, Page, Read_Only), "old root: kernel refused");
      Expect (not Authorize (T, Q, MSI_First, Page, Read_Write), "old root: lapic refused");
      Expect (Authorize (T, Q, FB_First, FB_Last - FB_First + 1, Read_Write), "old root: framebuffer ok");
      --  Old DMA "permission" capabilities at physical 0 authorize nothing.
      Set (0, 1 * MiB, Capabilities.READ_WRITE);
      Expect (not Authorize (T, Q, 0, Page, Read_Write), "dma token at 0 maps nothing");
      Set (16#60_0000_0000#, 16 * MiB, Capabilities.READ_WRITE);
      Expect (Authorize (T, Q, 16#60_0080_0000#, 2 * MiB, Read_Only), "ggtt above 4 GiB");
      T := NULL_CAPABILITY;
      Expect (not Authorize (T, Q, 16#FEB0_0000#, Page, Read_Only), "no capability");
      --  A capability of another kind authorizes nothing.
      T := (capType => CAP_IOPORT, rights => Capabilities.READ_WRITE,
            authorityTag => NO_AUTHORITY_TAG,
            object => (ref => 16#FEB0_0000#, param => 16#4000#), gen => INITIAL_GENERATION);
      Expect (not Authorize (T, Q, 16#FEB0_0000#, Page, Read_Only), "wrong kind");
      --  Mint bound.
      Expect (Mint_Admissible (Q, FB_First, FB_Last - FB_First + 1), "mint framebuffer");
      Expect (Mint_Admissible (Q, 16#60_0000_0000#, 16 * MiB), "mint high bar");
      Expect (Mint_Admissible (Q, 16#FEB0_0000# + 16#3004#, 16#10#), "mint unaligned notify subrange");
      Expect (not Mint_Admissible (Q, 0, 16#1000_0000#), "old framebuffer token");
      Expect (not Mint_Admissible (Q, 0, 1 * MiB), "old dma token");
      Expect (not Mint_Admissible (Q, 16#FFFF_F000#, 2 * Page), "mint one page into ram");
      Expect (not Mint_Admissible (Q, 16#C000_0000#, 0), "mint zero size");
      Expect (not Mint_Admissible (Q, Unsigned_64'Last - 4095, 8192), "mint wrapping");
      Expect (not Mint_Admissible (Q, IOAPIC, Page), "mint io apic");
   end Authorization;

   procedure Attenuation is
      P : constant Capability :=
        (capType => CAP_DEVICE_MEM, rights => Capabilities.READ_WRITE,
         authorityTag => 7, object => (ref => 16#FEB0_0000#, param => 16#4000#),
         gen => INITIAL_GENERATION);
      C : Capability;
   begin
      --  The copy Capability_Install would derive (tests/capability-install).
      C := (P with delta rights => P.rights and Capabilities.READ_ONLY,
                         object => (16#FEB0_1000#, Page));
      Checks := Checks + 1; pragma Assert (isAttenuationOf (C, P), "derived subrange attenuates");
      Expect (C.rights = Capabilities.READ_ONLY and then C.object = (16#FEB0_1000#, Page), "derived fields");
      Checks := Checks + 1; pragma Assert (isAttenuationOf (P, P), "identity attenuates");
      C := P;
      C.object := (16#FEB0_0000#, 16#4001#);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "one byte larger");
      C.object := (16#FEAF_F000#, Page);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "below parent");
      C.object := (16#FEB0_3000#, 2 * Page);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "straddles parent end");
      C.object := (16#FEB0_1000#, 0);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "empty subrange");
      C.object := (Unsigned_64'Last - 4095, 8192);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "wrapping child");
      C.object := P.object;
      C.rights := ALL_RIGHTS;
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "rights superset");
      C := P;
      C.authorityTag := 8;
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "tag change");
      C := P;
      C.gen := INITIAL_GENERATION + 1;
      Checks := Checks + 1; pragma Assert (not isAttenuationOf (C, P), "generation change");
      --  Non-range objects still require the identical object.
      C := (capType => CAP_IOPORT, rights => Capabilities.READ_ONLY, authorityTag => 0,
            object => (16#CF8#, 4), gen => 1);
      Checks := Checks + 1; pragma Assert (not isAttenuationOf
                (C, (C with delta object => (16#CF8#, 8))), "ioport range does not narrow");
   end Attenuation;

   --  Independent oracle: page classes computed by brute force on a small
   --  address space, compared with Class_Of, Device_Range and the runs.
   procedure Oracle is
      Pages : constant := 48;
      Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
      function Next return Unsigned_64 is
      begin
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 7);
         Seed := Seed xor Shift_Left (Seed, 17);
         return Seed;
      end Next;
      type Classes is array (Unsigned_64 range 0 .. Pages - 1) of Range_Class;
   begin
      for Round in 1 .. 4000 loop
         declare
            M : Map := Empty_Map;
            C : Classes := [others => Unreported];
            Spans : constant Natural := Natural (Next mod 7);
            First, Last : Unsigned_64;
            K : Range_Class;
            R : Run_Array;
            N : Root_Count;
            Expected_Runs : Natural := 0;
         begin
            for S in 1 .. Spans loop
               First := Next mod (Pages * Page);
               Last := Unsigned_64'Min (First + Next mod (6 * Page), Pages * Page - 1);
               K := Range_Class'Val (Natural (Next mod (Range_Class'Pos (Range_Class'Last) + 1)));
               Add (M, First, Last, K);
               for P in First / Page .. Unsigned_64'Min (Last / Page, Pages - 1) loop
                  if K < C (P) then
                     C (P) := K;
                  end if;
               end loop;
            end loop;
            Seal (M);
            for P in C'Range loop
               Expect (Class_Of (M, P) = C (P), "oracle class");
            end loop;
            for Q1 in 0 .. Unsigned_64 (Pages - 1) loop
               for Q2 in Q1 .. Unsigned_64 (Pages - 1) loop
                  declare
                     Want : Boolean := True;
                     O1 : Unsigned_64 := Next mod Page;
                     O2 : Unsigned_64 := Next mod Page;
                     Swap : constant Unsigned_64 := O1;
                  begin
                     if Q1 = Q2 and then O1 > O2 then
                        O1 := O2;
                        O2 := Swap;
                     end if;
                     for P in Q1 .. Q2 loop
                        Want := Want and then Device_Mappable (C (P));
                     end loop;
                     Expect (Device_Range (M, Q1 * Page + O1, Q2 * Page + O2) = Want,
                             "oracle range");
                  end;
               end loop;
            end loop;
            Largest_Device_Runs (M, R, N);
            for P in C'Range loop
               if Device_Mappable (C (P)) and then (P = 0 or else not Device_Mappable (C (P - 1))) then
                  Expected_Runs := Expected_Runs + 1;
               end if;
            end loop;
            --  Plus the run that continues past the generated space (or
            --  starts there).
            if not Device_Mappable (C (Pages - 1)) then
               Expected_Runs := Expected_Runs + 1;
            end if;
            Expect (N = Natural'Min (Expected_Runs, Max_Roots), "oracle run count");
            for I in 1 .. N loop
               Expect (Device_Range (M, R (I).First, R (I).Last), "oracle run admissible");
            end loop;
         end;
      end loop;
   end Oracle;


   --  Denial explanations (the kernel's records) and the reader's ring
   --  arithmetic.
   procedure Explanations is
      package Pub renames Device_Memory_Publication;
      use type Pub.Reason;
      T : constant Capability := NULL_CAPABILITY;
      E : Device_Memory_Authority.Explanation;
      U : constant Map := Empty_Map;
   begin
      E := Explain (T, Q, 16#FEB0_0000#, Page, Read_Write);
      Expect (E.Why = Pub.No_Covering_Capability, "device page, no capability");
      E := Explain (T, Q, 16#0400_0000#, 2 * Page, Read_Only);
      Expect (E.Why = Pub.Blocked_Page and then E.Page = 16#0400_0000#
              and then E.Class = Usable_RAM, "ram page explained");
      E := Explain (T, Q, 16#FFFF_F000#, 2 * Page, Read_Write);
      Expect (E.Why = Pub.Blocked_Page and then E.Page = High_RAM_First
              and then E.Class = Usable_RAM, "first offending page is the second");
      E := Explain (T, Q, 16#FEBF_F000#, 3 * Page, Read_Write);
      Expect (E.Why = Pub.Blocked_Page and then E.Page = IOAPIC
              and then E.Class = Kernel_MMIO, "bar running into the io apic");
      E := Explain (T, Q, Fragment_First - 16#800#, 16#800#, Read_Write);
      Expect (E.Why = Pub.Blocked_Page and then E.Page = Fragment_First - 16#800#,
              "page-shared fragment explained at its page");
      E := Explain (T, Q, 16#C000_0000#, 0, Read_Write);
      Expect (E.Why = Pub.Invalid_Range, "zero size explained");
      E := Explain (T, Q, Unsigned_64'Last - 4095, 8192, Read_Write);
      Expect (E.Why = Pub.Invalid_Range, "wrapping explained");
      E := Explain (T, U, 16#C000_0000#, Page, Read_Write);
      Expect (E.Why = Pub.Classification_Unavailable, "unsealed map explained");
      E := Explain_Mint (Q, HPET, Page);
      Expect (E.Why = Pub.Blocked_Page and then E.Class = Kernel_MMIO, "mint of hpet");
      E := Explain_Mint (Q, 0, 16#1000_0000#);
      Expect (E.Why = Pub.Blocked_Page and then E.Page = 0 and then E.Class = Low_Memory,
              "old framebuffer token explained");
      --  Reader arithmetic.
      Expect (Pub.First_Available (0, 1) = 1, "first record");
      Expect (Pub.First_Available (3, 5) = 4, "next records");
      Expect (Pub.First_Available (0, Pub.Denial_Capacity) = 1, "full ring, nothing lost");
      Expect (Pub.First_Available (0, Pub.Denial_Capacity + 1) = 2, "one lost");
      Expect (Pub.First_Available (5, 100) = 100 - Pub.Denial_Capacity + 1, "many lost");
      Expect (Pub.First_Available (Unsigned_64'Last - 1, Unsigned_64'Last) = Unsigned_64'Last,
              "at the top");
      Expect (Pub.Slot_Of (1) = 0 and then Pub.Slot_Of (Pub.Denial_Capacity + 1) = 0
              and then Pub.Slot_Of (Pub.Denial_Capacity) = Pub.Denial_Capacity - 1, "slots");
      Expect (Pub.Class_Name (Unsigned_64 (Range_Class'Pos (Usable_RAM))) = "RAM", "class names");
      --  Process-kill ring (same protocol, its own capacity).
      Expect (Pub.First_Kill_Available (0, 1) = 1, "first kill");
      Expect (Pub.First_Kill_Available (0, Pub.Kill_Capacity) = 1, "full kill ring");
      Expect (Pub.First_Kill_Available (0, Pub.Kill_Capacity + 1) = 2, "one kill lost");
      Expect (Pub.First_Kill_Available (Unsigned_64'Last - 1, Unsigned_64'Last) = Unsigned_64'Last,
              "kills at the top");
      Expect (Pub.Kill_Slot_Of (1) = 0 and then Pub.Kill_Slot_Of (Pub.Kill_Capacity + 1) = 0,
              "kill slots");
      Expect (Pub.Kill_Kind_Name (Pub.Kill_Kind'Pos (Pub.Not_Present)) = "not-present"
              and then Pub.Kill_Kind_Name (99) = "kind?", "kill kind names");
   end Explanations;

begin
   Build_Q;
   Classification;
   Hostile_Ranges;
   Map_States;
   Runs;
   Authorization;
   Attenuation;
   Oracle;
   Explanations;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PASS: device-memory authority" & Natural'Image (Checks) & " checks");
   else
      Ada.Text_IO.Put_Line ("FAILED:" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Authority_Tests;
