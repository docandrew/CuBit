pragma Ada_2022;

package body Devmgr_Port_Claims with SPARK_Mode is

   --  8259A interrupt controllers, 8254 timer, MC146818 CMOS clock, PCI
   --  configuration mechanism #1, 16550 COM1 (the kernel's console).
   PIC_Master       : constant := 16#20#;
   PIC_Slave        : constant := 16#A0#;
   PIC_Ports        : constant := 2;
   PIT              : constant := 16#40#;
   PIT_Ports        : constant := 4;
   CMOS             : constant := 16#70#;
   CMOS_Ports       : constant := 2;
   PCI_Config       : constant := 16#CF8#;
   PCI_Config_Ports : constant := 8;
   COM1             : constant := 16#3F8#;
   COM1_Ports       : constant := 8;

   procedure Initialize (Claims : in out Claim_Array; Count : out Natural) is
      function Seed (First, Ports : Unsigned_32; By : Holder) return Claim is
        ((First => First, Ends => First + Ports, By => By, Owner => No_Owner));
   begin
      Claims (1) := Seed (PIC_Master, PIC_Ports, Kernel);
      Claims (2) := Seed (PIT, PIT_Ports, Kernel);
      Claims (3) := Seed (CMOS, CMOS_Ports, Device_Manager);
      Claims (4) := Seed (PIC_Slave, PIC_Ports, Kernel);
      Claims (5) := Seed (COM1, COM1_Ports, Kernel);
      Claims (6) := Seed (PCI_Config, PCI_Config_Ports, Device_Manager);
      Count := Seed_Claims;
   end Initialize;

   procedure Take
     (Claims : in out Claim_Array; Count : in out Natural;
      First, Ports : Unsigned_64; By : Holder; Owner : Owner_Word;
      Result : out Take_Result)
   is
      Low, Ends : Unsigned_32;
      Same : Boolean := False;
      Wanted : Claim;
   begin
      Result := Refused;
      if not Valid_Range (First, Ports) then
         return;
      end if;
      Low := Unsigned_32 (First);
      Ends := Unsigned_32 (First + Ports);
      Wanted := (First => Low, Ends => Ends, By => By, Owner => Owner);
      for K in 1 .. Count loop
         if Overlaps (Claims (K), Low, Ends) then
            if (By = ACPI) /= (Claims (K).By = ACPI) then
               return;        --  another holder's ports
            elsif By = ACPI then
               if Claims (K).First /= Low or else Claims (K).Ends /= Ends then
                  return;     --  a different overlapping ACPI window
               end if;
            end if;
         end if;
         if Claims (K) = Wanted then
            Same := True;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              (if (By = ACPI) /= (Claims (J).By = ACPI)
               then not Overlaps (Claims (J), Low, Ends)));
         pragma Loop_Invariant (if Same then (for some J in 1 .. K => Claims (J) = Wanted));
      end loop;
      if Same then
         Result := Granted;
      elsif Count < Claims'Last then
         Count := Count + 1;
         Claims (Count) := Wanted;
         Result := Granted;
      else
         Result := Needs_Room;
      end if;
   end Take;

   procedure Release_Owner
     (Claims : in out Claim_Array; Count : in out Natural; Owner : Owner_Word)
   is
      Kept : Natural := 0;
   begin
      for Read in 1 .. Count loop
         if not Owned_By (Claims (Read), Owner) then
            Kept := Kept + 1;
            Claims (Kept) := Claims (Read);
         end if;
         pragma Loop_Invariant (Kept <= Read);
         pragma Loop_Invariant
           (for all K in 1 .. Count => (if K > Read then Claims (K) = Claims'Loop_Entry (K)));
         pragma Loop_Invariant (for all J in 1 .. Kept => not Owned_By (Claims (J), Owner));
         pragma Loop_Invariant
           (for all K in 1 .. Read =>
              (if not Owned_By (Claims'Loop_Entry (K), Owner)
               then (for some J in 1 .. Kept => Claims (J) = Claims'Loop_Entry (K))));
      end loop;
      Count := Kept;
   end Release_Owner;

end Devmgr_Port_Claims;
