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

   procedure Seed (Item : in out Ledger; First, Count : Unsigned_32; By : Holder)
   with Pre => Item.Count < Maximum_Claims and then Count <= Port_Space
               and then First <= Port_Space - Count,
        Post => Item.Count = Item.Count'Old + 1;

   procedure Seed (Item : in out Ledger; First, Count : Unsigned_32; By : Holder) is
   begin
      Item.Count := Item.Count + 1;
      Item.Claims (Item.Count) := (First => First, Ends => First + Count, By => By);
   end Seed;

   procedure Initialize (Item : out Ledger) is
   begin
      Item := (others => <>);
      Seed (Item, PIC_Master, PIC_Ports, Kernel);
      Seed (Item, PIT, PIT_Ports, Kernel);
      Seed (Item, CMOS, CMOS_Ports, Device_Manager);
      Seed (Item, PIC_Slave, PIC_Ports, Kernel);
      Seed (Item, COM1, COM1_Ports, Kernel);
      Seed (Item, PCI_Config, PCI_Config_Ports, Device_Manager);
   end Initialize;

   procedure Take
     (Item : in out Ledger; First, Count : Unsigned_64; By : Holder;
      Granted : out Boolean)
   is
      Low, Ends : Unsigned_32;
      Same : Boolean := False;
   begin
      Granted := False;
      if not Valid_Range (First, Count) then
         return;
      end if;
      Low := Unsigned_32 (First);
      Ends := Unsigned_32 (First + Count);
      for K in 1 .. Item.Count loop
         if Overlaps (Item.Claims (K), Low, Ends) then
            if (By = ACPI) /= (Item.Claims (K).By = ACPI) then
               return;        --  another holder's ports
            elsif By = ACPI then
               if Item.Claims (K).First /= Low or else Item.Claims (K).Ends /= Ends then
                  return;     --  a different overlapping ACPI window
               end if;
               Same := True;
            elsif Item.Claims (K).First = Low and then Item.Claims (K).Ends = Ends
              and then Item.Claims (K).By = By
            then
               Same := True;
            end if;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              (if (By = ACPI) /= (Item.Claims (J).By = ACPI)
               then not Overlaps (Item.Claims (J), Low, Ends)));
      end loop;
      if Same then
         Granted := True;
      elsif Item.Count < Maximum_Claims then
         Item.Count := Item.Count + 1;
         Item.Claims (Item.Count) := (First => Low, Ends => Ends, By => By);
         Granted := True;
      end if;
   end Take;

end Devmgr_Port_Claims;
