with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power; use Intel_GPU_PCI_Power;
with Intel_GPU_GGTT_Access; use Intel_GPU_GGTT_Access;
procedure GGTT_Access_Tests is
   Data, Changed : Configuration := [others => 0];
   Base : constant Unsigned_64 := 16#60_0000_0000#;
   Item : Grant_Plan;
   procedure Reject (Config : Configuration; BAR, Bytes : Unsigned_64) is
   begin
      Item := Plan_Write (Config, BAR, Bytes, True, True);
      pragma Assert (not Item.Valid and Item.Physical = 0 and Item.Bytes = 0);
   end Reject;
begin
   Data (0 .. 3) := [16#86#, 16#80#, 16#D2#, 16#46#];
   Data (4) := 6; Data (5) := 4; Data (6) := 16#10#;
   Data (16#10#) := 4; Data (16#14#) := 16#60#;
   Data (16#34#) := 16#80#; Data (16#80#) := 1; Data (16#82#) := 3;
   for Size_Code in 1 .. 3 loop
      Data (16#50#) := Unsigned_8 (Size_Code * 64);
      for Owner in Boolean loop
         for Disabled in Boolean loop
            Item := Plan_Write (Data, Base, 2 ** Size_Code * 1024 * 1024, Owner, Disabled);
            pragma Assert (Item.Valid = (Owner and Disabled));
            if Item.Valid then
               pragma Assert (Item.Physical = Base + 16#80_0000#);
            end if;
         end loop;
      end loop;
      Reject (Data, Base + 4096, 2 ** Size_Code * 1024 * 1024);
      Reject (Data, Base, 4096);
   end loop;
   Changed := Data; Changed (0) := 0; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (4) := 4; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (5) := 0; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (16#84#) := 3; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (16#50#) := 0; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (16#50# .. 16#51#) := [255, 255]; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (16#14#) := 16#61#; Reject (Changed, Base, 8_388_608);
   Changed := Data; Changed (16#81#) := 16#80#; Reject (Changed, Base, 8_388_608);
   Reject (Data, 0, 8_388_608);
   Reject (Data, Unsigned_64'Last, 8_388_608);
   -- The executor's historical completed state is not sufficient if current
   -- configuration has re-enabled a source. Check every MSI/MSI-X combination.
   Data (16#34#) := 16#40#;
   Data (16#40#) := 5; Data (16#41#) := 16#60#;
   Data (16#60#) := 16#11#; Data (16#61#) := 16#80#;
   for MSI in Boolean loop
      for MSIX in Boolean loop
         for Masked in Boolean loop
            Data (16#42#) := (if MSI then 1 else 0);
            Data (16#63#) := (if MSIX then 128 else 0) + (if Masked then 64 else 0);
            Item := Plan_Write (Data, Base, 8_388_608, True, True);
            pragma Assert (Item.Valid = (not MSI and not MSIX and Masked));
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("GGTT write admission PASS: owner/state, BAR/table stability, D0 and interrupt controls");
end GGTT_Access_Tests;
