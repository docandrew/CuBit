with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power; use Intel_GPU_PCI_Power;
with Intel_GPU_PCI_Interrupts; use Intel_GPU_PCI_Interrupts;
procedure PCI_IRQ_Disable_Tests is
   Data, Updated : Configuration;
   Plan : Disable_Plan;
   State : Snapshot;
   Changed : array (Data'Range) of Boolean;
   procedure Baseline is
   begin
      Data := [others => 0];
      Data (0) := 16#86#; Data (1) := 16#80#;
      Data (2) := 16#D2#; Data (3) := 16#46#;
      Data (4) := 7; Data (6) := 16#10#; Data (7) := 16#F9#;
      Data (16#34#) := 16#40#;
      Data (16#40#) := 5; Data (16#41#) := 16#60#;
      Data (16#60#) := 16#11#;
   end Baseline;
begin
   for Flags in Unsigned_32 range 0 .. 255 loop
      Baseline;
      Data (5) := (if (Flags and 1) /= 0 then 4 else 0);
      Data (16#42#) := Unsigned_8 (Flags and 16#FF#);
      Data (16#43#) := 1; -- per-vector masking, longest MSI layout
      Data (16#62#) := 16#5A#;
      Data (16#63#) := Unsigned_8 (Flags and 16#C7#);
      Plan := Plan_Disable (Data);
      pragma Assert (Valid (Plan) and Count (Plan) <= 3);
      Updated := Data; Changed := [others => False];
      for I in 1 .. Count (Plan) loop
         declare
            Address : constant Natural := Offset (Plan, I);
            Prior : constant Unsigned_16 := Unsigned_16 (Data (Address)) or
              Shift_Left (Unsigned_16 (Data (Address + 1)), 8);
            Expected : constant Unsigned_16 :=
              (case Address is when 4 => Prior or 16#400#,
               when 16#42# => Prior and not 1,
               when 16#62# => (Prior and not 16#8000#) or 16#4000#,
               when others => 0);
         begin
            pragma Assert (Address in 4 | 16#42# | 16#62#);
            pragma Assert (not Changed (Address) and not Changed (Address + 1));
            pragma Assert (Before (Plan, I) = Prior and After (Plan, I) = Expected);
            pragma Assert (Prior /= Expected);
            Updated (Address) := Unsigned_8 (Expected and 255);
            Updated (Address + 1) := Unsigned_8 (Shift_Right (Expected, 8));
            Changed (Address) := True; Changed (Address + 1) := True;
         end;
      end loop;
      for I in Data'Range loop
         if not Changed (I) then pragma Assert (Data (I) = Updated (I)); end if;
      end loop;
      pragma Assert (Updated (6 .. 7) = Data (6 .. 7)); -- W1C status untouched
      State := Decode (Updated);
      pragma Assert (State.Valid and State.INTx_Disabled and not State.MSI_Enabled
        and not State.MSIX_Enabled and State.MSIX_Masked);
      Plan := Plan_Disable (Updated);
      pragma Assert (Valid (Plan) and Count (Plan) = 0);
   end loop;
   for Bad in 0 .. 5 loop
      Baseline;
      case Bad is
         when 0 => Data (16#41#) := 16#40#; -- cycle
         when 1 => Data (16#41#) := 16#44#; -- overlap
         when 2 => Data (16#60#) := 5; -- duplicate MSI
         when 3 => Data (4 .. 5) := [others => 255];
         when 4 => Data (16#42# .. 16#43#) := [others => 255];
         when others => Data (16#62# .. 16#63#) := [others => 255];
      end case;
      pragma Assert (not Valid (Plan_Disable (Data)));
   end loop;
   Baseline; Data (6) := 0;
   Plan := Plan_Disable (Data);
   pragma Assert (Valid (Plan) and Count (Plan) = 1 and Offset (Plan, 1) = 4);
   Ada.Text_IO.Put_Line ("PCI IRQ disable plan PASS: 256 control combinations, exact word preservation, no-op replay, malformed rejection");
end PCI_IRQ_Disable_Tests;
