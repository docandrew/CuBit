with Ada.Text_IO; with Compositor_Transfer_Delta;
procedure Transfer_Delta_Tests is
   package D renames Compositor_Transfer_Delta;
   use type D.Count; use type D.Sample;
   type Values is array (Positive range <>) of D.Count;
   Points : constant Values := (0, 1, 255, 262_144, D.Count'Last - 1, D.Count'Last);
   Checked : Natural := 0;
   Actual : D.Sample;
begin
   for PG of Points loop
      for PC of Points loop
         for CG of Points loop
            for CC of Points loop
               Actual := D.Prepare (PG, PC, CG, CC, False);
               if CG < PG or CC < PC then
                  pragma Assert (Actual = (D.Invalid_Counters, 0, 0));
               elsif CG = PG and CC = PC then
                  pragma Assert (Actual = (D.No_Work, 0, 0));
               else
                  pragma Assert (Actual = (D.Sample_Ready, CG - PG, CC - PC));
               end if;
               pragma Assert (D.Prepare (PG, PC, CG, CC, True) = (D.Invalid_Counters, 0, 0));
               Checked := Checked + 2;
            end loop;
         end loop;
      end loop;
   end loop;
   -- GPU submission precedes copies, and copied rows may span polls.
   pragma Assert (D.Prepare (0, 0, 4096, 0, False) = (D.Sample_Ready, 4096, 0));
   pragma Assert (D.Prepare (4096, 0, 4096, 1024, False) = (D.Sample_Ready, 0, 1024));
   pragma Assert (D.Prepare (4096, 1024, 4096, 4096, False) = (D.Sample_Ready, 0, 3072));
   pragma Assert (D.Prepare (4096, 4096, 4096, 4096, False) = (D.No_Work, 0, 0));
   -- A skipped sample preserves the previous snapshot and accumulates work.
   pragma Assert (D.Prepare (0, 0, 4096, 4096, False) = (D.Sample_Ready, 4096, 4096));
   Ada.Text_IO.Put_Line ("PASS" & Natural'Image (Checked) & " boundary cases and 5 submission/copy sampling cases");
end Transfer_Delta_Tests;
