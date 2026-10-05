with Ada.Text_IO;
with Compositor_Layout_Restore;
procedure Layout_Restore_Tests is
   package R renames Compositor_Layout_Restore;
   package L renames R.L;
   use type L.Layout, L.G.UI_Scale;
   Fresh, Saved, Changed : L.Layout;
begin
   Fresh.Count := 2;
   Fresh.Items (1) := (1, (Width => 1200, Height => 900, others => <>));
   Fresh.Items (2) := (2, (Width => 1200, Height => 900, X => 1200, others => <>));
   for N in 4 .. 8 loop
      for Vertical in Boolean loop
         Saved := Fresh;
         Saved.Items (1).Geometry.Scale := (L.G.Scale_Component (N), 4);
         Saved.Items (2).Geometry.Scale := (L.G.Scale_Component (N), 4);
         Saved.Items (2).Geometry.X := L.G.Output_Origin (if Vertical then 0 else 4800 / N);
         Saved.Items (2).Geometry.Y := L.G.Output_Origin (if Vertical then 3600 / N else 0);
         -- Place the neighbour at the computed logical boundary; N=7 is fractional.
         if Vertical then Saved.Items (2).Geometry.Y := L.G.Output_Origin (L.G.Bounds (Saved.Items (1).Geometry).Bottom);
         else Saved.Items (2).Geometry.X := L.G.Output_Origin (L.G.Bounds (Saved.Items (1).Geometry).Right); end if;
         pragma Assert (R.Compatible (Saved, Fresh));
         pragma Assert (R.Choose (Saved, Fresh) = Saved);
         Changed := Fresh; Changed.Items (2).Geometry.Width := 1199;
         pragma Assert (R.Choose (Saved, Changed) = Changed);
         Changed := Fresh; Changed.Count := 1;
         pragma Assert (R.Choose (Saved, Changed) = Changed);
         Changed := Fresh; Changed.Items (1).Display := 3;
         pragma Assert (R.Choose (Saved, Changed) = Changed);
         Changed := Saved; Changed.Items (2).Geometry.X := 0; Changed.Items (2).Geometry.Y := 0;
         pragma Assert (R.Choose (Changed, Fresh) = Fresh);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("LAYOUT-RESTORE: PASS 10 scaled/arranged layouts preserved exactly; changed mode/count/identity and invalid overlap rejected");
end Layout_Restore_Tests;
