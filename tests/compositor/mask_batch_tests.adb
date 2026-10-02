with Ada.Text_IO;
with Compositor_Mask_Batch;
procedure Mask_Batch_Tests is
   package B renames Compositor_Mask_Batch;
   use type B.Packet, B.Command;
   P : B.Packet := (Width => 80, Height => 72, others => <>);
   V : B.Command := (Mask => 0, Description =>
     (0, 0, 32, 17, 1, 1, 0, 0, 0, 80, 72, 1), Tint => 16#FF12_3456#);
   OK : Boolean;
begin
   pragma Assert (B.Valid (P));
   for I in B.Index loop
      V.Mask := I - 1;
      B.Append (P, V, OK);
      pragma Assert (OK and P.Length = I and P.Items (I) = V and B.Valid (P));
      for J in 1 .. I loop pragma Assert (P.Items (J).Mask = J - 1); end loop;
   end loop;
   declare Before : constant B.Packet := P; begin
      B.Append (P, V, OK);
      pragma Assert (not OK and P = Before);
   end;
   for Failure in 1 .. 4 loop
      P.Length := 0;
      V.Description := (0, 0, 32, 17, 1, 1, 0, 0, 0, 80, 72, 1);
      case Failure is
         when 1 => V.Description.Over := 0;
         when 2 => V.Description.Clip_W := 81;
         when 3 => V.Description.Denominator := 0;
         when others => V.Description.Rotation := 4;
      end case;
      declare Before : constant B.Packet := P; begin
         B.Append (P, V, OK);
         pragma Assert (not OK and P = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("COMPOSITOR-MASK-BATCH: PASS capacity32, ordered append, overflow and invalid rejection");
end Mask_Batch_Tests;
