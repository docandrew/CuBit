with Ada.Text_IO; use Ada.Text_IO;
with Boot_Panel;
with Boot_QR;
with Boot_QR_Capsule;
procedure Main is
   function Line (Value : String) return Boot_Panel.Line is
     (Boot_Panel.Fit (Value));
   Capsule : constant String := Boot_QR_Capsule.Build
     (Line ("Starting services / waiting for display owner"),
      Line ("CPU initialization complete"),
      Line ("procmgr: bootstrap 4/4 reading startup profile"),
      Line (""));
   Code : Boot_QR.Matrix;
   Dark : Natural := 0;
begin
   Boot_QR.Encode (Capsule, Code);
   pragma Assert (Capsule'Length <= Boot_QR.Maximum_Byte_Length);
   pragma Assert (Capsule (Capsule'First .. Capsule'First + 3) = "CB1;");
   pragma Assert (Code (0, 0));
   pragma Assert (Code (6, 6));
   pragma Assert (Code (0, 7) = False);
   pragma Assert (Code (Boot_QR.Dimension - 1, 0));
   pragma Assert (Code (0, Boot_QR.Dimension - 1));
   for Row in Boot_QR.Module_Index loop
      for Column in Boot_QR.Module_Index loop
         if Code (Row, Column) then Dark := Dark + 1; end if;
      end loop;
   end loop;
   pragma Assert (Dark in 250 .. 700);
   Put_Line ("BOOT QR PASS: " & Capsule);
end Main;
