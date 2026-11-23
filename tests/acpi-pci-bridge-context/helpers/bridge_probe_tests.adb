pragma Ada_2022;
with Ada.Text_IO;
with AML_PCI_Bridge_Probe;
procedure Bridge_Probe_Tests is
   package P renames AML_PCI_Bridge_Probe;
   use type P.Byte, P.Stage;
   S : P.State;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Bus in P.Byte loop
      for H in P.Byte loop
         P.Start (S, Bus);
         Check (P.At_Stage (S) = P.Header and not P.Is_Bridge (S));
         Check (P.Offset (S) = 14 and P.Remaining (S) = 3);
         P.Apply_Byte (S, H);
         Check (P.Containing_Bus (S) = Bus and P.Downstream_Bus (S) = Bus);
         if H in 1 | 2 | 129 | 130 then
            Check (P.Is_Bridge (S) and P.At_Stage (S) = P.Primary);
            Check (P.Offset (S) = 24 and P.Remaining (S) = 2);
            P.Apply_Byte (S, 91);
            Check (P.Containing_Bus (S) = 91 and P.Downstream_Bus (S) = Bus);
            Check (P.Offset (S) = 25 and P.Remaining (S) = 1);
            P.Apply_Byte (S, 207);
            Check (P.Is_Bridge (S) and P.At_Stage (S) = P.Complete);
            Check (P.Containing_Bus (S) = 91 and P.Downstream_Bus (S) = 207);
         else
            Check (not P.Is_Bridge (S) and P.At_Stage (S) = P.Complete);
            Check (P.Containing_Bus (S) = Bus and P.Downstream_Bus (S) = Bus);
         end if;
         Check (P.Remaining (S) = 0);
      end loop;
   end loop;
   for Header in P.Byte range 1 .. 2 loop
      for Primary in P.Byte loop
         for Secondary in P.Byte loop
            P.Start (S, 42); P.Apply_Byte (S, Header);
            P.Apply_Byte (S, Primary); P.Apply_Byte (S, Secondary);
            Check (P.At_Stage (S) = P.Complete and P.Is_Bridge (S));
            Check (P.Containing_Bus (S) = Primary and P.Downstream_Bus (S) = Secondary);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Bridge probe checks" & Checks'Image);
end Bridge_Probe_Tests;
