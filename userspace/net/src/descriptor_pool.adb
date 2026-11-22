------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body Descriptor_Pool with SPARK_Mode is

   procedure Initialize (P : out Pool) is
   begin
      P := (In_Flight => [others => False], Free => [others => 0], Top => 0);
      for D in Id loop
         pragma Loop_Invariant (P.Top = D);
         pragma Loop_Invariant (for all E in Id => not P.In_Flight (E));
         pragma Loop_Invariant (for all K in 0 .. D - 1 => P.Free (K) = K);
         P.Free (D) := D;
         P.Top := D + 1;
      end loop;
   end Initialize;

   procedure Take (P : in out Pool; D : out Id; OK : out Boolean) is
   begin
      OK := P.Top > 0;
      if not OK then
         D := 0;
         return;
      end if;
      D := P.Free (P.Top - 1);
      P.Top := P.Top - 1;
      P.In_Flight (D) := True;
   end Take;

   procedure Give_Back (P : in out Pool; Raw : Unsigned_32; OK : out Boolean) is
   begin
      OK := Raw < Unsigned_32 (Count) and then P.In_Flight (Natural (Raw)) and then
            P.Top < Count;
      if OK then
         P.In_Flight (Natural (Raw)) := False;
         P.Free (P.Top) := Natural (Raw);
         P.Top := P.Top + 1;
      end if;
   end Give_Back;

end Descriptor_Pool;
