------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Desktop_Auto_Scale with SPARK_Mode is
   function Choose (Width, Height : G.Physical_Extent) return D.Scale_Preset is
      Wanted : constant D.Scale_Preset := By_Size (Width, Height);
   begin
      for P in reverse D.Scale_Preset range D.Scale_Preset'Succ (D.Scale_100) .. Wanted loop
         if Fits (Width, Height, P) then
            return P;
         end if;
         pragma Loop_Invariant
           (for all Q in D.Scale_Preset =>
              (if Q >= P and then Q <= Wanted then not Fits (Width, Height, Q)));
      end loop;
      return D.Scale_100;
   end Choose;
end Desktop_Auto_Scale;
