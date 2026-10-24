------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Link_Tracker with SPARK_Mode is

   procedure Observe
     (O      : in out Observation;
      Before : NL.Generation;
      Words  : NL.Report_Words;
      After  : NL.Generation;
      Result : out Step)
   is
      Old : constant Observation := O;
   begin
      Result := (others => <>);
      if not NL.Stable (Before, After) then
         Result.Found := Unstable;
         return;
      end if;
      if Before = O.Seen then
         return;
      end if;
      Result.Reports := NL.Reports_Between (Old.Seen, Before);
      declare
         D : constant NL.Decoded_Report := NL.Decode (Words);
      begin
         if D.Valid then
            O := (State => D.State, Seen => Before);
            Result.Found := Taken;
         else
            O := (State => Fault_State, Seen => Before);
            Result.Found := Refused;
         end if;
      end;
      Result.Change := Change_Of (Old.State, O.State, Result.Reports);
   end Observe;

   procedure Lemma_No_Hidden_Flap
     (Old, New_State : NL.Link_State; Reports : NL.Generation) is null;

end Link_Tracker;
