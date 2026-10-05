------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Child_Table with SPARK_Mode is

   procedure Started (T : in out Table; Process : Process_Number;
                      Generation : Unsigned_64) is
   begin
      if T.Live_Count < Capacity then
         T.Live_Count := T.Live_Count + 1;
         T.Live (T.Live_Count) := (Process, Generation);
      end if;
   end Started;

   procedure Exited (T : in out Table; Report : CuBit.Child_Exits.Report) is
      use CuBit.Child_Exits;
      Status : Wait_Status;
   begin
      if Report.Process not in Process_Number
        or else T.Ended_Count = Capacity
      then
         return;
      end if;
      Status := (case Report.Kind is
                   when Exited  =>
                     Wait_Status (Report.Code * 2 ** Status_Code_Shift),
                   when Stopped => Killed_Status);
      for K in 1 .. T.Live_Count loop
         if T.Live (K).Process = Report.Process
           and then T.Live (K).Generation = Report.Generation
         then
            --  The last live child takes its place.
            T.Live (K) := T.Live (T.Live_Count);
            T.Live_Count := T.Live_Count - 1;
            T.Ended_Count := T.Ended_Count + 1;
            T.Ended (T.Ended_Count) := (Report.Process, Status);
            return;
         end if;
         pragma Loop_Invariant (T = T'Loop_Entry);
      end loop;
   end Exited;

   procedure Take (T : in out Table; Wanted : Interfaces.C.int;
                   Found : out Interfaces.C.int; Status : out Wait_Status)
   is
   begin
      Found := 0;
      Status := 0;
      for K in 1 .. T.Ended_Count loop
         if Wanted <= 0 or else T.Ended (K).Process = Unsigned_64 (Wanted) then
            --  A specific PID found is that PID.
            Found := (if Wanted > 0 then Wanted
                      else Interfaces.C.int (T.Ended (K).Process));
            Status := T.Ended (K).Status;
            --  Oldest first: the later ones move up.
            for J in K .. T.Ended_Count - 1 loop
               T.Ended (J) := T.Ended (J + 1);
               pragma Loop_Invariant (T.Live = T.Live'Loop_Entry
                                      and then T.Live_Count = T.Live_Count'Loop_Entry
                                      and then T.Ended_Count = T.Ended_Count'Loop_Entry);
            end loop;
            T.Ended_Count := T.Ended_Count - 1;
            return;
         end if;
         pragma Loop_Invariant (T = T'Loop_Entry and then Found = 0);
      end loop;
   end Take;

end CuBit.Child_Table;
