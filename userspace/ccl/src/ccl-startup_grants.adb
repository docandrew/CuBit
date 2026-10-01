package body CCL.Startup_Grants with SPARK_Mode => On is
   procedure Decide
     (Policy : Launch_Entry; Plan : Section_Plan; Decisions : out Decision_Array)
   is
   begin
      --  Denied unless shown otherwise.
      Decisions := [others => Device_Not_Approved];
      for I in 1 .. Plan.Count loop
         case Plan.Entries (I).Kind is
            when Device_Resource =>
               if Device_Approved (Policy) then
                  Decisions (I) := Granted;
               end if;
            when Scheduling =>
               if not Policy.Approve_Scheduling then
                  Decisions (I) := Scheduling_Not_Approved;
               elsif Within_Ceiling (Policy, Plan.Entries (I)) then
                  Decisions (I) := Granted;
               else
                  Decisions (I) := Scheduling_Exceeds_Ceiling;
               end if;
         end case;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              (Decisions (J) = Granted) = Allowed (Policy, Plan.Entries (J)));
         pragma Loop_Invariant
           (for all J in I + 1 .. MAX_ENTRIES => Decisions (J) /= Granted);
      end loop;
   end Decide;
end CCL.Startup_Grants;
