package body Scheduler_Timing with SPARK_Mode is
   procedure Advance
     (Phase : in out Tick_Phase; Millisecond, Quantum : out Boolean) is
   begin
      Millisecond := Phase.Clock = Second_Half;
      Quantum := Phase.Scheduling = Last_Third;
      if Millisecond then
         Phase.Clock := First_Half;
      else
         Phase.Clock := Millisecond_Phase'Succ (Phase.Clock);
      end if;
      if Quantum then
         Phase.Scheduling := First_Third;
      else
         Phase.Scheduling := Quantum_Phase'Succ (Phase.Scheduling);
      end if;
   end Advance;
end Scheduler_Timing;
