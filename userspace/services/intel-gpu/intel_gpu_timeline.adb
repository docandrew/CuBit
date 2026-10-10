package body Intel_GPU_Timeline with SPARK_Mode is
   function Combine (High_First, Low, High_Second : Half) return Read_Result is
     (if High_First = High_Second then
        (Stable => True, Observed => Value (High_First) * 2 ** 32 + Value (Low))
      else (Stable => False, Observed => 0));

   function Classify (Completed, Published, Observed : Value) return Observation is
     (if Observed < Completed then Regressed
      elsif Observed = Completed then Unchanged
      elsif Observed < Published then Advanced
      elsif Observed = Published then Reached
      else Beyond_Published);

   procedure Start (Object : in out Waiter; Done, Target_Value : Value;
                    Now, Budget : Microseconds; Started : out Boolean) is
   begin
      Started := Can_Start (Object, Done, Target_Value, Now, Budget);
      if not Started then return; end if;
      Object := (Active => True, Done => Done, Goal => Target_Value,
                 Seen => Done, Limit => Now + Budget, Previous => Now);
   end Start;

   procedure Step (Object : in out Waiter; Read_OK : Boolean; Observed : Value;
                   Gate_Open : Boolean; Now : Microseconds; Result : out Outcome) is
      Seen : Observation;
   begin
      if not Waiting (Object) then Result := Not_Waiting; return; end if;
      -- Every path below ends the wait unless it returns Pending.
      if Now = Clock_Unavailable or else Now < Object.Previous then
         Object.Active := False; Result := Clock_Fault; return;
      end if;
      Object.Previous := Now;
      if not Read_OK then
         Object.Active := False; Result := Read_Fault; return;
      end if;
      Seen := Classify (Object.Done, Object.Goal, Observed);
      if not Accepted (Seen) then
         Object.Active := False; Result := Timeline_Fault; return;
      end if;
      Object.Seen := Observed;
      if Seen = Reached and then Gate_Open then
         Object.Done := Object.Goal; Object.Active := False;
         Result := Complete; return;
      end if;
      if Now >= Object.Limit then
         Object.Active := False; Result := Deadline_Expired; return;
      end if;
      Result := Pending;
   end Step;

   procedure Abandon (Object : in out Waiter) is
   begin
      Object.Active := False;
   end Abandon;
end Intel_GPU_Timeline;
