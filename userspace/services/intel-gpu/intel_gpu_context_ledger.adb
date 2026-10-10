package body Intel_GPU_Context_Ledger with SPARK_Mode is
   use type Intel_GPU_Timeline.Observation;

   -- Owed values are fewer than the capacity apart, so their slots differ.
   procedure Lemma_Distinct_Slots (A, B : Value)
     with Ghost, Global => null,
          Pre => A < B and then B - A < Max_In_Flight,
          Post => Slot_Of (A) /= Slot_Of (B);
   procedure Lemma_Distinct_Slots (A, B : Value) is null;

   function Started (Done : Value; Now : Microseconds) return Ledger is
     ((Jobs => [others => (others => <>)], Owed => 0, Last => Done, Done => Done, Seen => Done,
       Call => 0, Current => Active, Cause => Q.None, Progress => Now));

   procedure Accept_Job
     (L : in out Ledger; Next : Value; Source : Origin; Item : Job; Now : Microseconds)
   is
      Old : constant Ledger := L with Ghost;
   begin
      if L.Done = L.Last then
         L.Progress := Now;
      end if;
      L.Jobs (Slot_Of (Next)) := Item;
      L.Owed := L.Owed + 1;
      L.Last := Next;
      if Source = From_Call then
         L.Call := Next;
      end if;
      pragma Assert (L.Last <= Last_Usable);
      pragma Assert (L.Done <= L.Last);
      pragma Assert (Value (L.Owed) <= L.Last);
      pragma Assert (L.Last - L.Done <= Value (L.Owed));
      pragma Assert (L.Seen >= L.Done and then L.Seen <= L.Last);
      pragma Assert (L.Call = 0 or else
         (L.Owed > 0 and then L.Call > L.Last - Value (L.Owed) and then L.Call <= L.Last));
      pragma Assert (for all V in First_Owed (L) .. Old.Last =>
                       Slot_Of (V) /= Slot_Of (Next));
      pragma Assert (for all V in First_Owed (L) .. Old.Last =>
                       Job_At (L, V) = Job_At (Old, V));
   end Accept_Job;

   procedure Observe
     (L : in out Ledger; Read_OK : Boolean; Observed : Value; Gate_Open : Boolean;
      Now, Hang_Budget : Microseconds; Result : out Observation)
   is
      Seen : Intel_GPU_Timeline.Observation;
   begin
      if L.Current in Failed_Health then
         Result := Not_Active;
         return;
      elsif L.Done = L.Last then
         Result := Idle;
         return;
      elsif not Read_OK then
         L.Current := Lost;
         L.Cause := Q.Timeline_Fault;
         Result := Failed;
         return;
      end if;
      Seen := Intel_GPU_Timeline.Classify (L.Done, L.Last, Observed);
      if not Intel_GPU_Timeline.Accepted (Seen) then
         L.Current := Lost;
         L.Cause := Q.Timeline_Fault;
         Result := Failed;
         return;
      elsif Now = Intel_GPU_Timeline.Clock_Unavailable or else Now < L.Progress then
         L.Current := Lost;
         L.Cause := Q.Device_Fault;
         Result := Failed;
         return;
      end if;
      if Seen /= Intel_GPU_Timeline.Unchanged and then Gate_Open then
         pragma Assert (Observed > L.Done and Observed <= L.Last);
         L.Done := Observed;
         L.Seen := Observed;
         L.Progress := Now;
         Result := Advanced;
         pragma Assert (Valid (L));
         return;
      elsif Observed > L.Seen then
         -- The GPU progressed; completion waits for the gate.
         L.Seen := Observed;
         L.Progress := Now;
         Result := Held;
         return;
      end if;
      -- No progress: the watchdogs.
      if Now - L.Progress >= Hang_Budget then
         L.Current := Hung;
         L.Cause := Q.Hang;
         Result := Failed;
      elsif L.Jobs (Slot_Of (L.Done + 1)).Deadline <= Now then
         L.Current := Hung;
         L.Cause := Q.Deadline;
         Result := Failed;
      else
         Result := Unchanged;
      end if;
   end Observe;

   procedure Pop
     (L : in out Ledger; Item : out Job; V : out Value; Source : out Origin;
      Status : out Pop_Status)
   is
   begin
      V := First_Owed (L);
      Item := L.Jobs (Slot_Of (V));
      Status := (if V <= L.Done then Done else Lost_Job);
      if L.Call /= 0 and then L.Call = V then
         Source := From_Call;
         L.Call := 0;
      else
         Source := From_Queue;
      end if;
      pragma Assert (if L.Current not in Failed_Health then V <= L.Done);
      pragma Assert (if L.Current not in Failed_Health then
                       L.Last - L.Done <= Value (L.Owed) - 1);
      pragma Assert (L.Call = 0 or else L.Call > V);
      L.Owed := L.Owed - 1;
      pragma Assert (L.Call = 0 or else
         (L.Owed > 0 and then L.Call > L.Last - Value (L.Owed) and then L.Call <= L.Last));
      pragma Assert (Value (L.Owed) <= L.Last);
      pragma Assert (if L.Current not in Failed_Health then L.Last - L.Done <= Value (L.Owed));
      pragma Assert (Valid (L));
      pragma Assert (V = L.Last - Value (L.Owed));
      pragma Assert (if L.Owed > 0 then First_Owed (L) = V + 1);
   end Pop;

   procedure Fault (L : in out Ledger; Cause : Reason) is
   begin
      if L.Current = Active then
         L.Current := Faulted;
         L.Cause := Cause;
      end if;
   end Fault;

   procedure Lose (L : in out Ledger; Cause : Reason) is
   begin
      if L.Current not in Failed_Health then
         L.Current := Lost;
         L.Cause := Cause;
      end if;
   end Lose;
end Intel_GPU_Context_Ledger;
