pragma Ada_2022;
package body ACPI_Delay_Waits with SPARK_Mode is
   use type AML_Delays.Delay_Kind;
   function Duration (Item : AML_Delays.Request) return Tick is
     (case Item.Kind is
         when AML_Delays.Sleep_Delay => Tick (Item.Milliseconds) * Microseconds_Per_Millisecond,
         when AML_Delays.Stall_Delay => Tick (Item.Microseconds));
   procedure Run (Item : AML_Delays.Request; Deadline : Tick;
      Budget : Observation_Budget; Result : out Report) is
      Requested : constant Tick := Duration (Item);
      Now : Tick := 0;
      Accepted, Sampled : Boolean;
      Need_Zero_Sleep : Boolean := Item.Kind = AML_Delays.Sleep_Delay and Requested = 0;
      procedure Sample (Success : out Boolean)
        with Pre => Result.Samples <= Budget,
          Post => Result.Samples <= Budget
            and then Result.Began = Result.Began'Old
            and then Result.Target = Result.Target'Old
            and then Result.Sleeps = Result.Sleeps'Old
            and then Result.Samples =
              (if Result.Samples'Old = Budget then Result.Samples'Old else Result.Samples'Old + 1)
            and then (if Success then Result.Last = Now and then Now < Deadline)
            and then (if not Success then Result.Code /= Completed)
      is
         Available : Boolean;
      begin
         Success := False;
         if Result.Samples = Budget then Result.Code := Observation_Limit; return; end if;
         Result.Samples := Result.Samples + 1;
         Read_Raw (Now, Available);
         if not Available then Result.Code := Clock_Unavailable; return; end if;
         if Result.Samples > 1 and then Now < Result.Last then
            Result.Code := Clock_Regressed; return;
         end if;
         Result.Last := Now;
         if Now >= Deadline then Result.Code := Deadline_Expired; return; end if;
         Success := True;
      end Sample;
   begin
      Result := (others => <>);
      Sample (Sampled);
      if not Sampled then return; end if;
      Result.Began := Now;
      if Requested > Tick'Last - Now then Result.Code := Deadline_Overflow; return; end if;
      Result.Target := Now + Requested;
      if Result.Target >= Deadline then Result.Code := Delay_Exceeds_Deadline; return; end if;
      loop
         pragma Loop_Invariant (Result.Samples in 1 .. Budget);
         pragma Loop_Invariant (Result.Sleeps < Result.Samples);
         pragma Loop_Invariant (Result.Last = Now and then Now < Deadline);
         pragma Loop_Variant (Decreases => Budget - Result.Samples);
         if Now >= Result.Target and not Need_Zero_Sleep then
            Result.Code := Completed; return;
         end if;
         if Result.Samples = Budget then Result.Code := Observation_Limit; return; end if;
         if Item.Kind = AML_Delays.Sleep_Delay then
            declare
               Until_Time : constant Tick := Now + Tick'Min
                 (Result.Target - Now, Tick (Maximum_Sleep_Slice));
            begin
               Result.Sleeps := Result.Sleeps + 1;
               Sleep_Raw (Until_Time, Accepted);
            end;
            if not Accepted then Result.Code := Sleep_Refused; return; end if;
            Need_Zero_Sleep := False;
         end if;
         Sample (Sampled);
      if not Sampled then return; end if;
      end loop;
   end Run;
end ACPI_Delay_Waits;
