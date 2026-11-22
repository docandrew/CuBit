pragma Ada_2022;
package body ACPI_Backend_Mapping_Chunks with SPARK_Mode is
   use type B.Tick;
   function Observe (State : Context) return Statistics is (State.Stats);
   function Fits (Base, Pages, Limit : Word) return Boolean is
     (Pages > 0 and then Base mod Page_Bytes = 0 and then
      Base < Limit and then Pages <= (Limit - Base) / Page_Bytes);
   procedure Map_Exact
     (State : in out Context; Held : Authority; Physical, Virtual, Pages : Word;
      Read_Only : Boolean; Deadline : B.Tick; Result : out Outcome)
   is
      procedure Run
        with Pre => State.Stats.Working and then State.Stats.Stage = Preparing
          and then State.Stats.Attempts = 0 and then State.Stats.Confirmed_Pages = 0,
        Post => Result not in Reentered | Already_Attempted | Attempt_Limit
          and then State.Stats.Working and then State.Stats.Confirmed_Pages <= Pages
          and then ((State.Stats.Stage = Complete) = (Result = Mapped))
          and then (if Result = Mapped then State.Stats.Confirmed_Pages = Pages);
      procedure Run is
         procedure Check_Time (Status : out Outcome) is
            Now : B.Tick;
            Available : Boolean;
         begin
            Read_Clock (Now, Available);
            if not Available then Status := Clock_Unavailable; return; end if;
            if State.Seen_Clock and then Now < State.Last_Clock then
               Status := Clock_Reversed; return;
            end if;
            State.Seen_Clock := True; State.Last_Clock := Now;
            if Now >= Deadline then Status := Deadline_Expired; return; end if;
            Status := Mapped;
         end Check_Time;
         Done : Word := 0;
         Chunk : Word;
         Completed : Boolean;
      begin
         if Held = No_Authority then Result := Missing_Authority; return; end if;
         if Deadline = B.No_Finite_Deadline then Result := Invalid_Deadline; return; end if;
         if Pages > Word (Maximum_Pages) or else Virtual = 0 or else
           not Fits (Physical, Pages, Physical_Limit) or else
           not Fits (Virtual, Pages, Virtual_Limit)
         then Result := Invalid_Request; return; end if;
         for Attempt in 1 .. Maximum_Calls loop
            pragma Loop_Invariant (Done < Pages);
            pragma Loop_Invariant (State.Stats.Working and then State.Stats.Stage /= Complete);
            pragma Loop_Invariant (Done = Word (Attempt - 1) * Word (Maximum_Per_Call));
            pragma Loop_Invariant (State.Stats.Confirmed_Pages = Done);
            pragma Loop_Invariant (State.Stats.Attempts = Attempt - 1);
            Check_Time (Result); if Result /= Mapped then return; end if;
            Chunk := Word'Min (Pages - Done, Word (Maximum_Per_Call));
            pragma Assert (Chunk in 1 .. Word (Maximum_Per_Call));
            pragma Assert (Done <= Pages - Chunk);
            pragma Assert (Physical + Done * Page_Bytes >= Physical and then
              Physical + (Done + Chunk) * Page_Bytes <= Physical + Pages * Page_Bytes);
            pragma Assert (Virtual + Done * Page_Bytes >= Virtual and then
              Virtual + (Done + Chunk) * Page_Bytes <= Virtual + Pages * Page_Bytes);
            pragma Assert ((Physical + Done * Page_Bytes) mod Page_Bytes = 0
              and then (Virtual + Done * Page_Bytes) mod Page_Bytes = 0);
            State.Stats.Stage := Mapping;
            State.Stats.Attempts := Attempt;
            Map_Chunk (Held, Physical + Done * Page_Bytes,
              Virtual + Done * Page_Bytes, Chunk, Read_Only, Completed);
            if not Completed then Result := Mapping_Indeterminate; return; end if;
            Done := Done + Chunk;
            State.Stats.Confirmed_Pages := Done;
            if Done = Pages then
               Check_Time (Result); if Result /= Mapped then return; end if;
               State.Stats.Stage := Complete; Result := Mapped; return;
            end if;
         end loop;
         Result := Attempt_Limit;
      end Run;
   begin
      if State.Stats.Working then Result := Reentered; return; end if;
      if State.Stats.Stage /= Fresh then Result := Already_Attempted; return; end if;
      State.Stats := (Stage => Preparing, Working => True, others => <>);
      Run;
      if State.Stats.Stage /= Complete then State.Stats.Stage := Stopped; end if;
      State.Stats.Working := False;
   end Map_Exact;
end ACPI_Backend_Mapping_Chunks;
