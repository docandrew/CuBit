pragma Ada_2022;
package body ACPI_Local_Field_Scope with SPARK_Mode is
   use type Interfaces.Unsigned_64, P.Lock_Outcome;
   function Observe (State : Context) return Statistics is (State.Info);
   function Captured_Window (State : Context) return Interfaces.Unsigned_64 is (State.Window_Deadline);
   function Pending (State : Context) return Boolean is
     (State.Info.Working or else State.Info.Cleanup_Required);
   function Ready (State : Context) return Boolean is
     (not State.Info.Working and then not State.Info.Faulted
      and then (State.Info.Held or else not State.Info.Cleanup_Required));
   procedure Fail (State : in out Context; Result : P.Lock_Outcome) is
   begin
      if not State.Info.Faulted then State.Info.First_Failure := Result; end if;
      State.Info.Faulted := True;
   end Fail;
   procedure Read_Window (State : Context; Deadline : out Interfaces.Unsigned_64;
      Available : out Boolean) is
   begin
      Inspect_IO_Window (Deadline,Available);
      Available := Available and then Deadline not in 0 | Interfaces.Unsigned_64'Last
        and then (State.Info.Depth = 0 or else Deadline = State.Window_Deadline);
      if not Available then Deadline := 0; end if;
   end Read_Window;
   procedure Begin_Field (State : in out Context; Requires_Lock, For_Write : Boolean;
      Accepted : out Boolean)
   is
      pragma Unreferenced (For_Write);
      Deadline : Interfaces.Unsigned_64;
      Owner_Deadline : Interfaces.Unsigned_64;
      Budget : P.Attempt_Budget;
      Available, Owns : Boolean := False;
      Result : P.Lock_Outcome;
   begin
      Accepted := False;
      if not Ready (State) then return; end if;
      if State.Info.Depth = Depth_Count'Last then Fail (State,P.Lease_Exhausted); return; end if;
      State.Info.Working := True;
      if not State.Info.Held and then not Admission_Open then
         Fail (State,P.Not_Ready); State.Info.Working := False; return;
      end if;
      Read_Window (State,Owner_Deadline,Available);
      if not Available then
         Fail (State,P.Invalid_Deadline); State.Info.Working := False; return;
      end if;
      if State.Info.Depth = 0 then State.Window_Deadline := Owner_Deadline; end if;
      if State.Info.Held then
         Check_Access (State.Item,Result);
         if Result /= P.Configured then Fail (State,Result); State.Info.Working := False; return; end if;
      elsif Requires_Lock then
         Begin_Window (Deadline,Budget,Available);
         if not Available or else Deadline in 0 | Interfaces.Unsigned_64'Last then
            Fail (State,P.Invalid_Deadline); State.Info.Working := False; return;
         end if;
         Deadline := Interfaces.Unsigned_64'Min (Deadline,State.Window_Deadline);
         pragma Assert (Deadline <= State.Window_Deadline);
         Acquire (State.Item,Deadline,Budget,Result);
         State.Info.Cleanup_Required := Needs_Cleanup (State.Item);
         if Result /= P.Acquired then Fail (State,Result); State.Info.Working := False; return; end if;
         if not State.Info.Cleanup_Required then Fail (State,P.Not_Ready); State.Info.Working := False; return; end if;
         State.Info.Held := True; Owns := True;
         Check_Access (State.Item,Result);
         if Result /= P.Configured then Fail (State,Result); State.Info.Working := False; return; end if;
      end if;
      State.Info.Depth := State.Info.Depth + 1;
      State.Owners (State.Info.Depth) := Owns;
      State.Info.Working := False; Accepted := True;
   end Begin_Field;
   procedure Check_Datum (State : in out Context; Accepted : out Boolean) is
      Result : P.Lock_Outcome;
      Deadline : Interfaces.Unsigned_64;
      Available : Boolean;
   begin
      Accepted := False;
      if not Ready (State) or else State.Info.Depth = 0 then return; end if;
      State.Info.Working := True;
      Read_Window (State,Deadline,Available);
      if not Available then
         Fail (State,P.Invalid_Deadline); State.Info.Working := False; return;
      end if;
      if State.Info.Held then
         Check_Access (State.Item,Result);
         Accepted := Result = P.Configured;
         if not Accepted then Fail (State,Result); end if;
      else
         Accepted := Admission_Open;
         if not Accepted then Fail (State,P.Not_Ready); end if;
      end if;
      State.Info.Working := False;
   end Check_Datum;
   procedure End_Field (State : in out Context; Accepted : out Boolean) is
      Result : P.Lock_Outcome;
   begin
      Accepted := False;
      if State.Info.Working then return; end if;
      if State.Info.Depth = 0 then Fail (State,P.Not_Ready); return; end if;
      State.Info.Working := True;
      if State.Info.Held and then not State.Info.Faulted then
         Check_Access (State.Item,Result);
         if Result /= P.Configured then Fail (State,Result); end if;
      end if;
      if State.Owners (State.Info.Depth) then
         Release (State.Item,Result);
         if Result = P.Released then State.Info.Held := False;
         else Fail (State,Result); end if;
         State.Info.Cleanup_Required := Needs_Cleanup (State.Item);
         if State.Info.Cleanup_Required then Fail (State,P.Not_Ready); end if;
      end if;
      State.Owners (State.Info.Depth) := False;
      State.Info.Depth := State.Info.Depth - 1;
      if State.Info.Depth = 0 then State.Window_Deadline := 0; end if;
      State.Info.Working := False;
      Accepted := not State.Info.Faulted;
   end End_Field;
   procedure Mark_Fault (State : in out Context) is
   begin
      if not State.Info.Working then Fail (State,P.Not_Ready); end if;
   end Mark_Fault;
   procedure Cleanup (State : in out Context; Completed : out Boolean) is
      Result : P.Lock_Outcome;
   begin
      Completed := False;
      if State.Info.Working then return; end if;
      Fail (State,P.Not_Ready);
      if State.Info.Depth /= 0 then return; end if;
      if not State.Info.Cleanup_Required then Completed := True; return; end if;
      State.Info.Working := True;
      Release (State.Item,Result);
      if Result = P.Released then State.Info.Held := False; end if;
      State.Info.Cleanup_Required := Needs_Cleanup (State.Item);
      State.Info.Working := False;
      Completed := Result = P.Released and then not State.Info.Cleanup_Required;
   end Cleanup;
end ACPI_Local_Field_Scope;
