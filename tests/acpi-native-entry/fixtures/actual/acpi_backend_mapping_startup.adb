pragma Ada_2022;
package body ACPI_Backend_Mapping_Startup with SPARK_Mode is
   use type B.Tick, Chunks.Outcome;
   function Observe (State : Context) return Statistics is
     ((State.Stage, State.Used, State.Completed, State.Working));
   procedure Configure (State : in out Context; Deadline : B.Tick; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif State.Stage /= Fresh then Result := Already_Configured;
      elsif Deadline = B.No_Finite_Deadline then
         State.Stage := Failed; Result := Invalid_Deadline;
      else State.Deadline := Deadline; State.Stage := Open; Result := Configured;
      end if;
   end Configure;
   procedure Map_Window
     (State : in out Context; Held : Authority;
      Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Result : out Report) is
   begin
      Result := (others => <>);
      if State.Working then Result.Status := Reentered; return; end if;
      case State.Stage is
         when Fresh => Result.Status := Not_Configured; return;
         when Sealed => Result.Status := Startup_Sealed; return;
         when Failed => Result.Status := Startup_Failed; return;
         when Open => null;
      end case;
      if Held = No_Authority then
         State.Stage := Failed; Result.Status := Missing_Authority; return;
      end if;
      for I in 1 .. State.Used loop
         if State.Items (I).Held = Held then
            State.Stage := Failed; Result.Status := Duplicate_Authority;
            Result.Slot := I; return;
         end if;
      end loop;
      if State.Used = Window_Count'Last then
         State.Stage := Failed; Result.Status := Capacity_Exceeded; return;
      end if;
      State.Working := True;
      State.Used := State.Used + 1;
      State.Items (State.Used).Held := Held;
      Result.Slot := State.Used;
      Chunks.Map_Exact (State.Items (State.Used).Mapping, Held,
        Physical, Virtual, Pages, Read_Only, State.Deadline, Result.Mapping);
      State.Working := False;
      if Result.Mapping = Chunks.Mapped then
         State.Completed := State.Completed + 1; Result.Status := Window_Mapped;
      else State.Stage := Failed; Result.Status := Mapping_Failed;
      end if;
   end Map_Window;
   procedure Seal (State : in out Context; Accepted : out Boolean) is
   begin
      Accepted := not State.Working and then State.Stage = Open;
      if Accepted then State.Stage := Sealed; end if;
   end Seal;
end ACPI_Backend_Mapping_Startup;
