pragma Ada_2022;
package body ACPI_Device_Enumeration with SPARK_Mode is
   use type N.Object_Kind, Core.Values.Access_Status;
   function Observe (Run : Session) return Report is (Run.Info);
   procedure Start (Run : in out Session; Service : Core.State; Budget : Natural) is
   begin
      if Run.Info.Attempted then return; end if;
      Run.Info.Attempted := True; Run.Info.Budget := Budget;
      Run.Info.Stage := Not_Ready;
      Run.Context := Core.Capture_Context (Service);
      if not Core.Current_Context (Service, Run.Context) then return; end if;
      if Budget = 0 then Run.Info.Stage := Budget_Exhausted; return; end if;
      Core.Describe_Node (Service, N.Root, Run.Anchor, Run.Info.Last_Access);
      Run.Info.Charged := 1;
      if Run.Info.Last_Access /= Core.Values.Available
        or else not Core.Current_Node (Service, Run.Anchor) then return; end if;
      Run.Info.Initial_Last := Core.Namespace_Node_Count (Service);
      Run.Info.Stage := Active;
   end Start;
   procedure Advance (Run : in out Session; Service : Core.State;
      Max_Nodes : Positive; Device : out Core.Node_Descriptor;
      Reads : out Natural; Result : out Step_Result) is
      Candidate : Core.Node_Descriptor;
      Next : N.Node_ID;
   begin
      Device := Core.Values.No_Node; Reads := 0;
      case Run.Info.Stage is
         when Not_Started => Result := No_Start; return;
         when Initial_Range_Exhausted => Result := Range_Exhausted; return;
         when Budget_Exhausted => Result := Out_Of_Budget; return;
         when Topology_Changed => Result := Source_Changed; return;
         when Metadata_Failed | Not_Ready => Result := Failed; return;
         when Active => null;
      end case;
      Result := Wrong_Context;
      if not Core.Current_Context (Service, Run.Context) then return; end if;
      Result := Failed;
      if not Core.Current_Node (Service, Run.Anchor) then
         Run.Info.Stage := Metadata_Failed; return;
      end if;
      if Core.Namespace_Node_Count (Service) /= Run.Info.Initial_Last then
         Run.Info.Stage := Topology_Changed; Result := Source_Changed; return;
      end if;
      Result := Yielded;
      while Run.Info.Last_Visited < Run.Info.Initial_Last and then Reads < Max_Nodes
        and then Run.Info.Charged < Run.Info.Budget
      loop
         Next := Run.Info.Last_Visited + 1;
         Core.Describe_Node (Service, Next, Candidate, Run.Info.Last_Access);
         Reads := Reads + 1; Run.Info.Charged := Run.Info.Charged + 1;
         if Run.Info.Last_Access = Core.Values.Invalid_Value then
            -- Describe_Node reports Invalid_Value only for an absent or
            -- out-of-range slot. Next is in the checked captured range here.
            Run.Info.Holes := Run.Info.Holes + 1;
         elsif Run.Info.Last_Access /= Core.Values.Available
           or else not Core.Current_Node (Service, Candidate)
         then Run.Info.Stage := Metadata_Failed; Result := Failed; return;
         elsif Core.Values.Node_Kind (Candidate) = N.Device_Object then
            Run.Info.Last_Visited := Next; Run.Info.Devices := Run.Info.Devices + 1;
            Device := Candidate; Result := Device_Found; return;
         end if;
         Run.Info.Last_Visited := Next;
      end loop;
      if Run.Info.Last_Visited = Run.Info.Initial_Last then
         Run.Info.Stage := Initial_Range_Exhausted; Result := Range_Exhausted;
      elsif Run.Info.Charged = Run.Info.Budget then
         Run.Info.Stage := Budget_Exhausted; Result := Out_Of_Budget;
      end if;
   end Advance;
end ACPI_Device_Enumeration;
