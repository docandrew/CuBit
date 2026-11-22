pragma Ada_2022;
package body GPIO_Startup with SPARK_Mode is
   package T renames GPIO_Community_Types;
   use type GPIO_Controller_Resources.Pin_Count, C.Pin_ID, C.Admission, C.Engine.Phase;
   use type Registry.Channel_ID, T.Pin_Admission, G.Outcome, G.Pad_Number, G.Memory_Resource_Ordinal;
   use type B.Trigger_Mode, B.Polarity;
   subtype Slot is GPIO_Controller_Resources.Pin_Count range 1 .. Registry.Capacity;
   Plans : array (Slot) of G.Plan_Result;
   Modes : array (Slot) of I.Trigger;
   Info : Report;
   Cursor : Slot := Slot'First;
   Until_Time : I.Tick := 0;
   Work_Budget : I.Limit := I.Limit'First;
   function Observe return Report is (Info);
   function Mode (Input : B.Interrupt_Input) return I.Trigger is
     (if Input.Trigger = B.Level_Triggered then
        (if Input.Active = B.Active_Low then I.Level_Low else I.Level_High)
      else (case Input.Active is when B.Active_Low => I.Falling_Edge,
            when B.Active_High => I.Rising_Edge, when B.Active_Both => I.Both_Edges));
   procedure Fail (Why : Failure) is
   begin Info.At_Phase := Failed; Info.Reason := Why; end Fail;
   procedure Start (Pins : Inventory; Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean) is
      Next : Slot := Slot'First;
   begin
      Accepted := False;
      if Info.At_Phase /= Cold then return; end if;
      if C.Observe.Registered /= 0 or else C.Observe.Active /= C.No_Pin or else
         C.Observe.Faulted or else Registry.Observe.Sealed
      then Fail (Not_Empty); return; end if;
      if Pins'Length = 0 then Fail (Empty_Inventory); return; end if;
      if Pins'Length > Natural (Registry.Capacity) then Fail (Capacity_Refused); return; end if;
      for Input of Pins loop
         if not B.Valid (Input) then Fail (Invalid_Input); return; end if;
         Admit_Pin (G.ACPI_Pin (Input.Pin), Info.Last_Admission, Plans (Next));
         if Info.Last_Admission not in T.Admitted | T.Already_Admitted or else Plans (Next).Status /= G.Ready
         then Fail (Unavailable_Pin); return; end if;
         for Prior in Slot'First .. Next - 1 loop
            if Plans (Prior).Selected.Resource = Plans (Next).Selected.Resource and then
              Plans (Prior).Selected.Pad = Plans (Next).Selected.Pad
            then Fail (Duplicate_Pin); return; end if;
         end loop;
         Modes (Next) := Mode (Input); Info.Count := Next;
         if Next < GPIO_Controller_Resources.Pin_Count (Pins'Length) then Next := Next + 1; end if;
      end loop;
      Until_Time := Deadline; Work_Budget := Budget;
      Info.At_Phase := Registering; Accepted := True;
   end Start;
   procedure Step is
      Pin : C.Pin_ID;
      Owner_Result : C.Engine.Admission;
      Worked, Sealed : Boolean;
      Observation : C.Pin_Observation;
   begin
      if Info.At_Phase in Cold | Ready | Failed then return; end if;
      if C.Observe.Faulted then Fail (Controller_Faulted); return; end if;
      if C.Observe.Registered /= Info.Registered then Fail (Layout_Changed); return; end if;
      case Info.At_Phase is
         when Registering =>
            C.Register_Parked (Plans (Cursor), Modes (Cursor), Until_Time, Work_Budget,
               Pin, Info.Last_Controller, Owner_Result);
            if Info.Last_Controller /= C.Accepted then Fail (Registration_Refused); return; end if;
            Info.Registered := Info.Registered + 1;
            if Pin /= C.Pin_ID (Cursor) then Fail (Layout_Changed); return; end if;
            Info.At_Phase := Await_Registration;
         when Await_Registration =>
            if C.Observe.Active /= C.No_Pin then C.Advance (Worked); return; end if;
            Observation := C.Observe_Pin (C.Pin_ID (Cursor));
            if not Observation.Present or else Observation.Report.At_Phase /= C.Engine.Held_Masked
               or else Observation.Report.Pin_May_Be_Enabled
            then Fail (Unexpected_Pin_State); return; end if;
            if Cursor < Info.Count then Cursor := Cursor + 1; Info.At_Phase := Registering;
            else
               Registry.Seal (Until_Time, Work_Budget, Sealed);
               if not Sealed then Fail (Seal_Refused); else Info.At_Phase := Parking; end if;
            end if;
         when Parking =>
            Registry.Step (Worked);
            if Registry.Observe.Layout_Changed then Fail (Layout_Changed);
            elsif Registry.Observe.Unresolved /= 0 then Fail (Parking_Refused);
            elsif Registry.Observe.Operational and then Registry.Quiescent then Info.At_Phase := Ready;
            end if;
         when Cold | Ready | Failed => null;
      end case;
   end Step;
end GPIO_Startup;
