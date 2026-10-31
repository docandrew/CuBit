pragma Ada_2022;
package body GPIO_Community_Park with SPARK_Mode is
   package T renames GPIO_Community_Types;
   use type I.Tick, I.Word, I.Access_Status, G.Outcome, T.Pin_Admission;
   use type G.ACPI_Pin, G.Location;
   function Observe (S : State) return Report is (S.Info);
   procedure Fail (S : in out State; Why : Failure) is
   begin S.Info.At_Phase := Failed; S.Info.Reason := Why; end Fail;
   procedure Start (S : in out State; Deadline : I.Tick; Budget : Work_Limit; Accepted : out Boolean) is
   begin
      Accepted := False; if S.Info.At_Phase /= Cold then return; end if;
      S.Until_Time := Deadline; S.Remaining := Budget; S.Info.Starting_Budget := Budget;
      S.Info.At_Phase := Select_Pin; Accepted := True;
   end Start;
   procedure Request_Stop (S : in out State) is
   begin S.Stopping := True; end Request_Stop;
   procedure Next_Pin (S : in out State) is
   begin
      if S.Info.Last_Pin = G.ACPI_Pin'Last then S.Info.At_Phase := Ready;
      else S.Cursor := S.Info.Last_Pin + 1; S.Info.At_Phase := Select_Pin; end if;
   end Next_Pin;
   procedure Step (S : in out State) is
      Now : I.Tick;
      Available : Boolean;
      Found : G.Pin_Search;
      Admission : T.Pin_Admission;
      Value : I.Word := 0;
      Status : I.Access_Status;
      ID : I.Read_ID := I.Pad_Owner;
      Write_ID : I.Write_ID := I.Write_Enable;
      Writing : Boolean := False;
   begin
      if S.Info.At_Phase in Cold | Ready | Failed | Cancelled then return; end if;
      if S.Stopping then S.Info.At_Phase := Cancelled; S.Info.Reason := Stop_Requested; return; end if;
      if S.Remaining = 0 then Fail (S, Budget_Exhausted); return; end if;
      S.Remaining := S.Remaining - 1; S.Info.Consumed_Budget := S.Info.Consumed_Budget + 1;
      Clock (Now, Available);
      if not Available then Fail (S, Clock_Unavailable); return;
      elsif S.Have_Time and then Now < S.Previous_Time then Fail (S, Clock_Reversed); return;
      elsif Now >= S.Until_Time then Fail (S, Deadline_Expired); return; end if;
      S.Have_Time := True; S.Previous_Time := Now;
      if S.Info.At_Phase = Select_Pin then
         Found := G.First_At_Or_After (S.Cursor);
         if not Found.Found then
            if S.Info.Visited = S.Info.Unmapped then Fail (S, No_Resources);
            else S.Info.At_Phase := Ready; end if;
            return;
         end if;
         S.Info.Last_Pin := Found.Pin; S.Info.Visited := S.Info.Visited + 1;
         if not Resource_Ready (G.Locate (Found.Pin).Resource) then
            S.Info.Unmapped := S.Info.Unmapped + 1; Next_Pin (S); return;
         end if;
         Admit_Pin (Found.Pin, Admission, S.Plan);
         if Admission not in T.Admitted | T.Already_Admitted or else S.Plan.Status /= G.Ready
            or else S.Plan.Selected /= G.Locate (Found.Pin)
         then Fail (S, Geometry_Refused); return; end if;
         S.Info.At_Phase := Read_Owner; return;
      end if;
      case S.Info.At_Phase is
         when Read_Owner => ID := I.Pad_Owner;
         when Read_Host => ID := I.Host_Owner;
         when Read_Enable | Mask_Confirm => ID := I.Group_Enable;
         when Mask_Write => Writing := True; Value := S.Desired;
         when Clear_Status => Writing := True; Write_ID := I.Clear_Status; Value := S.Plan.Selected.Interrupt_Mask;
         when Clear_Confirm => ID := I.Group_Status;
         when Cold | Select_Pin | Ready | Failed | Cancelled => return;
      end case;
      S.Info.Last_Access_Phase := S.Info.At_Phase;
      if Writing then
         Write (S.Plan, Write_ID, Value, Status);
         if Status in I.Access_OK | I.Unknown_Effect then S.Info.Effects_Possible := True; end if;
         if Status = I.Access_OK then S.Info.Completed_Writes := S.Info.Completed_Writes + 1; end if;
      else Read (S.Plan, ID, Value, Status); end if;
      S.Info.Accesses := S.Info.Accesses + 1; S.Info.Last_Status := Status; S.Info.Last_Value := Value;
      if Status /= I.Access_OK then
         Fail (S, (if Status = I.Unknown_Effect then Register_Indeterminate else Register_Refused)); return;
      end if;
      case S.Info.At_Phase is
         when Read_Owner => S.Owner_Value := Value; S.Info.At_Phase := Read_Host;
         when Read_Host =>
            if not G.Interrupt_Eligible (S.Plan.Selected, S.Owner_Value, Value, 0) then
               S.Info.Firmware_Owned := S.Info.Firmware_Owned + 1; Next_Pin (S);
            else S.Info.At_Phase := Read_Enable; end if;
         when Read_Enable => S.Desired := Value and not S.Plan.Selected.Interrupt_Mask; S.Info.At_Phase := Mask_Write;
         when Mask_Write => S.Info.At_Phase := Mask_Confirm;
         when Mask_Confirm =>
            if (Value and S.Plan.Selected.Interrupt_Mask) /= 0 then Fail (S, Mask_Not_Confirmed);
            else S.Info.At_Phase := Clear_Status; end if;
         when Clear_Status => S.Info.At_Phase := Clear_Confirm;
         when Clear_Confirm => S.Info.Parked := S.Info.Parked + 1; Next_Pin (S);
         when Cold | Select_Pin | Ready | Failed | Cancelled => null;
      end case;
   end Step;
end GPIO_Community_Park;
