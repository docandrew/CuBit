pragma Ada_2022;
package body ACPI_Local_EC_Commands with SPARK_Mode is
   use type T.Operation, T.Outcome, T.Work_Budget, T.Microseconds;
   function Owns_Command return Boolean is
     (Frame_Held and then Backing_Owned);
   -- Working is deliberately absent: it is already latched around Engine.Step
   -- when the engine calls this actual. Native Read/Write still recheck the
   -- real scope immediately before each access.
   function Observe return Observation is (View);
   function Engine_Observation return T.Transaction_Observation is (Engine.Diagnostics);
   function Wake_At return T.Microseconds is
     (if View.Stage = Running then Engine.Wake_At else 0);
   function Quiescent return Boolean is
     (not Working and then not Frame_Held
      and then View.Stage in Ready | Stopped | Quarantined
      and then Engine.Status = T.Idle and then Command_Quiet);
   procedure Start (Kind : T.Operation; Cell : T.Cell_Offset; Value : T.Byte;
      Deadline : T.Microseconds; Budget : T.Work_Budget;
      Result : out Start_Result; Token : out Token_ID) is
      Accepted : Boolean;
   begin
      Token := No_Token; Result := Busy;
      if Working or else View.Stage in Running | Result_Ready | Retained then return; end if;
      Result := Admission_Stopped;
      if Stopping or else View.Stage in Stopped | Quarantined then return; end if;
      Result := Sequence_Exhausted; if Sequence = Token_ID'Last then return; end if;
      Result := Invalid_Policy; if Deadline = 0 or else Budget = 0 then return; end if;
      Working := True; Sequence := Sequence + 1;
      View := (Token => Sequence, Kind => Kind, others => <>);
      Enter_Command (Kind = T.Query_Event,Kind = T.Write_Cell,Deadline,Accepted);
      if not Accepted then
         View.Why := Entry_Refused;
         View.Recovery := not Command_Quiet;
         View.Stage := (if View.Recovery then Retained else Ready);
         Result := (if View.Recovery then Guard_Retained else Guard_Refused);
         Working := False; return;
      end if;
      Frame_Held := True; View.Guard_Held := True;
      Engine.Start (Kind,Cell,Value,Deadline,Budget,Accepted);
      if not Accepted then
         View.Stage := Retained; View.Why := Engine_Refused; View.Recovery := True;
         Result := Engine_Unavailable; Working := False; return;
      end if;
      View.Stage := Running; View.Hardware := Engine.Status;
      Token := Sequence; Result := Started; Working := False;
   end Start;
   procedure Advance is
   begin
      if Working or else View.Stage /= Running then return; end if;
      Working := True; Engine.Step;
      View.Hardware := Engine.Status;
      View.Effects_Possible := Engine.Effects_Possible;
      View.Recovery := Engine.Needs_Recovery;
      if View.Hardware /= T.Running then
         View.Stage := (if View.Recovery then Retained else Result_Ready);
      end if;
      Working := False;
   end Advance;
   procedure Collect (Item : out Completion) is
      Accepted, Released, Quiet : Boolean;
   begin
      Item := (others => <>);
      if Working or else View.Collected
        or else View.Stage not in Result_Ready | Retained
        or else View.Hardware in T.Idle | T.Running then return; end if;
      Working := True; View.Collected := True;
      Item := (Present => True, Token => View.Token, Kind => View.Kind,
         Hardware => View.Hardware, Value => Engine.Result_Byte,
         Effects_Possible => View.Effects_Possible, Recovery => View.Recovery,
         Stopping => Stopping, Guard_Cleared => False);
      if View.Recovery then Working := False; return; end if;
      Engine.Acknowledge (Accepted);
      if not Accepted then
         View.Stage := Retained; View.Why := Engine_Refused;
         View.Recovery := True; Item.Recovery := True; Working := False; return;
      end if;
      Leave_Command (Released); Quiet := Command_Quiet;
      Frame_Held := not Quiet; View.Guard_Held := Frame_Held;
      if not Released or else not Quiet then
         View.Stage := (if Quiet then Quarantined else Retained);
         View.Why := Cleanup_Refused; View.Recovery := True; Item.Recovery := True;
      else
         Item.Guard_Cleared := True;
         View.Stage := (if Stopping then Stopped else Ready);
      end if;
      Working := False;
   end Collect;
   procedure Request_Stop (Accepted : out Boolean) is
   begin
      Accepted := not Working; if not Accepted then return; end if;
      Working := True; Stopping := True; View.Stopping := True;
      if View.Stage = Running then Engine.Request_Stop;
      elsif View.Stage = Ready then View.Stage := Stopped;
      end if;
      Working := False;
   end Request_Stop;
   procedure Reconcile is
   begin
      if Working or else View.Stage /= Retained or else Engine.Status /= T.Idle then return; end if;
      Working := True;
      if Command_Quiet then
         Frame_Held := False; View.Guard_Held := False; View.Stage := Quarantined;
      end if;
      Working := False;
   end Reconcile;
end ACPI_Local_EC_Commands;
