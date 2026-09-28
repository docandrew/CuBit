with CuBit.Messages; use CuBit.Messages;
with Config_Object_Client.VM.Calls;
with Config_Object_Messages;
with Source_Fixture;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Language;
with CCL.Format;
with Config_Object_Outcomes;

package body Async_Fixture is
   procedure Run
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Types : CCL.Types.Registry;
      Expected : CCL.VM.Value; Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Mode : Scenario; Good : out Boolean)
   is
      use CCL.VM;
      use type Interfaces.Unsigned_64;
      use type Interfaces.Unsigned_32;
      use type Config_Object_Client.Submission;
      use type Config_Object_Client.Completion_Result;
      use type Config_Object_Messages.Status;
      use type CCL.Compiler.Compilation_Status;
      use type CCL.Format.Format_Error;
      use type CCL.Catalog.Link_Result;
      use type Source_Fixture.Operation;
      Candidate : Program;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Linkage : CCL.Catalog.Linkage_Table;
      Linked : CCL.Catalog.Link_Result;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Bytes : CCL.Format.Byte_Array;
      Size : CCL.Format.Module_Length;
      Limits : CCL.Format.Resource_Limits;
      Format_Error : CCL.Format.Format_Error;
      Prepared : Boolean;
      Write_Result : CCL.VM.Value;
      Checked : Validated_Program;
      Error : Validation_Error;
      Machine : Machine_State;
      Outcome : Execution_Result;
      Before : Machine_Snapshot;
      Sent : Config_Object_Client.Submission;
      Done : Config_Object_Client.Completion_Result;
      Reply : Config_Object_Client.VM.Calls.Outcome;
      Action : Source_Fixture.Operation := Source_Fixture.Get_Value;
      Entry_Data : aliased CompletionEntry := NULL_COMPLETION;
      Activity : Activity_Result;
      Pending : Boolean := False;
      Calls, Replies : Natural := 0;
      Write : constant Boolean := Mode /= Read_Existing;
      Write_Code : constant Config_Object_Messages.Status :=
        (if Mode = Denied_Write then Config_Object_Messages.Denied else Config_Object_Messages.Success);
      Write_Revision : constant Interfaces.Unsigned_64 := (if Mode = Denied_Write then 0 else Revision);
      procedure Trace (Stage : String) is
      begin
         debugPrint ("CONFIG-OBJECTS: async " & Stage & ASCII.LF);
      end Trace;
   begin
      Good := False;
      Source_Fixture.Configure (Catalog, Grants, Contract, Types, Write, Prepared);
      if not Prepared then return; end if;
      Trace ("catalog ready");
      CCL.Language.Analyze
        ((if Write then
            "(let ((saved (config-test.set (Reading.Value 42)))) " &
            "(let ((first (config-test.get))) " &
            "(match saved ((ConfigWrite.Committed revision) " &
            (if Mode = Denied_Write then "Reading.Unavailable" else "(config-test.get)") & ") " &
            "((ConfigWrite.InvalidRequest) Reading.Unavailable) ((ConfigWrite.Denied) " &
            (if Mode = Denied_Write then "(config-test.get)" else "Reading.Unavailable") & ") " &
            "((ConfigWrite.Busy) Reading.Unavailable) ((ConfigWrite.Unavailable) Reading.Unavailable) " &
            "((ConfigWrite.Conflict) Reading.Unavailable) ((ConfigWrite.Rejected) Reading.Unavailable) " &
            "((ConfigWrite.Uncertain) Reading.Unavailable))))"
          else "(let ((first (config-test.get))) (config-test.get))"),
         Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Trace ("compile " & Compiled.Status'Image);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then return; end if;
      -- Serialize/restore only the program, never a Config value. Linkage has
      -- schema identities, but no endpoint or process-local runtime binding.
      CCL.Format.Encode (Compiled.Program, Compiled.Linkage,
        (Fuel => 128, Memory => 4096, In_Flight => 1), Bytes, Size, Format_Error, Error);
      if Format_Error /= CCL.Format.Format_Valid then return; end if;
      Trace ("encoded");
      CCL.Format.Decode (Bytes, Size, Candidate, Linkage, Limits, Format_Error, Error);
      if Format_Error /= CCL.Format.Format_Valid then return; end if;
      Trace ("decoded");
      CCL.Catalog.Link_Program (Grants, Linkage, Candidate, Linked, Catalog);
      if Linked /= CCL.Catalog.Link_Valid then return; end if;
      Trace ("linked");
      Verify (Candidate, Checked, Error);
      if Error /= Valid then return; end if;
      Config_Object_Outcomes.To_VM (Candidate.Data_Types, True, Write_Code,
        Write_Revision, Write_Result, Prepared);
      if not Prepared then return; end if;
      Initialize (Checked, Limits.Fuel, Machine);
      loop
         Continue_Execution_For (Checked, Machine, 1, Outcome);
         case Outcome.Status is
            when Paused => null;
            when Waiting_For_Host =>
               if not Pending then
                  Token := Token + 1;
                  if Outcome.Requested_Binding = Source_Fixture.Operation'Enum_Rep (Source_Fixture.Get_Value) then
                     Action := Source_Fixture.Get_Value;
                  elsif Write and Outcome.Requested_Binding = Source_Fixture.Operation'Enum_Rep (Source_Fixture.Set_Value) then
                     Action := Source_Fixture.Set_Value;
                  else return;
                  end if;
                  Config_Object_Client.VM.Calls.Submit
                    (Client, (if Action = Source_Fixture.Get_Value then
                       Config_Object_Client.VM.Calls.Read_Value else
                       Config_Object_Client.VM.Calls.Write_Value),
                     Candidate.Data_Types, Outcome, 0, Token, Sent);
                  if Sent /= Config_Object_Client.Submitted then return; end if;
                  Trace ("submitted " & Action'Image);
                  Pending := True;
                  Calls := Calls + 1;
               end if;
               Before := Snapshot (Machine);
               Continue_Execution_For (Checked, Machine, 1, Outcome);
               if Outcome.Status /= Waiting_For_Host or Snapshot (Machine) /= Before then return; end if;
               if Poll_Completion (Entry_Data'Address) = 1 then
                  Config_Object_Client.Complete (Client, Entry_Data, Done);
                  if Done = Config_Object_Client.Completed then
                     Trace ("completion " & Action'Image);
                     Config_Object_Client.VM.Calls.Take_Outcome (Client, Candidate.Data_Types, Reply);
                     if not Config_Object_Client.VM.Calls.Can_Resume (Reply) or else
                       Reply.Code /= (if Action = Source_Fixture.Get_Value then Config_Object_Messages.Success else Write_Code) or else
                       Reply.Revision /= (if Action = Source_Fixture.Get_Value then Revision else Write_Revision) or else
                       Reply.Value /= (if Action = Source_Fixture.Get_Value then Expected else Write_Result)
                     then return; end if;
                     Complete_Host_Call (Checked, Machine, Reply.Value, True);
                     Pending := False;
                     Replies := Replies + 1;
                  end if;
               else
                  -- Only the test event loop parks; the VM and client return
                  -- immediately while waiting. A GUI can dispatch input here.
                  Activity := Wait_For_Activity_Until (Interfaces.Unsigned_64'Last);
                  if Activity = Unavailable then return; end if;
               end if;
            when Completed =>
               Good := Outcome.Has_Value and Outcome.Result_Value = Expected and
                 not Pending and Calls = (if Write then 3 else 2) and Replies = Calls;
               return;
            when others => Trace ("execution " & Outcome.Status'Image); return;
         end case;
      end loop;
   end Run;
end Async_Fixture;
