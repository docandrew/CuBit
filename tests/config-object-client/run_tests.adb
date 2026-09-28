with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects;
with CCL.Catalog; use CCL.Catalog;
with CCL.Language;
with CCL.Language.Views;
with CCL.Compiler;
with CCL.VM;
with Config_Object_Client.Resources.Runs;
with Config_Object_Interfaces;
with Config_Read_Outcomes;
with Config_Object_Messages;
with CuBit.Messages;
with CuBit.Memory_Grants;

procedure Run_Tests is
   package H renames Config_Object_Client.Resources.Runs;
   package D renames Config_Object_Interfaces;
   package W renames Config_Object_Messages;
   package IPC renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package O renames CCL.Objects;
   package V renames CCL.VM;
   use type V.Execution_Status;
   use type V.Machine_Snapshot;
   use type V.Value;
   use type V.Validation_Error;
   use type O.Build_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Views.Conversion_Status;
   use type CCL.Compiler.Compilation_Status;
   Types : Registry;
   Contract : O.Binding;
   Reads : Config_Read_Outcomes.Description;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Kind : Type_Reference;
   Good : Boolean;
   Program, Replacement : V.Validated_Program;
   Checks : Natural := 0;
   Bindings : constant H.Binding_Table := [D.Open_Collection => 1, D.Read_Value => 2,
     D.Write_Value => 3, D.Close_Collection => 4];
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Site; end if; end Check;
   procedure Compile (Source : String; Item : out V.Validated_Program) is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Linked : Link_Result;
      Validity : V.Validation_Error;
   begin
      CCL.Language.Analyze (Source, Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog); Check (Linked = Link_Valid);
      V.Verify (Compiled.Program, Item, Validity); Check (Validity = V.Valid);
   end Compile;
   procedure Exercise (Stop_At : Natural; Reject_Submit : Boolean := False;
     Existing_Revision : Unsigned_64 := 0; Conflict : Boolean := False;
     Delay_Retirement : Boolean := False) is
      Host : H.Runner (501);
      Tokens, Token : Unsigned_64 := 0;
      Outcome : V.Execution_Result;
      Before : V.Machine_Snapshot;
      Inspected : V.Inspection_Snapshot;
      Consumed : Boolean;
      Stage : Natural := 0;
      Iterations : Natural := 0;
      Receipt : IPC.CompletionEntry;
   begin
      H.Configure (Host, Visible_Types (Catalog), Kind, Contract, Reads, Bindings, 5,
        "org.cubit.run-test", Good); Check (Good);
      H.Load (Host, Program, 128, Good); Check (Good);
      H.Configure (Host, Visible_Types (Catalog), Kind, Contract, Reads, Bindings, 5,
        "org.other", Good); Check (not Good);
      G.Expected_Pages := W.Creation_Bytes / 4096;
      IPC.Accept_Submission := not Reject_Submit;
      loop
         Iterations := Iterations + 1; Check (Iterations < 12);
         H.Advance (Host, 128, Tokens, Outcome);
         exit when not H.Background_Work (Host);
         Stage := Stage + 1;
         Check (IPC.Waits = 0 and H.Waiting_For_IO (Host));
         Token := IPC.Last_Token;
         Before := H.Snapshot (Host);
         H.Load (Host, Replacement, 128, Good); Check (not Good and H.Snapshot (Host) = Before);
         H.Advance (Host, 128, Tokens, Outcome); Check (H.Snapshot (Host) = Before);
         H.Inspect (Host, Inspected); Check (Inspected.Machine = Before);
         Receipt := (requestId => 1, token => Token + 1000, msg => W.Reply (W.Success, 55),
           from => 42, status => IPC.COMPLETION_OK, valid => True);
         H.Complete (Host, Receipt, Tokens, Consumed); Check (not Consumed);
         if Stage = Stop_At then
            H.Stop (Host, Tokens); Check (H.Snapshot (Host).Status = V.Stopped);
            H.Load (Host, Replacement, 128, Good); Check (not Good);
         end if;
         Receipt.token := Token;
         -- These source programs create/read/write/close in order. A stopped
         -- run instead emits the cleanup close, never its next data operation.
         case IPC.Last_Request.tag.label is
            when W.Operation'Enum_Rep (W.Create_Collection) => Receipt.msg := W.Reply (W.Success, 55);
            when W.Operation'Enum_Rep (W.Get_Object) =>
               if Existing_Revision = 0 then Receipt.msg := W.Reply (W.Missing);
               else
                  declare
                     Loan : W.Frame with Import, Address => G.Mapping;
                     Built : O.Build_Result;
                  begin
                     Loan.Value := O.Empty (Contract);
                     O.Append (Loan.Value, O.Integer_Cell (17), Built); Check (Built = O.Added);
                  end;
                  Receipt.msg := W.Reply (W.Success, Existing_Revision);
               end if;
            when W.Operation'Enum_Rep (W.Set_Object) =>
               Check (IPC.Last_Request.words (2) = Existing_Revision);
               Receipt.msg := (if Conflict then W.Reply (W.Conflict)
                 else W.Reply (W.Success, Existing_Revision + 1));
            when W.Operation'Enum_Rep (W.Close_Collection) =>
               Receipt.msg := W.Reply (W.Success);
               if Delay_Retirement then G.Is_Retired := False; end if;
            when others => Check (False);
         end case;
         H.Complete (Host, Receipt, Tokens, Consumed); Check (Consumed);
         H.Complete (Host, Receipt, Tokens, Consumed); Check (not Consumed);
         if not G.Is_Retired then
            Check (not H.Can_Replace (Host));
            H.Load (Host, Replacement, 128, Good); Check (not Good);
            H.Maintain (Host, Tokens); Check (not H.Can_Replace (Host));
            G.Is_Retired := True;
            H.Maintain (Host, Tokens);
         end if;
      end loop;
      IPC.Accept_Submission := True;
      Check (H.Can_Replace (Host));
      if Reject_Submit then Check (Outcome.Status = V.Host_Call_Failed);
      elsif Stop_At > 0 then Check (H.Snapshot (Host).Status = V.Stopped);
      else Check (Outcome.Status = V.Completed and Outcome.Result_Value = V.Integer_Constant (42)); end if;
      if not Reject_Submit and Stop_At = 0 then Check (Stage = 4); end if;
      H.Load (Host, Replacement, 128, Good); Check (Good);
      H.Advance (Host, 128, Tokens, Outcome);
      Check (Outcome.Status = V.Completed and Outcome.Result_Value = V.Integer_Constant (7));
   end Exercise;
   procedure Check_Uncertain_Acquisition is
      Host : H.Runner (502);
      Tokens, Saved_Tokens : Unsigned_64 := 0;
      Outcome : V.Execution_Result;
      Consumed : Boolean;
      Receipt : IPC.CompletionEntry;
   begin
      H.Configure (Host, Visible_Types (Catalog), Kind, Contract, Reads, Bindings, 5,
        "org.cubit.run-test", Good); Check (Good);
      H.Load (Host, Program, 128, Good); Check (Good);
      H.Advance (Host, 128, Tokens, Outcome); Check (H.Waiting_For_IO (Host));
      Receipt := (requestId => 1, token => IPC.Last_Token, msg => W.Reply (W.Uncertain),
        from => 42, status => IPC.COMPLETION_OK, valid => True);
      H.Complete (Host, Receipt, Tokens, Consumed); Check (Consumed);
      Check (not H.Can_Replace (Host) and not H.Waiting_For_IO (Host));
      Check (not H.Cleanup_Retry_Needed (Host));
      Saved_Tokens := Tokens;
      H.Maintain (Host, Tokens); Check (Tokens = Saved_Tokens);
      H.Load (Host, Replacement, 128, Good); Check (not Good);
      H.Stop (Host, Tokens); Check (not H.Can_Replace (Host));
   end Check_Uncertain_Acquisition;
   procedure Check_Token_Exhaustion is
      Host : H.Runner (503);
      Tokens : Unsigned_64 := Unsigned_64'Last - 1;
      Outcome : V.Execution_Result;
   begin
      H.Configure (Host, Visible_Types (Catalog), Kind, Contract, Reads, Bindings, 5,
        "org.cubit.run-test", Good); Check (Good);
      H.Load (Host, Program, 128, Good); Check (Good);
      H.Advance (Host, 128, Tokens, Outcome);
      Check (Outcome.Status = V.Host_Call_Failed and Tokens = Unsigned_64'Last - 1);
      Check (H.Can_Replace (Host) and not H.Background_Work (Host));
   end Check_Token_Exhaustion;
   procedure Compile_Sample (Name : String) is
      File : Ada.Text_IO.File_Type;
      Source : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
      Last : Natural := 0;
      View : CCL.Language.Views.Conversion;
      Item : V.Validated_Program;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "../userspace/ccl/samples/" & Name);
      while not Ada.Text_IO.End_Of_File (File) loop
         declare Line : constant String := Ada.Text_IO.Get_Line (File); begin
            Source (Last + 1 .. Last + Line'Length) := Line;
            Last := Last + Line'Length + 1; Source (Last) := ASCII.LF;
         end;
      end loop;
      Ada.Text_IO.Close (File);
      CCL.Language.Views.Convert (Source (1 .. Last), CCL.Language.Views.Lisp,
        CCL.Language.Views.Lisp, Catalog, View);
      Check (View.Status = CCL.Language.Views.Converted);
      Compile (View.Canonical.Data (1 .. View.Canonical.Length), Item);
   end Compile_Sample;
begin
   O.Bind (Types, Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Config_Read_Outcomes.Define (Contract, Named ("CounterSnapshot"), Named ("CounterRead"),
     [11, 12, 13, 14], Reads, Good); Check (Good);
   D.Publish (Catalog, "settings", [21, 22, 23, 24], Contract, Reads, Kind, Good); Check (Good);
   for Action in D.Operation loop
      declare Op : Resolved_Operation; Installed : Grant_Result; begin
         Resolve (Catalog, "settings." & D.Name (Action), Op, Good); Check (Good);
         Install (Grants, Op, Bindings (Action), Installed); Check (Installed = Grant_Added);
      end;
   end loop;
   Compile ("7", Replacement);
   Compile ("(let ((c (settings.open))) (let ((r (settings.read c))) " &
     "(let ((w (settings.write c 42))) (let ((closed (settings.close c))) 42))))", Program);
   Exercise (0);
   for Stage in 1 .. 4 loop Exercise (Stage); end loop;
   Exercise (0, Reject_Submit => True);
   Exercise (0, Existing_Revision => 7);
   Exercise (0, Existing_Revision => 7, Conflict => True);
   Exercise (0, Delay_Retirement => True);
   Check_Uncertain_Acquisition;
   Check_Token_Exhaustion;
   D.Publish (Catalog, "config-values", [31, 32, 33, 34], Contract, Reads, Kind, Good); Check (Good);
   for Action in D.Operation loop
      declare Op : Resolved_Operation; Installed : Grant_Result; begin
         Resolve (Catalog, "config-values." & D.Name (Action), Op, Good); Check (Good);
         Install (Grants, Op, Bindings (Action), Installed); Check (Installed = Grant_Added);
      end;
   end loop;
   Compile_Sample ("config-counter.ccl");
   Compile_Sample ("config-counter-read.ccl");
   Ada.Text_IO.Put_Line ("Pinned Config execution/event loop: PASS" & Checks'Image & " checks");
end Run_Tests;
