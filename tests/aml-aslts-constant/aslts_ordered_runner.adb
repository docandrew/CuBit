pragma Ada_2022;
with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Events;
with Firmware_Tables;
procedure ASLTS_Ordered_Runner is
   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   package IO renames Ada.Streams.Stream_IO;
   use type IO.Count;
   Started : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Host_Window : constant := 180;
   Execution_Budget : constant := 100_000;
   Method_Budget : constant := 1_000_000;
   -- Exact largest cached table from the pinned twelve-case inventory.
   Table_Input_Limit : constant := 155_922;
   procedure Read_Host_Microseconds
     (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      Elapsed : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Clock - Started;
   begin
      Value := 0; Available := False;
      if Elapsed < Ada.Real_Time.Time_Span_Zero
        or else Elapsed > Ada.Real_Time.Seconds (Host_Window)
      then return; end if;
      Value := AML_Decode.Integer_Value (Elapsed / Ada.Real_Time.Microseconds (1));
      Available := True;
   end Read_Host_Microseconds;
   procedure Host_Delay (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Span : constant Ada.Real_Time.Time_Span :=
        (case Item.Kind is
          when AML_Delays.Sleep_Delay => Ada.Real_Time.Milliseconds (Integer (Item.Milliseconds)),
          when AML_Delays.Stall_Delay => Ada.Real_Time.Microseconds (Integer (Item.Microseconds)));
   begin
      Result := AML_Delays.Failed;
      if Now < Started or else Now + Span > Started + Ada.Real_Time.Seconds (Host_Window) then return; end if;
      -- A real hosted wait, with no fabricated clock advance or success.
      -- This is not a hardware Stall precision claim.
      delay until Now + Span;
      Result := AML_Delays.Completed;
   end Host_Delay;
   -- This private runner is single-threaded: no asynchronous Event signals
   -- or owner re-entry can occur while this finite timeout is pending.
   procedure Host_Event_Timeout
     (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome)
   is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Deadline : constant Ada.Real_Time.Time :=
        Now + Ada.Real_Time.Milliseconds (Integer (Milliseconds));
   begin
      Outcome := AML_Events.Failed;
      if Milliseconds = AML_Events.Poll_Only
        or else Milliseconds = AML_Events.Wait_Forever
        or else Now < Started
        or else Deadline > Started + Ada.Real_Time.Seconds (Host_Window)
      then return; end if;
      delay until Deadline;
      if Ada.Real_Time.Clock >= Deadline then
         Outcome := AML_Events.Elapsed;
      end if;
   end Host_Event_Timeout;
   -- Full cached static census, plus explicit hosted runtime reserves.
   -- These are instance budgets, not changes to service defaults or proofs
   -- that every runtime path fits. Seven firmware seeds are additional.
   Static_Node_Requirement : constant := 580;
   Predefined_Node_Allowance : constant := 7;
   Static_Method_Requirement : constant := 144_973;
   Runtime_Node_Reserve : constant := 512;
   Runtime_Method_Reserve : constant := 65_536;
   package Core is new ACPI_Service_Core
     (Perform_Delay => Host_Delay, Read_Microseconds => Read_Host_Microseconds,
      Namespace_Node_Capacity => Static_Node_Requirement + Predefined_Node_Allowance + Runtime_Node_Reserve,
      Aggregate_Method_Capacity => Static_Method_Requirement + Runtime_Method_Reserve,
      Retained_Result_Capacity => 512,
      Wait_Event_Timeout => Host_Event_Timeout,
      Block_Capacity => 128);
   use Core;
   use type Values.Access_Status;
   use type Values.Module_Outcome;
   use type Values.Preparation_Outcome;
   function Read_Input return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      if Ada.Command_Line.Argument_Count /= 1 then
         raise Program_Error with "usage: aslts_ordered_runner exact-table.aml";
      end if;
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare Size : constant IO.Count := IO.Size (File); begin
         if Size < IO.Count (Firmware_Tables.Table_Header_Size) or else Size > Table_Input_Limit then
            IO.Close (File); raise Program_Error with "input outside pinned hosted capacity";
         end if;
         declare
            Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
            Last : Ada.Streams.Stream_Element_Offset;
            Data : Firmware_Tables.Bytes (1 .. Natural (Size));
         begin
            IO.Read (File, Raw, Last); IO.Close (File);
            if Last /= Raw'Last then raise Program_Error with "short read"; end if;
            for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
            return Data;
         end;
      end;
   end Read_Input;
   Table_Data : constant Firmware_Tables.Bytes := Read_Input;
   -- Admit the checked table size explicitly. Only the two named instance
   -- budgets above differ; object/frame/fuel and retained-root quotas do not.
   Service : aliased State (1, Table_Data'Length, Table_Data'Length, Load_Capacity => 1);
   Stage : Stage_Status;
   Frozen : Freeze_Status;
   Access_Result : Values.Access_Status;
   Execution : Execution_Report;
   Members : Namespace.Initialization_Report;
   Node : Namespace.Node_ID;
   Result : AML_Execute.Execution_Result;
   Invoke_Started, Invoke_Finished : Ada.Real_Time.Time;
   procedure Metrics (Phase : String) is
      M : constant Core.Metrics := Observe (Service);
   begin
      Ada.Text_IO.Put_Line ("METRICS " & Phase & " TABLES" & M.Tables'Image & " BYTES" & M.Bytes'Image
        & " NODES" & M.Objects'Image & " VALUES" & M.Value_Objects'Image & " VALUE_BYTES" & M.Value_Bytes'Image
        & " ELEMENTS" & M.Package_Elements'Image & " METHODS" & M.Method_Bytes'Image);
   end Metrics;
   procedure Failure_Snapshot is
      use type Values.Value_Handle;
      use type Values.Value_Kind;
      Entry_Retained : constant Namespace.Owned.Retained_Root_Count := Retained_Results (Service);
      Completed : Natural := 0;
      Count_Known : Boolean := False;
      procedure Drop (Handle : in out Values.Value_Handle) is
         Status : Values.Access_Status;
      begin
         if Handle /= Values.No_Value then
            Release_Result (Service, Handle, Status);
            if Status /= Values.Available then
               raise Program_Error with "snapshot release failed";
            end if;
         end if;
      end Drop;
      procedure Print_Value (Label : String; Handle : Values.Value_Handle) is
         Description : Values.Value_Description;
         Status : Values.Access_Status;
         Data : AML_Decode.Bytes (1 .. 4096);
         Copied : Natural;
         Hex : constant String := "0123456789ABCDEF";
      begin
         Describe_Result (Service, Handle, Description, Status);
         Ada.Text_IO.Put_Line ("SNAPSHOT " & Label & " DESCRIBE " & Status'Image);
         if Status /= Values.Available then return; end if;
         Ada.Text_IO.Put_Line ("SNAPSHOT " & Label & " KIND " & Description.Kind'Image);
         if Description.Kind = Values.Integer_Description then
            Ada.Text_IO.Put_Line ("SNAPSHOT " & Label & " INTEGER" & Description.Number'Image);
         elsif Description.Kind = Values.String_Description then
            Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
            Ada.Text_IO.Put_Line ("SNAPSHOT " & Label & " READ " & Status'Image
              & " LENGTH" & Description.Length'Image & " COPIED" & Copied'Image);
            if Status = Values.Available then
               if Copied /= Natural'Min (Description.Length, Data'Length) then
                  raise Program_Error with "snapshot short string read";
               end if;
               Ada.Text_IO.Put ("SNAPSHOT " & Label & " HEX ");
               for I in 1 .. Copied loop
                  Ada.Text_IO.Put (Hex (Natural (Data (I)) / 16 + 1));
                  Ada.Text_IO.Put (Hex (Natural (Data (I)) mod 16 + 1));
               end loop;
               Ada.Text_IO.New_Line;
               if Description.Length > Data'Length then
                  Ada.Text_IO.Put_Line ("SNAPSHOT " & Label & " TRUNCATED");
               end if;
            end if;
         end if;
      end Print_Value;
      procedure Observe (Name : String; Package_Trace : Boolean := False) is
         Found : Namespace.Node_ID;
         Status : Values.Access_Status;
         Observation : Values.Result;
         Handle, Child : Values.Value_Handle := Values.No_Value;
         Description : Values.Value_Description;
      begin
         -- All names below are exact four-character root data bindings.
         Find_Child (Service, Namespace.Root, Name, Found, Status);
         Ada.Text_IO.Put_Line ("SNAPSHOT " & Name & " LOOKUP " & Status'Image
           & " NODE" & Found'Image);
         if Status /= Values.Available or else Found = Namespace.Root then return; end if;
         Observe_Named_Value (Service, Found, Observation, Status);
         Ada.Text_IO.Put_Line ("SNAPSHOT " & Name & " ACCESS " & Status'Image
           & " STATUS " & Observation.Status'Image & " CHARGED" & Observation.Charged'Image);
         if Observation.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
           | AML_Execute.Metadata_Returned
         then Handle := Observation.Handle; end if;
         if Observation.Charged /= 0 then raise Program_Error with "snapshot charged AML"; end if;
         if Status = Values.Available then
            if Observation.Status = AML_Execute.Returned then
               Ada.Text_IO.Put_Line ("SNAPSHOT " & Name & " INTEGER" & Observation.Number'Image);
               if Name = "RMRC" then
                  Completed := Natural (AML_Decode.Integer_Value'Min (Observation.Number, 6));
                  Count_Known := True;
               end if;
            elsif Handle /= Values.No_Value then
               Print_Value (Name, Handle);
               if Package_Trace and then Count_Known then
                  Describe_Result (Service, Handle, Description, Status);
                  if Status = Values.Available and then Description.Kind = Values.Package_Description then
                     for Index in 1 .. Natural'Min (Completed, Description.Length) loop
                        Read_Result_Element (Service, Handle, Index - 1, Child, Status);
                        Ada.Text_IO.Put_Line ("SNAPSHOT " & Name & " INDEX" & Natural'Image (Index - 1)
                          & " ACCESS " & Status'Image);
                        if Status = Values.Available then Print_Value (Name & Index'Image, Child); end if;
                        Drop (Child);
                     end loop;
                  end if;
               end if;
            end if;
         end if;
         Drop (Handle);
      exception
         when others => Drop (Child); Drop (Handle); raise;
      end Observe;
   begin
      Ada.Text_IO.Put_Line ("SNAPSHOT BEGIN POST_FAILURE_STORED_DATA_ONLY");
      Observe ("NRMT"); Observe ("RMRC"); Observe ("ERRS");
      Observe ("ERR5"); Observe ("ERR6"); Observe ("ERR0");
      Observe ("ERR1"); Observe ("ERR2"); Observe ("FNAM");
      Observe ("RP0P", Package_Trace => True);
      Observe ("ERRP", Package_Trace => True);
      if Retained_Results (Service) /= Entry_Retained then
         raise Program_Error with "snapshot retained handle leak";
      end if;
      Ada.Text_IO.Put_Line ("SNAPSHOT END RETAINED_RESTORED");
   end Failure_Snapshot;
   procedure Check_Completion (Expected : Positive) is
      use type Values.Value_Handle;
      use type Values.Value_Kind;
      Before : constant Namespace.Owned.Retained_Root_Count := Retained_Results (Service);
      Found : Namespace.Node_ID;
      Status : Values.Access_Status;
      Observation : Values.Result;
      Parent, Child : Values.Value_Handle := Values.No_Value;
      Description : Values.Value_Description;
      Expected_Text : constant String := ":STST:functional:constant:CST1:PASS:";
      Data : AML_Decode.Bytes (1 .. Expected_Text'Length);
      Copied : Natural;
      procedure Drop (Handle : in out Values.Value_Handle) is
         Released : Values.Access_Status;
      begin
         if Handle /= Values.No_Value then
            Release_Result (Service, Handle, Released);
            if Released /= Values.Available then
               raise Program_Error with "completion handle release failed";
            end if;
         end if;
      end Drop;
      procedure Stored (Name : String) is
      begin
         Find_Child (Service, Namespace.Root, Name, Found, Status);
         if Status /= Values.Available or else Found = Namespace.Root then
            raise Program_Error with "completion binding missing " & Name;
         end if;
         Observe_Named_Value (Service, Found, Observation, Status);
         if Observation.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
           | AML_Execute.Metadata_Returned
         then Parent := Observation.Handle; end if;
         if Status /= Values.Available or else Observation.Charged /= 0 then
            raise Program_Error with "completion stored observation failed " & Name;
         end if;
      end Stored;
   begin
      Stored ("RMRC");
      if Observation.Status /= AML_Execute.Returned
        or else Observation.Number /= AML_Decode.Integer_Value (Expected)
      then raise Program_Error with "completion count mismatch"; end if;
      Stored ("RP0P");
      if Observation.Status /= AML_Execute.Object_Returned then
         raise Program_Error with "completion records not object";
      end if;
      Describe_Result (Service, Parent, Description, Status);
      if Status /= Values.Available or else Description.Kind /= Values.Package_Description
        or else Description.Length < Expected
      then raise Program_Error with "completion records not package"; end if;
      -- Original RMRC accumulates across invocations. Verify every prior record
      -- as well as the newly appended one, without invoking any AML method.
      for Index in 1 .. Expected loop
         Read_Result_Element (Service, Parent, Index - 1, Child, Status);
         if Status /= Values.Available then raise Program_Error with "completion element unavailable"; end if;
         Describe_Result (Service, Child, Description, Status);
         if Status /= Values.Available or else Description.Kind /= Values.String_Description
           or else Description.Length /= Expected_Text'Length
         then raise Program_Error with "completion record type/length"; end if;
         Read_Result_Bytes (Service, Child, 0, Data, Copied, Status);
         if Status /= Values.Available or else Copied /= Data'Length then
            raise Program_Error with "completion record bytes unavailable";
         end if;
         for J in Data'Range loop
            if Natural (Data (J)) /= Character'Pos (Expected_Text (J)) then
               raise Program_Error with "completion record text mismatch";
            end if;
         end loop;
         Drop (Child);
      end loop;
      Drop (Parent);
      if Retained_Results (Service) /= Before then
         raise Program_Error with "completion retained leak";
      end if;
      Ada.Text_IO.Put_Line ("COMPLETION RMRC" & Expected'Image
        & " ALL_RECORDS_CST1_PASS RETAINED_RESTORED");
   exception
      when others => Drop (Child); Drop (Parent); raise;
   end Check_Completion;

begin
   Stage_Table (Service, 1, DSDT, Table_Data, Stage);
   Ada.Text_IO.Put_Line ("STAGE " & Stage'Image); Metrics ("STAGED");
   if Stage /= Staged then raise Program_Error with "stage failed"; end if;
   Freeze_Catalog (Service, Frozen); Ada.Text_IO.Put_Line ("FREEZE " & Frozen'Image);
   if Frozen /= Catalog_Frozen_OK then raise Program_Error with "freeze failed"; end if;
   Execute_Frozen (Service, Execution_Budget, Execution, Access_Result);
   Ada.Text_IO.Put_Line ("MODULE ACCESS " & Access_Result'Image & " PHASE " & Execution.Phase'Image
     & " PREPARATION " & Execution.Preparation'Image & " OUTCOME " & Execution.Modules.Outcome'Image
     & " COMPLETED" & Execution.Modules.Completed'Image & " CHARGED" & Execution.Modules.Charged'Image
     & " FAILED_TABLE" & Execution.Failed_Table_ID'Image & " FAILURE " & Execution.Modules.Failure'Image);
   Metrics ("EXECUTED");
   if Access_Result /= Values.Available or else Execution.Phase /= Module_Execution_Reported
     or else Execution.Preparation /= Values.Firmware_Prepared
     or else Execution.Modules.Outcome /= Values.Modules_Completed or else Execution.Modules.Completed /= 1
   then raise Program_Error with "module execution failed"; end if;
   Initialize_Members (Service, Members, Access_Result);
   Ada.Text_IO.Put_Line ("MEMBERS ACCESS " & Access_Result'Image & " BOUND" & Members.Bound'Image
     & " MISSING" & Members.Missing'Image & " UNSUPPORTED" & Members.Unsupported'Image);
   if Access_Result /= Values.Available then raise Program_Error with "member initialization failed"; end if;
   for Repeat in 1 .. 2 loop
      Find_Child (Service, Namespace.Root, "MN00", Node, Access_Result);
      if Access_Result /= Values.Available or else Node = Namespace.Root then raise Program_Error with "missing MN00"; end if;
      Invoke_Started := Ada.Real_Time.Clock;
      Invoke_Scalar (Service, Node, [others => 0], 0, Method_Budget, Result);
      Invoke_Finished := Ada.Real_Time.Clock;
      Ada.Text_IO.Put_Line ("MN00_INVOKE_SECONDS"
        & Duration'Image (Ada.Real_Time.To_Duration (Invoke_Finished - Invoke_Started)));
      Ada.Text_IO.Put_Line ("MN00_STATUS " & Result.Status'Image & " CHARGED" & Result.Charged'Image);
      Metrics ("RETURNED");
      if Retained_Results (Service) /= 0 then raise Program_Error with "retained result leak"; end if;
      if Result.Status /= AML_Execute.Returned then raise Program_Error with "MN00 failed"; end if;
      Ada.Text_IO.Put_Line ("RESULT" & Result.Value'Image);
      if Result.Value /= 0 then
         Failure_Snapshot;
         raise Program_Error with "MN00 nonzero result";
      end if;
      if Result.Charged = 0 or else Result.Charged > Method_Budget then raise Program_Error with "MN00 charged"; end if;
      Find_Child (Service, Namespace.Root, "ERRS", Node, Access_Result);
      if Access_Result /= Values.Available or else Node = Namespace.Root then
         raise Program_Error with "missing ERRS";
      end if;
      declare
         Descriptor : Node_Descriptor;
         Named : Values.Result;
         Release_Status : Values.Access_Status;
      begin
         Describe_Node (Service, Node, Descriptor, Access_Result);
         if Access_Result /= Values.Available then
            raise Program_Error with "ERRS descriptor unavailable";
         end if;
         Invoke_Started := Ada.Real_Time.Clock;
         Evaluate_Named (Service, Descriptor, Method_Budget, Named, Access_Result);
         Invoke_Finished := Ada.Real_Time.Clock;
         Ada.Text_IO.Put_Line ("ERRS_EVALUATE_SECONDS"
           & Duration'Image (Ada.Real_Time.To_Duration (Invoke_Finished - Invoke_Started)));
         Ada.Text_IO.Put_Line ("ERRS_ACCESS " & Access_Result'Image
           & " ERRS_STATUS " & Named.Status'Image & " CHARGED" & Named.Charged'Image);
         -- Scalar observations carry no retained handle. Release any unexpected
         -- compound outcome before rejecting it, preserving public API cleanup.
         if Named.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
           | AML_Execute.Metadata_Returned
         then
            Release_Result (Service, Named.Handle, Release_Status);
            if Release_Status /= Values.Available then
               raise Program_Error with "ERRS unexpected handle release failed";
            end if;
         end if;
         if Retained_Results (Service) /= 0 then
            raise Program_Error with "ERRS retained leak";
         end if;
         if Access_Result /= Values.Available or else Named.Status /= AML_Execute.Returned then
            raise Program_Error with "ERRS failed";
         end if;
         Ada.Text_IO.Put_Line ("ERRS_RESULT" & Named.Number'Image);
         if Named.Number /= 0 then raise Program_Error with "ERRS nonzero result"; end if;
         if Named.Charged = 0 or else Named.Charged > Method_Budget then
            raise Program_Error with "ERRS charged";
         end if;
      end;
      Check_Completion (Repeat);
   end loop;
end ASLTS_Ordered_Runner;
