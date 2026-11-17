pragma Ada_2022;
with AML_Decode;
with ACPI_Device_Status;
with ACPI_EC_Resources;
with Firmware_Tables;
with ACPI_EC_Boot_Table;
with ACPI_Native_Diagnostic_Output;with ACPI_Clock_Source;
package body ACPI_Native_EC_Discovery is
   package C renames ACPI_Service;
   package Fields renames ACPI_One_Service.Root.Fields;
   package Status_Check is new ACPI_Device_Status(C,ACPI_One_Service.Runtime.Evaluate_Discovery);
   use type Status_Check.Status_Outcome,ACPI_One_Service.Runtime.Status.Phase;
   use type C.Namespace.Object_Kind,C.Values.Access_Status,Fields.EC_Boot_Status;
   use type Driver.Outcome,Driver.Member_Kind,Driver.Source_Kind;
   use type ACPI_EC_Resources.Register_Role,Driver.Event_Number;
   use type ACPI_EC_Boot_Table.Decode_Status,Interfaces.Unsigned_32;
   use type Firmware_Tables.Byte,Interfaces.Unsigned_64;
   use type C.Values.Value_Handle,C.Values.Value_Kind,AML_Execute.Execution_Status,AML_Decode.Bytes;
   use type C.Namespace.Lookup_Status,C.Node_Descriptor;
   use type Boot_Binding.Outcome;
   type Session_Access is access all Driver.Session;
   First,Second : aliased Driver.Session;
   Boot_Run:Boot_Binding.Session;
   Boot_Chosen:Boolean:=False;
   Boot_Reconciled:Boolean:=False;
   UID_Validated:Boolean:=False;
   UID_Member:C.Node_Descriptor:=C.Values.No_Node;
   Identity_Handle : C.Values.Value_Handle := C.Values.No_Value;
   Saved,Working_Session:Session_Access:=null;
   Last:Report;
   Attempted,Working:Boolean:=False;
   function Observe return Report is (Last);
   function Pending return Boolean is
     (Working or else Boot_Binding.Pending(Boot_Run) or else Identity_Handle/=C.Values.No_Value or else (Saved/=null and then Driver.Pending(Saved.all))
      or else (Working_Session/=null and then Driver.Pending(Working_Session.all)));
   function UID_Current(Service:C.State) return Boolean is
      Found:C.Namespace.Lookup_Result;Item:C.Node_Descriptor;Status:C.Values.Access_Status;
   begin
      if not UID_Validated or else Saved=null then return False;end if;
      C.Find_Object(Service,C.Values.Position(Driver.Description(Saved.all).Device),"_UID",Found,Status);
      if Status/=C.Values.Available or else Found.Status/=C.Namespace.Found then return False;end if;
      C.Describe_Node(Service,Found.Node,Item,Status);
      return Status=C.Values.Available and then Item=UID_Member and then C.Current_Node(Service,Item);
   end UID_Current;
   function Current(Service:C.State) return Boolean is
     (Last.Status=Ready and then Saved/=null and then Driver.Is_Current(Saved.all,Service)
      and then (not Boot_Chosen or else Boot_Binding.Is_Current(Boot_Run,Service))
      and then (not Boot_Reconciled or else UID_Current(Service)));
   procedure Configure(Service:aliased in out C.State;Budget:Natural;Deadline:Interfaces.Unsigned_64;Result:out Report) is
      Boot:constant Fields.EC_Boot_Report:=Fields.EC_Boot_Observation;
      Node:C.Node_Descriptor;Access_Result:C.Values.Access_Status;
      Action:Driver.Action_Result;Status:Status_Check.Status_Report;
      Matched,Good:Boolean;
      Reconcile:Boolean:=False;
      Remaining:Natural:=Budget;
      Boot_Table:Natural:=0;
      Boot_Device:C.Node_Descriptor;
      Boot_Action:Boot_Binding.Action_Result;
      use type Boot_Binding.Action_Result;
      function Expired return Boolean is
         Now:Interfaces.Unsigned_64;Known:Boolean;
      begin
         ACPI_Clock_Source.Read(Now,Known);
         return Known and then Now>=Deadline;
      end Expired;
      function Recovery_Code return Outcome is
        (if Fields.Control_Idle and then Fields.EC_Commands.Quiescent and then C.Retained_Results(Service)=0
          and then Identity_Handle=C.Values.No_Value and then not Boot_Binding.Pending(Boot_Run) and then Expired
         then Window_Expired else Recovery_Required);
      Resource_Ceiling:constant Positive:=4_096;
      Data:Firmware_Tables.Bytes(1..Resource_Ceiling);
      System_IO:constant Firmware_Tables.Byte:=1;
      Excluded : array (C.Namespace.Node_ID range C.Namespace.Root .. C.Namespace_Node_Count(Service)) of Boolean := [others=>False];
      Maximum_Error_Records : constant Positive := 8;
      Maximum_Path_Preview : constant Positive := 112;
      function Quiet return Boolean is
        (Identity_Handle=C.Values.No_Value and then C.Retained_Results(Service)=0
         and then Fields.Control_Idle and then Fields.EC_Commands.Quiescent
         and then ACPI_One_Service.Runtime.Current=ACPI_One_Service.Runtime.Status.Active);
      procedure Note_Error is
         Text : ACPI_Native_Initializer_History.Path_Text; Complete : Boolean;
      begin
         Last.Node_Errors:=Last.Node_Errors+1;
         if Last.Node_Errors<=Maximum_Error_Records then
            ACPI_Native_Initializer_History.Copy_Path(Service,Last.At_Node,Text,Complete);
            ACPI_Native_Diagnostic_Output.Write("acpi: stage ec-node-error check="&Last.At_Checkpoint'Image&
              " count="&Last.Node_Errors'Image&" status="&Status.Outcome'Image&
              " exec="&(if Last.At_Checkpoint=Check_Status then Status.Execution'Image else Last.Identity_Execution'Image),False);
            ACPI_Native_Diagnostic_Output.Write("acpi: stage ec-node-path complete="&Complete'Image&" preview="&Text(Text'First..Text'First+Maximum_Path_Preview-1),False);
         end if;
      end Note_Error;
      function Ancestor_Excluded return Boolean is
         Cursor : C.Namespace.Node_ID := C.Values.Parent_Position(Node);
         Parent : C.Node_Descriptor; Access_Code : C.Values.Access_Status;
      begin
         for Step in 1 .. C.Namespace_Node_Count(Service) loop
            if Cursor=C.Namespace.Root then return False; end if;
            if Cursor not in Excluded'Range then return True; end if;
            if Excluded(Cursor) then return True; end if;
            C.Describe_Node(Service,Cursor,Parent,Access_Code);
            if Access_Code/=C.Values.Available then return True; end if;
            Cursor:=C.Values.Parent_Position(Parent);
         end loop;
         return True;
      end Ancestor_Excluded;
      procedure Finish(Code:Outcome) is
      begin
         Last.Status:=Code;Last.Retained:=C.Retained_Results(Service);
         Last.Boot_Selected:=Boot_Chosen;Last.Boot_Reconciled:=Boot_Reconciled;Last.Boot_Association:=Boot_Binding.Observe(Boot_Run);
         ACPI_Native_Initializer_History.Copy_Path(Service,Last.At_Node,Last.Device_Path,Last.Path_Complete);
         ACPI_Native_Diagnostic_Output.Write("acpi: stage ec-scan status="&Code'Image&" absent="&Last.Absent_Devices'Image&
           " pruned="&Last.Pruned_Devices'Image&" node-errors="&Last.Node_Errors'Image&
           " ecdt="&Boot_Chosen'Image&" reconciled="&Boot_Reconciled'Image&" uid-matched="&UID_Validated'Image&" bind="&Last.Boot_Association.Status'Image,False);
         Working:=False;Result:=Last;end Finish;
      -- This is a separate descriptive UID observation, not a successful
      -- ECDT path binding. The immutable boot coordinates remain unchanged.
      procedure Match_UID(Matched,Good:out Boolean) is
         Found:C.Namespace.Lookup_Result;Member:C.Node_Descriptor;Value:C.Values.Result;
      begin
         Matched:=False;Good:=False;
         C.Find_Object(Service,C.Values.Position(Node),"_UID",Found,Access_Result);
         if Access_Result/=C.Values.Available or else Found.Status/=C.Namespace.Found then return;end if;
         C.Describe_Node(Service,Found.Node,Member,Access_Result);if Access_Result/=C.Values.Available then return;end if;
         Last.UID_Node:=Member;
         ACPI_One_Service.Runtime.Evaluate_Named(Service,Member,Remaining,Value,Access_Result);
         Last.UID_Access:=Access_Result;Last.UID_Execution:=Value.Status;
         if Value.Status in AML_Execute.Object_Returned|AML_Execute.Reference_Returned|AML_Execute.Metadata_Returned then
            Identity_Handle:=Value.Handle;
         end if;
         if Value.Charged>Remaining then return;end if;
         Remaining:=Remaining-Value.Charged;Last.Charged:=Budget-Remaining;
         Good:=Access_Result=C.Values.Available and then C.Current_Node(Service,Node)
           and then C.Current_Node(Service,Member) and then Value.Status=AML_Execute.Returned;
         if Good then Matched:=Value.Number=AML_Decode.Integer_Value(Boot.UID);end if;
         if Identity_Handle/=C.Values.No_Value then
            C.Release_Result(Service,Identity_Handle,Access_Result);
            Good:=Good and then Access_Result=C.Values.Available and then Identity_Handle=C.Values.No_Value;
         end if;
         if Good and then Matched then UID_Member:=Member;UID_Validated:=True;end if;
      end Match_UID;
      -- The prefilter owns its one possible result until attributable cleanup.
      -- Discovery sessions are fixed process-lived objects, consumed once each:
      -- one selected controller and one possible conflicting controller.
      procedure Identify(Matched,Good:out Boolean) is
         Found:C.Namespace.Lookup_Result;Member:C.Node_Descriptor;
         Value:C.Values.Result;Shape:C.Values.Value_Description;
         Bytes:AML_Decode.Bytes(1..7);Copied:Natural;
         Identifier:constant AML_Decode.Integer_Value:=16#090C_D041#;
         Text:constant AML_Decode.Bytes:=[16#50#,16#4E#,16#50#,16#30#,16#43#,16#30#,16#39#];
      begin
         Matched:=False;Good:=False;
         Last.Identity_Node:=C.Namespace.Root;Last.Identity_Access:=C.Values.Wrong_Phase;
         Last.Identity_Cleanup:=C.Values.Wrong_Phase;Last.Identity_Execution:=AML_Execute.Unsupported_Value;
         Last.Identity_Kind_Known:=False;
         C.Find_Object(Service,C.Values.Position(Node),"_HID",Found,Access_Result);
         if Access_Result/=C.Values.Available then return;end if;
         if Found.Status=C.Namespace.Not_Found then Good:=True;return;end if;
         if Found.Status/=C.Namespace.Found then return;end if;
         C.Describe_Node(Service,Found.Node,Member,Access_Result);if Access_Result/=C.Values.Available then return;end if;
         Last.Identity_Node:=Found.Node;
         ACPI_One_Service.Runtime.Evaluate_Discovery(Service,Member,Remaining,Value,Access_Result);
         Last.Identity_Access:=Access_Result;Last.Identity_Execution:=Value.Status;
         if Value.Status in AML_Execute.Object_Returned|AML_Execute.Reference_Returned|AML_Execute.Metadata_Returned then
            Identity_Handle:=Value.Handle;
         end if;
         if Value.Charged>Remaining then return;end if;
         Remaining:=Remaining-Value.Charged;Last.Charged:=Budget-Remaining;
         if Access_Result=C.Values.Available and then C.Current_Node(Service,Node) and then C.Current_Node(Service,Member) then
            if Value.Status=AML_Execute.Returned then Matched:=Value.Number=Identifier;Good:=True;
            elsif Identity_Handle/=C.Values.No_Value then
               C.Describe_Result(Service,Identity_Handle,Shape,Access_Result);
               if Access_Result=C.Values.Available then
                  Last.Identity_Kind:=Shape.Kind;Last.Identity_Kind_Known:=True;
                  if Shape.Kind=C.Values.String_Description then
                     Good:=True;
                     if Shape.Length=Text'Length then
                        C.Read_Result_Bytes(Service,Identity_Handle,0,Bytes,Copied,Access_Result);
                        Good:=Access_Result=C.Values.Available and then Copied=Bytes'Length;
                        Matched:=Good and then Bytes=Text;
                     end if;
                  elsif Shape.Kind=C.Values.Integer_Description then
                     Good:=True;Matched:=Shape.Number=Identifier;
                  end if;
               end if;
            end if;
         end if;
         if Identity_Handle/=C.Values.No_Value then
            C.Release_Result(Service,Identity_Handle,Access_Result);Last.Identity_Cleanup:=Access_Result;
            Good:=Good and then Access_Result=C.Values.Available and then Identity_Handle=C.Values.No_Value;
         end if;
      end Identify;
   begin
      Result:=Last;if Attempted or Working then return;end if;Attempted:=True;Working:=True;
      if Deadline in 0|Interfaces.Unsigned_64'Last or else Expired then Finish(Window_Expired);return;end if;
      -- A unique validated ECDT names this physical controller. Bind that exact
      -- path/UID first; do not infer uniqueness of all ECs in the machine.
      for Table in 1..C.Table_Count(Service) loop
         if C.Table_Info(Service,Table).Signature="ECDT" then
            if Boot_Table/=0 then Finish(Boot_Binding_Failed);return;end if;Boot_Table:=Table;
         end if;
      end loop;
      if Boot_Table/=0 then
         Last.At_Checkpoint:=Bind_Boot_Controller;
         if Boot.Status/=Fields.EC_Ready then Finish(Boot_Unavailable);return;end if;
         if C.Table_Info(Service,Boot_Table).Extent>Resource_Ceiling then Finish(Boot_Binding_Failed);return;end if;
         declare Bytes:Firmware_Tables.Bytes(1..C.Table_Info(Service,Boot_Table).Extent);Facts:ACPI_EC_Boot_Table.Description;begin
            for Offset in Bytes'Range loop Bytes(Offset):=C.Table_Byte(Service,Boot_Table,Offset-Bytes'First);end loop;
            Facts:=ACPI_EC_Boot_Table.Decode(Bytes);
            if Facts.Status/=ACPI_EC_Boot_Table.Decoded then Finish(Boot_Binding_Failed);return;end if;
            if Facts.Control.Address/=Boot.Control or else Facts.Data.Address/=Boot.Data
              or else Facts.UID/=Boot.UID or else Facts.GPE_Bit/=Firmware_Tables.Byte(Boot.GPE) then
               Finish(Resources_Disagree);return;
            end if;
            Boot_Binding.Match(Boot_Run,Service,Bytes,Remaining,Boot_Action);
         end;
         Last.Boot_Association:=Boot_Binding.Observe(Boot_Run);
         if Last.Boot_Association.Last_Reported_Charge>Remaining then Finish(Recovery_Code);return;end if;
         Remaining:=Remaining-Last.Boot_Association.Charged;Last.Charged:=Budget-Remaining;
         if not Quiet or else Boot_Binding.Pending(Boot_Run) then Finish(Recovery_Code);return;end if;
         if Boot_Action/=Boot_Binding.Accepted then Finish(Boot_Binding_Failed);return;end if;
         if Boot_Binding.Is_Current(Boot_Run,Service) then
            Boot_Device:=Boot_Binding.Device(Boot_Run);Boot_Chosen:=True;
         elsif Last.Boot_Association.Status=Boot_Binding.Missing_Path then
            -- Some firmware retains a stale ECDT namespace name. Only this
            -- precise failure permits fresh PNP0C09 discovery below. A found
            -- path with bad UID/type/ownership never gets a second selection.
            Reconcile:=True;
         else Finish(Boot_Binding_Failed);return;
         end if;
      end if;
      for Position in C.Namespace.Node_ID range C.Namespace.Root .. C.Namespace_Node_Count(Service) loop
         if Boot_Chosen and then Position/=C.Values.Position(Boot_Device) then goto Next_Device;end if;
         Last.At_Node:=Position;Last.At_Checkpoint:=Scan_Roots;
         if Remaining=0 then Finish(Budget_Exhausted);return;end if;
         if Natural(C.Pending_Notifications(Service))>0 then Finish(Notifications_Pending);return;end if;
         if C.Retained_Results(Service)>0 then Finish(Recovery_Code);return;end if;
         Last.At_Checkpoint:=Describe_Device;
         C.Describe_Node(Service,Position,Node,Access_Result);
         if Access_Result=C.Values.Available and then C.Values.Node_Kind(Node)=C.Namespace.Device_Object then
            if Ancestor_Excluded then Last.Pruned_Devices:=Last.Pruned_Devices+1;goto Next_Device;end if;
            if Expired then Finish(Window_Expired);return;end if;
            if Reconcile then
               -- In compatibility enumeration identify first, so unrelated
               -- device status methods cannot consume the EC selection window.
               Last.At_Checkpoint:=Identify_Device;Identify(Matched,Good);
               if not Good then
                  if not Quiet then Finish(Recovery_Required);return;end if;
                  Note_Error;Excluded(Position):=True;goto Next_Device;
               end if;
               if not Matched then goto Next_Device;end if;
               if Expired then Finish(Window_Expired);return;end if;
            end if;
            Last.At_Checkpoint:=Check_Status;
            Status_Check.Observe_Status(Service,Node,Remaining,Status);
            if Status.Last_Charge>Remaining or else Status.Charged>Remaining then Finish(Recovery_Required);return;end if;
            Remaining:=Remaining-Status.Charged;Last.Charged:=Budget-Remaining;
            if not Quiet then Finish(Recovery_Required);return;end if;
            if Status.Outcome/=Status_Check.Status_Available then
               Note_Error;Excluded(Position):=True;goto Next_Device;
            end if;
            if not Status.Present then
               Last.Absent_Devices:=Last.Absent_Devices+1;
               -- Functioning-only parents still expose their children.
               Excluded(Position):=not Status.Functioning;goto Next_Device;
            end if;
            if Remaining=0 then Finish(Budget_Exhausted);return;end if;
            if Expired then Finish(Window_Expired);return;end if;
            if not Reconcile then
            Last.At_Checkpoint:=Identify_Device;Identify(Matched,Good);
            if not Good then
               if not Quiet then Finish(Recovery_Required);return;end if;
               Note_Error;Excluded(Position):=True;goto Next_Device;
            end if;
            if not Matched then goto Next_Device;end if;
            end if;
            if Reconcile then
               Last.At_Checkpoint:=Match_Boot_UID;Match_UID(Matched,Good);
               if not Quiet then Finish(Recovery_Code);return;end if;
               if not Good or else not Matched then Finish(Resources_Disagree);return;end if;
            end if;
            -- Missing early backing alone does not establish an absent EC.
            -- First identify an actual namespace controller; only a complete
            -- successful scan with no matching identity may report No_EC.
            if Boot.Status/=Fields.EC_Ready then Finish(Boot_Unavailable);return;end if;
            if Remaining=0 then Finish(Budget_Exhausted);return;end if;
            Working_Session:=(if Saved=null then First'Access else Second'Access);
            Last.At_Checkpoint:=Discover_Controller;
            Driver.Discover(Working_Session.all,Service,Node,False,Remaining,Data,Action);
            Last.Failure:=Driver.Observe(Working_Session.all);
            if Last.Failure.Charged>Remaining then Finish(Recovery_Required);return;end if;
            Remaining:=Remaining-Last.Failure.Charged;Last.Charged:=Budget-Remaining;
            if Driver.Pending(Working_Session.all) or else Last.Failure.Field_Uncertainty
              or else C.Retained_Results(Service)>0 then Finish(Recovery_Required);return;end if;
            if Last.Failure.Status=Driver.Not_An_EC
              or else (Last.Failure.Status=Driver.Missing_Member and then Last.Failure.At_Member=Driver.Hardware_ID)
            then Finish(Discovery_Failed);return;
            elsif Last.Failure.Status/=Driver.Described then
               Finish(Discovery_Failed);return;
            else
               if not Status_Check.Usable(Status) then
                  Finish(Device_Unusable);return;
               end if;
               Last.Candidates:=Last.Candidates+1;
               if Last.Candidates>1 then Finish(Multiple_Controllers);return;end if;
               Saved:=Working_Session;Working_Session:=null;
               Last.Description:=Driver.Description(Saved.all);
            end if;
         elsif Access_Result not in C.Values.Available|C.Values.Invalid_Value then
            Finish(Discovery_Failed);return;
         end if;
         <<Next_Device>>
         null;
      end loop;
      Last.At_Checkpoint:=Scan_Finished;
      if Saved=null then Finish((if Reconcile then Boot_Binding_Failed elsif Last.Node_Errors=0 then No_EC else Discovery_Failed));return;end if;
      if Last.Description.Source/=Driver.FADT_GPE or else Last.Description.GPIO_Present then Finish(Source_Unsupported);return;end if;
      if (Boot_Chosen or else Reconcile) and then Last.Description.Event/=Driver.Event_Number(Boot.GPE) then Finish(Resources_Disagree);return;end if;
      Last.At_Checkpoint:=Validate_Resources;
      for Role in ACPI_EC_Resources.Register_Role loop
         declare Claim:constant ACPI_EC_Resources.Register_Claim:=Last.Description.Registers(Role);begin
            if Claim.Address.Space/=System_IO or else Claim.IO_Decode_10
              or else (Claim.Writeability_Declared and then not Claim.Declared_Writeable)
              or else Claim.Address.Address/=(if Role=ACPI_EC_Resources.Data_Register then Boot.Data else Boot.Control)
            then Finish(Resources_Disagree);return;end if;
         end;
      end loop;
      if not Driver.Is_Current(Saved.all,Service) or else C.Retained_Results(Service)>0 then Finish(Recovery_Required);return;end if;
      if Natural(C.Pending_Notifications(Service))>0 then Finish(Notifications_Pending);return;end if;
      if Reconcile and then not UID_Current(Service) then Finish(Recovery_Required);return;end if;
      if Expired then Finish(Window_Expired);return;end if;
      Boot_Reconciled:=Reconcile;
      Last.At_Node:=C.Values.Position(Last.Description.Device);
      Finish(Ready);
   end Configure;
end ACPI_Native_EC_Discovery;
