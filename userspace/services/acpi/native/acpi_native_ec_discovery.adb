pragma Ada_2022;
with AML_Execute;
with AML_Decode;
with ACPI_Device_Status;
with ACPI_EC_Resources;
with Firmware_Tables;
with Interfaces;
package body ACPI_Native_EC_Discovery is
   package C renames ACPI_Service;
   package Fields renames ACPI_One_Service.Root.Fields;
   package Status_Check is new ACPI_Device_Status(C,ACPI_One_Service.Runtime.Evaluate_Named);
   use type C.Namespace.Object_Kind,C.Values.Access_Status,Fields.EC_Boot_Status;
   use type Driver.Outcome,Driver.Member_Kind,Driver.Source_Kind;
   use type ACPI_EC_Resources.Register_Role;
   use type Firmware_Tables.Byte,Interfaces.Unsigned_64;
   use type C.Values.Value_Handle,C.Values.Value_Kind,AML_Execute.Execution_Status,AML_Decode.Bytes;
   use type C.Namespace.Lookup_Status;
   type Session_Access is access all Driver.Session;
   First,Second : aliased Driver.Session;
   Identity_Handle : C.Values.Value_Handle := C.Values.No_Value;
   Saved,Working_Session:Session_Access:=null;
   Last:Report;
   Attempted,Working:Boolean:=False;
   function Observe return Report is (Last);
   function Pending return Boolean is
     (Working or else Identity_Handle/=C.Values.No_Value or else (Saved/=null and then Driver.Pending(Saved.all))
      or else (Working_Session/=null and then Driver.Pending(Working_Session.all)));
   function Current(Service:C.State) return Boolean is
     (Last.Status=Ready and then Saved/=null and then Driver.Is_Current(Saved.all,Service));
   procedure Configure(Service:aliased in out C.State;Budget:Natural;Result:out Report) is
      Boot:constant Fields.EC_Boot_Report:=Fields.EC_Boot_Observation;
      Node:C.Node_Descriptor;Access_Result:C.Values.Access_Status;
      Action:Driver.Action_Result;Status:Status_Check.Status_Report;
      Matched,Good:Boolean;
      Remaining:Natural:=Budget;
      Resource_Ceiling:constant Positive:=4_096;
      Data:Firmware_Tables.Bytes(1..Resource_Ceiling);
      System_IO:constant Firmware_Tables.Byte:=1;
      procedure Finish(Code:Outcome) is
      begin
         Last.Status:=Code;Last.Retained:=C.Retained_Results(Service);
         ACPI_Native_Initializer_History.Copy_Path(Service,Last.At_Node,Last.Device_Path,Last.Path_Complete);
         Working:=False;Result:=Last;end Finish;
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
         ACPI_One_Service.Runtime.Evaluate_Named(Service,Member,Remaining,Value,Access_Result);
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
      for Position in C.Namespace.Node_ID range C.Namespace.Root .. C.Namespace_Node_Count(Service) loop
         Last.At_Node:=Position;Last.At_Checkpoint:=Scan_Roots;
         if Remaining=0 then Finish(Budget_Exhausted);return;end if;
         if Natural(C.Pending_Notifications(Service))>0 then Finish(Notifications_Pending);return;end if;
         if C.Retained_Results(Service)>0 then Finish(Recovery_Required);return;end if;
         Last.At_Checkpoint:=Describe_Device;
         C.Describe_Node(Service,Position,Node,Access_Result);
         if Access_Result=C.Values.Available and then C.Values.Node_Kind(Node)=C.Namespace.Device_Object then
            Last.At_Checkpoint:=Identify_Device;Identify(Matched,Good);
            if not Good then Finish(Recovery_Required);return;end if;
            if not Matched then goto Next_Device;end if;
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
               if Remaining=0 then Finish(Budget_Exhausted);return;end if;
               Last.At_Checkpoint:=Check_Status;
               Status_Check.Observe_Status(Service,Node,Remaining,Status);
               if Status.Charged>Remaining then Finish(Recovery_Required);return;end if;
               Remaining:=Remaining-Status.Charged;Last.Charged:=Budget-Remaining;
               if C.Retained_Results(Service)>0 then Finish(Recovery_Required);return;end if;
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
      if Saved=null then Finish(No_EC);return;end if;
      if Last.Description.Source/=Driver.FADT_GPE or else Last.Description.GPIO_Present then Finish(Source_Unsupported);return;end if;
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
      Finish(Ready);
   end Configure;
end ACPI_Native_EC_Discovery;
