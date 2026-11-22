pragma Ada_2022;
with AML_Names;with AML_Decode;
with ACPI_Device_Policy;with ACPI_Resource_Reader;
package body ACPI_GPIO_Bootstrap_Observation with SPARK_Mode is
   package V renames Core.Values;package N renames Core.Namespace;
   package Reader is new ACPI_Resource_Reader(Core);
   use type V.Access_Status,V.Value_Handle,V.Value_Kind,N.Object_Kind,N.Lookup_Status;
   use type Core.Node_Descriptor,AML_Execute.Execution_Status,Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_8;
   use type Reader.Copy_Status,Resources.Outcome;
   Names:constant array(Member_Kind) of AML_Names.Segment:=
     [Device_Status=>"_STA",Hardware_ID=>"_HID",Unique_ID=>"_UID",Current_Resources=>"_CRS"];
   Enabled_Mask:constant Interfaces.Unsigned_64:=16#02#;
   function Observe(Run:Session) return Report is (Run.Info);
   function Consumed(Run:Session) return Boolean is (Run.Used);
   function Pending(Run:Session) return Boolean is (Run.Handle/=V.No_Value);
   function Description(Run:Session) return Metadata is
     (if Run.Info.Status=Described and then not Pending(Run) then Run.Cache else (others=><>));
   function Owns(Run:Session;Service:Core.State) return Boolean is
     (Core.Current_Binding(Service,Run.Root));
   function Current(Run:Session;Service:Core.State) return Boolean is
      Found:N.Lookup_Result;Item:Core.Node_Descriptor;Status:V.Access_Status;
   begin
      if not Owns(Run,Service) or else not Core.Current_Node(Service,Run.Device) then return False;end if;
      for Kind in Member_Kind loop
         if Run.Examined(Kind) then
            Core.Find_Object(Service,V.Position(Run.Device),Names(Kind),Found,Status);
            if Status/=V.Available then return False;end if;
            if Run.Present(Kind) then
               if Found.Status/=N.Found then return False;end if;
               Core.Describe_Node(Service,Found.Node,Item,Status);
               if Status/=V.Available or else Item/=Run.Members(Kind) or else not Core.Current_Node(Service,Item) then return False;end if;
            elsif Found.Status/=N.Not_Found then return False;end if;
         end if;
      end loop;
      return True;
   end Current;
   function Is_Current(Run:Session;Service:Core.State) return Boolean is
     (Run.Info.Status=Described and then not Pending(Run) and then Current(Run,Service));
   procedure Release(Run:in out Session;Service:in out Core.State) is
   begin
      Run.Info.Before_Cleanup:=Run.Info.Status;
      if Pending(Run) then
         if not Owns(Run,Service) then Run.Info.Status:=Cleanup_Failed;return;end if;
         Release_Result(Service,Run.Handle,Run.Info.Cleanup_Result);
         if Run.Info.Cleanup_Result/=V.Available or else Pending(Run) then Run.Info.Status:=Cleanup_Failed;end if;
         if not Pending(Run) and then Core.Retained_Results(Service)/=0 then Run.Info.Status:=Cleanup_Failed;end if;
      end if;
   end Release;
   procedure Evaluate(Run:in out Session;Service:aliased in out Core.State;
      Kind:Member_Kind;Value:out V.Result;Good:out Boolean) is
      Found:N.Lookup_Result;
   begin
      Good:=False;Value:=(Status=>AML_Execute.No_Return,Charged=>0);Run.Info.At_Member:=Kind;
      if not Current(Run,Service) then Run.Info.Status:=Identity_Changed;return;end if;
      Core.Find_Object(Service,V.Position(Run.Device),Names(Kind),Found,Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available or else Found.Status not in N.Found|N.Not_Found then Run.Info.Status:=Lookup_Failed;return;end if;
      Run.Examined(Kind):=True;Run.Present(Kind):=Found.Status=N.Found;
      if not Run.Present(Kind) then
         if Kind in Device_Status|Unique_ID then Good:=True;else Run.Info.Status:=Missing_Member;end if;return;
      end if;
      Core.Describe_Node(Service,Found.Node,Run.Members(Kind),Run.Info.Access_Result);
      if Run.Info.Access_Result/=V.Available then Run.Info.Status:=Lookup_Failed;return;end if;
      if Run.Info.Remaining=0 then Run.Info.Status:=Budget_Exhausted;return;end if;
      Evaluate_Named(Service,Run.Members(Kind),Run.Info.Remaining,Value,Run.Info.Access_Result);
      Run.Info.Last_Reported_Charge:=Value.Charged;Run.Info.Execution:=Value.Status;
      Run.Info.Field_Uncertainty:=Run.Info.Field_Uncertainty or Value.Status=AML_Execute.Field_Indeterminate;
      if Value.Status=AML_Execute.Fatal_Requested then Run.Info.Fatal:=(Present=>True,Item=>Value.Fatal);end if;
      if Value.Status in AML_Execute.Object_Returned|AML_Execute.Reference_Returned|AML_Execute.Metadata_Returned then
         Run.Handle:=Value.Handle;if not Pending(Run) then Run.Info.Status:=Provider_Contract_Error;return;end if;
      end if;
      if Core.Retained_Results(Service)/=(if Pending(Run) then 1 else 0) then Run.Info.Status:=Provider_Contract_Error;return;end if;
      if Value.Charged>Run.Info.Remaining then Run.Info.Status:=Provider_Contract_Error;return;end if;
      Run.Info.Remaining:=Run.Info.Remaining-Value.Charged;Run.Info.Charged:=Run.Info.Initial_Budget-Run.Info.Remaining;
      if Value.Status=AML_Execute.Field_Indeterminate then Run.Info.Status:=Indeterminate;
      elsif Value.Status=AML_Execute.Fatal_Requested then Run.Info.Status:=Fatal_Requested;
      elsif not Hardware_Quiet(Service) then Run.Info.Status:=Hardware_Not_Quiet;
      elsif not Current(Run,Service) then Run.Info.Status:=Identity_Changed;
      elsif Value.Status=AML_Execute.Budget_Exceeded then Run.Info.Status:=Budget_Exhausted;
      elsif Run.Info.Access_Result/=V.Available then Run.Info.Status:=Evaluation_Failed;
      elsif Value.Status not in AML_Execute.Returned|AML_Execute.Object_Returned then Run.Info.Status:=Wrong_Result_Type;
      else Good:=True;end if;
   end Evaluate;
   procedure Copy_Result(Run:in out Session;Service:in out Core.State;Data:out Firmware_Tables.Bytes) is
      Copied:Reader.Report;Projected:Resources.Report;
   begin
      Data:=[others=>0];
      if not Current(Run,Service) then Run.Info.Status:=Identity_Changed;
      elsif not Hardware_Quiet(Service) then Run.Info.Status:=Hardware_Not_Quiet;
      else
         Reader.Copy_Template(Service,Run.Handle,Data,Copied);Run.Info.Required:=Copied.Required;
         if Copied.Status=Reader.Insufficient_Capacity then Run.Info.Status:=Capacity_Required;return;
         elsif Copied.Status/=Reader.Copied_Template then Run.Info.Status:=Resource_Error;
         else
            Resources.Describe(Data(Data'First..Data'First+(Natural(Copied.Copied)-1)),Projected);
            Run.Info.Projection:=Projected.Status;
            if Projected.Status/=Resources.Described then Run.Info.Status:=Resource_Error;
            else Run.Cache.Resources:=Projected.Value;Run.Info.Status:=Described;end if;
         end if;
      end if;
      Release(Run,Service);
      if Run.Info.Status=Described and then (not Current(Run,Service) or else not Hardware_Quiet(Service) or else Core.Retained_Results(Service)/=0) then Run.Info.Status:=Hardware_Not_Quiet;end if;
      if Run.Info.Status/=Described then Data:=[others=>0];end if;
   end Copy_Result;
   procedure Describe(Run:in out Session;Service:aliased in out Core.State;
      Device:Core.Node_Descriptor;Budget:Natural;Data:out Firmware_Tables.Bytes;Action:out Action_Result) is
      Value:V.Result;Good:Boolean;Shape:V.Value_Description;
      Bytes:AML_Decode.Bytes(1..Hardware_Identifier'Length);Copied:Natural;
      procedure Work is
      begin
         if not Core.Current_Node(Service,Device) or else V.Node_Kind(Device)/=N.Device_Object then Run.Info.Status:=Invalid_Device;return;end if;
         if not Hardware_Quiet(Service) or else Core.Retained_Results(Service)/=0 then Run.Info.Status:=Hardware_Not_Quiet;return;end if;
         Core.Describe_Node(Service,N.Root,Run.Root,Run.Info.Access_Result);
         if Run.Info.Access_Result/=V.Available then Run.Info.Status:=Lookup_Failed;return;end if;
         Run.Device:=Device;Run.Cache.Device:=Device;
         Evaluate(Run,Service,Device_Status,Value,Good);if not Good then return;end if;
         if not Run.Present(Device_Status) then Run.Cache.Status_Flags:=ACPI_Device_Policy.Missing_Status_Default;Run.Cache.Status_Defaulted:=True;
         elsif Value.Status/=AML_Execute.Returned then Run.Info.Status:=Wrong_Result_Type;return;
         else Run.Cache.Status_Flags:=Value.Number;end if;
         if (Run.Cache.Status_Flags and ACPI_Device_Policy.Present_Mask)=0
           or else (Run.Cache.Status_Flags and Enabled_Mask)=0
           or else (Run.Cache.Status_Flags and ACPI_Device_Policy.Functioning_Mask)=0 then Run.Info.Status:=Device_Unusable;return;end if;
         Evaluate(Run,Service,Hardware_ID,Value,Good);if not Good then return;end if;
         if Value.Status/=AML_Execute.Object_Returned then Run.Info.Status:=Unsupported_Identity;return;end if;
         Core.Describe_Result(Service,Run.Handle,Shape,Run.Info.Access_Result);
         if Run.Info.Access_Result/=V.Available or else Shape.Kind/=V.String_Description or else Shape.Length/=Bytes'Length then Run.Info.Status:=Unsupported_Identity;return;end if;
         Core.Read_Result_Bytes(Service,Run.Handle,0,Bytes,Copied,Run.Info.Access_Result);
         if Run.Info.Access_Result/=V.Available or else Copied/=Bytes'Length then Run.Info.Status:=Evaluation_Failed;return;end if;
         for I in Bytes'Range loop Run.Cache.Identifier(I):=Character'Val(Bytes(I));end loop;
         if Run.Cache.Identifier not in "INTC1057"|"INTC1055" then Run.Info.Status:=Unsupported_Identity;return;end if;
         Release(Run,Service);if Run.Info.Status=Cleanup_Failed then return;end if;
         Evaluate(Run,Service,Unique_ID,Value,Good);if not Good then return;end if;
         if not Run.Present(Unique_ID) then Run.Cache.UID.Kind:=No_UID;
         elsif Value.Status=AML_Execute.Returned then
            Run.Cache.UID.Kind:=Integer_UID;Run.Cache.UID.Number:=Value.Number;
         elsif Value.Status=AML_Execute.Object_Returned then
            Core.Describe_Result(Service,Run.Handle,Shape,Run.Info.Access_Result);
            if Run.Info.Access_Result/=V.Available or else Shape.Kind/=V.String_Description
              or else Shape.Length=0 or else Shape.Length>UID_Text'Length then
               Run.Info.Status:=Unsupported_UID;return;
            end if;
            Core.Read_Result_Bytes(Service,Run.Handle,0,Bytes(1..Shape.Length),Copied,Run.Info.Access_Result);
            if Run.Info.Access_Result/=V.Available or else Copied/=Shape.Length then
               Run.Info.Status:=Evaluation_Failed;return;
            end if;
            for I in 1..Copied loop
               if Bytes(I)<Character'Pos('!') or else Bytes(I)>Character'Pos('~') then
                  Run.Info.Status:=Unsupported_UID;return;
               end if;
               Run.Cache.UID.Text(I):=Character'Val(Bytes(I));
            end loop;
            Run.Cache.UID.Kind:=String_UID;Run.Cache.UID.Length:=Copied;
            Release(Run,Service);if Run.Info.Status=Cleanup_Failed then return;end if;
         else Run.Info.Status:=Unsupported_UID;return;
         end if;
         Evaluate(Run,Service,Current_Resources,Value,Good);if not Good then return;end if;
         if Value.Status/=AML_Execute.Object_Returned then Run.Info.Status:=Wrong_Result_Type;return;end if;
         Copy_Result(Run,Service,Data);
      end Work;
   begin
      Data:=[others=>0];Action:=Already_Consumed;if Run.Used then return;end if;Run.Used:=True;Action:=Accepted;
      Run.Info.Initial_Budget:=Budget;Run.Info.Remaining:=Budget;
      Work;
      -- A failed attributable release is retried ONLY by explicit Close.
      if Run.Info.Status not in Described|Capacity_Required|Cleanup_Failed then Release(Run,Service);Data:=[others=>0];end if;
   end Describe;
   procedure Copy_Resources(Run:in out Session;Service:in out Core.State;Data:out Firmware_Tables.Bytes;Action:out Action_Result) is
   begin
      Data:=[others=>0];Action:=Not_Awaiting_Capacity;
      if Run.Info.Status/=Capacity_Required then return;end if;
      if not Owns(Run,Service) then Action:=Wrong_Context;return;end if;
      Action:=Accepted;Copy_Result(Run,Service,Data);
   end Copy_Resources;
   procedure Close(Run:in out Session;Service:in out Core.State;Action:out Action_Result) is
   begin
      Action:=No_Pending_Result;if not Pending(Run) then return;end if;
      if not Owns(Run,Service) then Action:=Wrong_Context;return;end if;
      Action:=Accepted;Release(Run,Service);if not Pending(Run) and then Run.Info.Cleanup_Result=V.Available then Run.Info.Status:=Closed;end if;
   end Close;
end ACPI_GPIO_Bootstrap_Observation;
