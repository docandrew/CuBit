pragma Ada_2022;
with AML_Names;
with AML_Decode;
with ACPI_Resource_Reader;
package body ACPI_EC_Discovery with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   package Reader is new ACPI_Resource_Reader (Core);
   use type V.Access_Status, V.Value_Handle, V.Value_Kind;
   use type N.Object_Kind, N.Lookup_Status, Core.Node_Descriptor;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value, AML_Decode.Bytes;
   use type Reader.Copy_Status, ACPI_EC_Resources.Status;
   use type ACPI_Resource_Data.Byte_Count;
   Member_Names : constant array (Member_Kind) of AML_Names.Segment :=
     [Hardware_ID => "_HID", Global_Lock => "_GLK", Event_Source => "_GPE", Resource_Set => "_CRS"];
   EC_Number : constant AML_Decode.Integer_Value := 16#090C_D041#;
   EC_Text : constant AML_Decode.Bytes := [16#50#,16#4E#,16#50#,16#30#,16#43#,16#30#,16#39#];
   function Observe (Run : Session) return Report is (Run.Info);
   function Description (Run : Session) return Metadata is
     (if Run.Info.Status = Described then Run.Cache else (others => <>));
   function Consumed (Run : Session) return Boolean is (Run.Used);
   function Pending (Run : Session) return Boolean is
     (Run.Parent /= V.No_Value or Run.Child /= V.No_Value);
   function Owns (Run : Session; Service : Core.State) return Boolean is
     (Core.Current_Binding (Service, Run.Root));
   function Current (Run : Session; Service : Core.State) return Boolean is
      Found : N.Lookup_Result;
      Descriptor : Core.Node_Descriptor;
      Status : V.Access_Status;
   begin
      if not Owns (Run, Service) or else not Core.Current_Node (Service, Run.Device) then return False; end if;
      if Run.Cache.Source = Device_GPE
        and then not Core.Current_Node (Service, Run.Cache.Block_Device)
      then return False; end if;
      for Kind in Member_Kind loop
         if Run.Examined (Kind) then
            Core.Find_Object (Service, V.Position (Run.Device), Member_Names (Kind), Found, Status);
            if Status /= V.Available then return False; end if;
            if Run.Present (Kind) then
               if Found.Status /= N.Found then return False; end if;
               Core.Describe_Node (Service, Found.Node, Descriptor, Status);
               if Status /= V.Available or else Descriptor /= Run.Members (Kind)
                 or else not Core.Current_Node (Service, Descriptor)
               then return False; end if;
            elsif Found.Status /= N.Not_Found then return False;
            end if;
         end if;
      end loop;
      return True;
   end Current;
   function Is_Current (Run : Session; Service : Core.State) return Boolean is
     (Run.Info.Status = Described and then Current (Run, Service));
   procedure Release (Run : in out Session; Service : in out Core.State) is
   begin
      Run.Info.Before_Cleanup := Run.Info.Status;
      if Pending (Run) and then not Owns (Run, Service) then
         Run.Info.Cleanup_Result := V.Invalid_Value; Run.Info.Status := Cleanup_Failed; return;
      end if;
      if Run.Child /= V.No_Value then
         Release_Result (Service, Run.Child, Run.Info.Cleanup_Result);
         if Run.Info.Cleanup_Result /= V.Available or Run.Child /= V.No_Value then
            Run.Info.Status := Cleanup_Failed; return;
         end if;
      end if;
      if Run.Parent /= V.No_Value then
         Release_Result (Service, Run.Parent, Run.Info.Cleanup_Result);
         if Run.Info.Cleanup_Result /= V.Available or Run.Parent /= V.No_Value then
            Run.Info.Status := Cleanup_Failed;
         end if;
      end if;
   end Release;
   procedure Evaluate
     (Run : in out Session; Service : aliased in out Core.State; Kind : Member_Kind;
      Value : out V.Result; OK : out Boolean)
   is
      Found : N.Lookup_Result;
   begin
      OK := False; Value := (Status => AML_Execute.No_Return, Charged => 0);
      Run.Info.At_Member := Kind;
      if not Current (Run, Service) then Run.Info.Status := Identity_Changed; return; end if;
      Core.Find_Object (Service, V.Position (Run.Device), Member_Names (Kind), Found, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available or else Found.Status not in N.Found | N.Not_Found then
         Run.Info.Status := Lookup_Failed; return;
      end if;
      Run.Examined (Kind) := True;
      Run.Present (Kind) := Found.Status = N.Found;
      if not Run.Present (Kind) then
         if Kind = Global_Lock or (Kind = Event_Source and Run.Reduced) then OK := True;
         else Run.Info.Status := Missing_Member; end if;
         return;
      end if;
      Core.Describe_Node (Service, Found.Node, Run.Members (Kind), Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
      if Run.Info.Remaining = 0 then Run.Info.Status := Budget_Exhausted; return; end if;
      Evaluate_Named (Service, Run.Members (Kind), Run.Info.Remaining, Value, Run.Info.Access_Result);
      Run.Info.Last_Reported_Charge := Value.Charged; Run.Info.Execution := Value.Status;
      Run.Info.Field_Uncertainty := Run.Info.Field_Uncertainty or Value.Status = AML_Execute.Field_Indeterminate;
      if Value.Status = AML_Execute.Fatal_Requested then Run.Info.Fatal := (Present => True, Item => Value.Fatal); end if;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         Run.Parent := Value.Handle;
         if Run.Parent = V.No_Value then Run.Info.Status := Provider_Contract_Error; return; end if;
      end if;
      if Value.Charged > Run.Info.Remaining then Run.Info.Status := Provider_Contract_Error; return; end if;
      Run.Info.Remaining := Run.Info.Remaining - Value.Charged;
      Run.Info.Charged := Run.Info.Initial_Budget - Run.Info.Remaining;
      if Value.Status = AML_Execute.Field_Indeterminate then Run.Info.Status := Indeterminate;
      elsif not Current (Run, Service) then Run.Info.Status := Identity_Changed;
      elsif Value.Status = AML_Execute.Budget_Exceeded then Run.Info.Status := Budget_Exhausted;
      elsif Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed;
      elsif Value.Status in AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned | AML_Execute.No_Return then Run.Info.Status := Wrong_Result_Type;
      elsif Value.Status not in AML_Execute.Returned | AML_Execute.Object_Returned then Run.Info.Status := Evaluation_Failed;
      else OK := True;
      end if;
   end Evaluate;
   procedure Read_HID (Run : in out Session; Service : in out Core.State; Value : V.Result) is
      Shape : V.Value_Description;
      Bytes : AML_Decode.Bytes (EC_Text'Range);
      Copied : Natural;
   begin
      Run.Info.Status := Not_An_EC;
      if Value.Status = AML_Execute.Returned then
         if Value.Number = EC_Number then Run.Info.Status := Not_Started; end if;
         return;
      end if;
      Core.Describe_Result (Service, Run.Parent, Shape, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed;
      elsif Shape.Kind /= V.String_Description then Run.Info.Status := Wrong_Result_Type;
      elsif Shape.Length = EC_Text'Length then
         Core.Read_Result_Bytes (Service, Run.Parent, 0, Bytes, Copied, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available or Copied /= Bytes'Length then Run.Info.Status := Evaluation_Failed;
         elsif Bytes = EC_Text then Run.Info.Status := Not_Started;
         end if;
      end if;
   end Read_HID;
   procedure Read_GPE (Run : in out Session; Service : in out Core.State; Value : V.Result) is
      Shape : V.Value_Description;
      Number : AML_Decode.Integer_Value;
   begin
      if Value.Status = AML_Execute.Returned then
         if Value.Number > AML_Decode.Integer_Value (Event_Number'Last) then Run.Info.Status := Unsupported_Value; return; end if;
         Run.Cache.Source := FADT_GPE; Run.Cache.Event := Event_Number (Value.Number); return;
      end if;
      Core.Describe_Result (Service, Run.Parent, Shape, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return;
      elsif Shape.Kind /= V.Package_Description or else Shape.Length /= 2 then Run.Info.Status := Wrong_Result_Type; return;
      end if;
      Core.Read_Result_Element (Service, Run.Parent, 0, Run.Child, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
      Core.Read_Result_Device (Service, Run.Child, Run.Cache.Block_Device, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Wrong_Result_Type; return; end if;
      Release_Result (Service, Run.Child, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or Run.Child /= V.No_Value then Run.Info.Status := Cleanup_Failed; return; end if;
      Core.Read_Result_Element (Service, Run.Parent, 1, Run.Child, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
      Core.Describe_Result (Service, Run.Child, Shape, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return;
      elsif Shape.Kind /= V.Integer_Description then Run.Info.Status := Wrong_Result_Type; return;
      end if;
      Number := Shape.Number;
      if Number > AML_Decode.Integer_Value (Event_Number'Last) then Run.Info.Status := Unsupported_Value; return; end if;
      Run.Cache.Event := Event_Number (Number); Run.Cache.Source := Device_GPE;
   end Read_GPE;
   procedure Copy_Resources
     (Run : in out Session; Service : in out Core.State;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result)
   is
      Copy : Reader.Report;
      Resources : ACPI_EC_Resources.Result;
   begin
      Data := [others => 0]; Action := Not_Awaiting_Capacity;
      if Run.Info.Status /= Capacity_Required or Run.Parent = V.No_Value then return; end if;
      Action := Wrong_Context; if not Owns (Run, Service) then return; end if;
      Action := Accepted;
      if not Current (Run, Service) then Run.Info.Status := Identity_Changed;
      else
         Reader.Copy_Template (Service, Run.Parent, Data, Copy);
         Run.Info.Access_Result := Copy.Access_Status; Run.Info.Required := Copy.Required;
         if Copy.Status = Reader.Insufficient_Capacity then return;
         elsif Copy.Status = Reader.Not_A_Buffer then Run.Info.Status := Wrong_Result_Type;
         elsif Copy.Status /= Reader.Copied_Template or Copy.Copied = 0 then Run.Info.Status := Resource_Error;
         else
            Resources := ACPI_EC_Resources.Describe (Data (Data'First .. Data'First + (Natural (Copy.Copied) - 1)));
            Run.Info.Projection := Resources.Outcome;
            if Resources.Outcome /= ACPI_EC_Resources.Described then Run.Info.Status := Resource_Error;
            elsif Run.Reduced and (Run.Cache.Lock_Required or not Resources.GPIO_Interrupt_Present) then
               Run.Info.Status := Unsupported_Platform;
            else
               Run.Cache.Registers := Resources.Registers;
               Run.Cache.GPIO_Present := Resources.GPIO_Interrupt_Present;
               Run.Cache.GPIO_Offset := Resources.GPIO_Offset;
               if Run.Reduced then Run.Cache.Source := GPIO_Interrupt; end if;
               if not Current (Run, Service) then Run.Info.Status := Identity_Changed;
               else Run.Info.Status := Described; end if;
            end if;
         end if;
      end if;
      Release (Run, Service);
      if Run.Info.Status /= Described then Data := [others => 0]; end if;
   end Copy_Resources;
   procedure Discover
     (Run : in out Session; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; HW_Reduced : Boolean; Budget : Natural;
      Data : out Firmware_Tables.Bytes; Action : out Action_Result)
   is
      Value : V.Result;
      OK : Boolean;
   begin
      Data := [others => 0]; Action := Already_Consumed; if Run.Used then return; end if;
      Run.Used := True; Action := Accepted;
      Run.Info := (Status => Invalid_Device, Initial_Budget => Budget, Remaining => Budget, others => <>);
      if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      Run.Device := Device; Run.Cache.Device := Device; Run.Reduced := HW_Reduced;
      Core.Describe_Node (Service, N.Root, Run.Root, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
      Run.Info.Status := Not_Started;
      Evaluate (Run, Service, Hardware_ID, Value, OK);
      if OK then Read_HID (Run, Service, Value); end if;
      Release (Run, Service);
      if not OK or Run.Info.Status /= Not_Started then return; end if;
      Evaluate (Run, Service, Global_Lock, Value, OK);
      if OK and Run.Present (Global_Lock) then
         if Value.Status /= AML_Execute.Returned then Run.Info.Status := Wrong_Result_Type;
         elsif Value.Number > 1 then Run.Info.Status := Unsupported_Value;
         else Run.Cache.Lock_Declared := True; Run.Cache.Lock_Required := Value.Number = 1;
         end if;
      end if;
      Release (Run, Service);
      if not OK or Run.Info.Status /= Not_Started then return; end if;
      if Run.Reduced and Run.Cache.Lock_Required then Run.Info.Status := Unsupported_Platform; return; end if;
      Evaluate (Run, Service, Event_Source, Value, OK);
      if OK and Run.Present (Event_Source) then Read_GPE (Run, Service, Value); end if;
      --  A failed child release is retained until an explicit Close, not retried
      --  implicitly here. Other errors can release their still-owned roots once.
      if Run.Info.Status /= Cleanup_Failed then Release (Run, Service); end if;
      if not OK or Run.Info.Status /= Not_Started then return; end if;
      Evaluate (Run, Service, Resource_Set, Value, OK);
      if OK and Value.Status = AML_Execute.Object_Returned then
         Run.Info.Status := Capacity_Required;
         Copy_Resources (Run, Service, Data, Action);
      else
         if OK then Run.Info.Status := Wrong_Result_Type; end if;
         Release (Run, Service);
      end if;
   end Discover;
   procedure Close (Run : in out Session; Service : in out Core.State; Action : out Action_Result) is
   begin
      Action := No_Pending_Result; if not Pending (Run) then return; end if;
      Action := Wrong_Context; if not Owns (Run, Service) then return; end if;
      Action := Accepted; Release (Run, Service);
      if not Pending (Run) and Run.Info.Cleanup_Result = V.Available then Run.Info.Status := Closed; end if;
   end Close;
end ACPI_EC_Discovery;
