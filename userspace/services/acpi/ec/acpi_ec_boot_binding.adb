pragma Ada_2022;
with AML_Decode;
package body ACPI_EC_Boot_Binding with SPARK_Mode is
   package B renames ACPI_EC_Boot_Table;
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type B.Decode_Status, B.Path_Status, B.Register_Status;
   use type V.Access_Status, V.Value_Handle, Core.Node_Descriptor;
   use type N.Lookup_Status, N.Object_Kind;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   function Observe (Run : Session) return Report is (Run.Info);
   function Consumed (Run : Session) return Boolean is (Run.Used);
   function Pending (Run : Session) return Boolean is (Run.Handle /= V.No_Value);
   function Device (Run : Session) return Core.Node_Descriptor is
     (if Run.Info.Status = UID_Matched then Run.Target else V.No_Node);
   function Owns (Run : Session; Service : Core.State) return Boolean is
     (Core.Current_Binding (Service, Run.Root));
   function Current (Run : Session; Service : Core.State) return Boolean is
      Parent : N.Node_ID := N.Root;
      Found : N.Lookup_Result;
      Item : Core.Node_Descriptor;
      Status : V.Access_Status;
   begin
      if not Owns (Run, Service) or Run.Count = 0 then return False; end if;
      for I in 1 .. Run.Count loop
         Core.Find_Object (Service, Parent, Run.Parts (I), Found, Status);
         if Status /= V.Available or else Found.Status /= N.Found then return False; end if;
         Core.Describe_Node (Service, Found.Node, Item, Status);
         if Status /= V.Available or else Item /= Run.Path_Nodes (I)
           or else not Core.Current_Node (Service, Item)
         then return False; end if;
         Parent := V.Position (Item);
      end loop;
      if Run.UID_Saved then
         Core.Find_Object (Service, Parent, "_UID", Found, Status);
         if Status /= V.Available or else Found.Status /= N.Found then return False; end if;
         Core.Describe_Node (Service, Found.Node, Item, Status);
         if Status /= V.Available or else Item /= Run.UID_Node
           or else not Core.Current_Node (Service, Item)
         then return False; end if;
      end if;
      return True;
   end Current;
   function Is_Current (Run : Session; Service : Core.State) return Boolean is
     (Run.Info.Status = UID_Matched and then Current (Run, Service));
   procedure Release (Run : in out Session; Service : in out Core.State) is
   begin
      Run.Info.Before_Cleanup := Run.Info.Status;
      if not Pending (Run) then return; end if;
      Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or Run.Handle /= V.No_Value then
         Run.Info.Status := Cleanup_Failed;
      end if;
   end Release;
   procedure Match
     (Run : in out Session; Service : aliased in out Core.State;
      Table : Firmware_Tables.Bytes; Budget : Natural; Action : out Action_Result)
   is
      Item : constant B.Description := B.Decode (Table);
      Path : constant B.Path_Result := B.Read_Path (Table);
      Parent : N.Node_ID := N.Root;
      Found : N.Lookup_Result;
      Value : V.Result;
   begin
      Action := Already_Consumed; if Run.Used then return; end if;
      Run.Used := True; Action := Accepted;
      Run.Info.Initial_Budget := Budget;
      Run.Info.Table_Status := Item.Status; Run.Info.Path_Status := Path.Status;
      Run.Info.Status := Invalid_Table; if Item.Status /= B.Decoded then return; end if;
      Run.Info.Registers := B.Check_Registers (Table);
      Run.Info.Status := Unsupported_Registers;
      if Run.Info.Registers /= B.Byte_Registers_Described then return; end if;
      Run.Info.Status := Invalid_Path; if Path.Status /= B.Parsed then return; end if;
      Core.Describe_Node (Service, N.Root, Run.Root, Run.Info.Access_Result);
      Run.Info.Status := Lookup_Failed; if Run.Info.Access_Result /= V.Available then return; end if;
      Run.Parts := Path.Parts;
      for I in 1 .. Path.Count loop
         Core.Find_Object (Service, Parent, Path.Parts (I), Found, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then return;
         elsif Found.Status = N.Not_Found then Run.Info.Status := Missing_Path; return;
         elsif Found.Status /= N.Found then return;
         end if;
         Core.Describe_Node (Service, Found.Node, Run.Path_Nodes (I), Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then return; end if;
         Parent := Found.Node; Run.Count := I;
      end loop;
      Run.Target := Run.Path_Nodes (Run.Count);
      Run.Info.Status := Not_A_Device;
      if V.Node_Kind (Run.Target) /= N.Device_Object then return; end if;
      Core.Find_Object (Service, Parent, "_UID", Found, Run.Info.Access_Result);
      Run.Info.Status := Lookup_Failed;
      if Run.Info.Access_Result /= V.Available then return;
      elsif Found.Status = N.Not_Found then Run.Info.Status := Missing_UID; return;
      elsif Found.Status /= N.Found then return;
      end if;
      Core.Describe_Node (Service, Found.Node, Run.UID_Node, Run.Info.Access_Result);
      if Run.Info.Access_Result /= V.Available then return; end if;
      Run.UID_Saved := True;
      Run.Info.Status := Identity_Changed; if not Current (Run, Service) then return; end if;
      Run.Info.Status := Budget_Exhausted; if Budget = 0 then return; end if;
      Evaluate_Named (Service, Run.UID_Node, Budget, Value, Run.Info.Access_Result);
      Run.Info.Last_Reported_Charge := Value.Charged;
      Run.Info.Execution := Value.Status;
      Run.Info.Field_Uncertainty := Value.Status = AML_Execute.Field_Indeterminate;
      if Value.Status = AML_Execute.Fatal_Requested then Run.Info.Fatal := (Present => True, Item => Value.Fatal); end if;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         Run.Handle := Value.Handle;
      end if;
      if Value.Charged > Budget then Run.Info.Status := Provider_Contract_Error;
      else
         Run.Info.Charged := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
           and then Run.Handle = V.No_Value
         then Run.Info.Status := Provider_Contract_Error;
         elsif Run.Info.Field_Uncertainty then Run.Info.Status := Indeterminate;
         elsif not Current (Run, Service) then Run.Info.Status := Identity_Changed;
         elsif Value.Status = AML_Execute.Budget_Exceeded then Run.Info.Status := Budget_Exhausted;
         elsif Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed;
         elsif Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned | AML_Execute.No_Return then Run.Info.Status := Wrong_UID_Type;
         elsif Value.Status /= AML_Execute.Returned then Run.Info.Status := Evaluation_Failed;
         elsif Value.Number /= AML_Decode.Integer_Value (Item.UID) then Run.Info.Status := UID_Mismatch;
         else Run.Info.Status := UID_Matched;
         end if;
      end if;
      Release (Run, Service);
   end Match;
   procedure Close
     (Run : in out Session; Service : in out Core.State; Action : out Action_Result) is
   begin
      Action := No_Pending_Result; if not Pending (Run) then return; end if;
      Action := Wrong_Context; if not Owns (Run, Service) then return; end if;
      Action := Accepted; Release (Run, Service);
      if not Pending (Run) and Run.Info.Cleanup_Result = V.Available then Run.Info.Status := Closed; end if;
   end Close;
end ACPI_EC_Boot_Binding;
