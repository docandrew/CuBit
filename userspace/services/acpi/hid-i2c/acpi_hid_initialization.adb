pragma Ada_2022;
with AML_Execute;
with AML_Names;
package body ACPI_HID_Initialization with SPARK_Mode is
   package N renames Core.Namespace;
   package V renames Core.Values;
   use type N.Object_Kind, N.Lookup_Status, V.Access_Status;
   use type Initialization.Path_Outcome, Initialization.Phase, Queries.Status_Outcome;
   use type AML_Execute.Execution_Status;
   function Name (Value : Outcome) return String is
     (case Value is
        when Eligible => "ELIGIBLE", when Excluded => "EXCLUDED",
        when History_Unknown => "HISTORY_UNKNOWN",
        when Initialization_Incomplete => "INITIALIZATION_INCOMPLETE",
        when Stale_History => "STALE_HISTORY", when Status_Failed => "STATUS_FAILED",
        when Budget_Exhausted => "BUDGET_EXHAUSTED", when Recovery_Required => "RECOVERY_REQUIRED");
   function Status_Known (Item : Report) return Boolean is
     (Item.Last_Status.Outcome = Queries.Status_Available);
   procedure Check_Path
     (Run : Initialization.Session; Service : aliased in out Core.State;
      Item : Core.Node_Descriptor; Budget : Natural; Result : out Report) is
      type Descriptors is array (N.Node_ID) of Core.Node_Descriptor;
      Path : Descriptors := [others => V.No_Node];
      type Membership is array (N.Node_ID) of Boolean;
      On_Path : Membership := [others => False];
      Cursor, Parent : N.Node_ID;
      Status : V.Access_Status;
      Compatible_ID : constant AML_Names.Segment := "_CID";
      Identity : N.Lookup_Result;
      function Current_Path return Boolean is
      begin
         for I in N.Root .. V.Position (Item) loop
            if On_Path (I) and then not Core.Current_Node (Service, Path (I)) then return False; end if;
         end loop;
         return Initialization.Initialization_Path (Run, Service, Item) = Result.History;
      end Current_Path;
   begin
      Result := (others => <>);
      Result.Last_Node := Item;
      Result.History := Initialization.Initialization_Path (Run, Service, Item);
      case Result.History is
         when Initialization.History_Unknown => return;
         when Initialization.Initialization_Not_Complete =>
            if Initialization.Observe (Run).Stage not in Initialization.Completed | Initialization.Completed_With_Errors then
               Result.Result := Initialization_Incomplete; return;
            end if;
            -- A completed walk deliberately skips INI on inactive alternatives.
            -- Current status may exclude them without running identity AML.
            -- A currently eligible path must still have completed its INI.
         when Initialization.Stale_History => Result.Result := Stale_History; return;
         when Initialization.Pruned_Path => Result.Result := Excluded; return;
         when Initialization.Initialization_Ready | Initialization.Initialization_With_Errors => null;
      end case;
      if V.Node_Kind (Item) /= N.Device_Object then Result.Result := Status_Failed; return; end if;
      -- Identification currently accepts the HID-over-I2C compatible ID in
      -- a local _CID. Absence is metadata, requiring no status AML or Field
      -- access on unrelated devices. Validate history and kind first; every
      -- possible candidate still needs the entire current ancestor path.
      Core.Find_Object (Service, V.Position (Item), Compatible_ID, Identity, Status);
      if Status /= V.Available then Result.Result := Status_Failed; return; end if;
      if Identity.Status = N.Not_Found then Result.Result := Excluded; return; end if;
      if Identity.Status /= N.Found then Result.Result := Status_Failed; return; end if;
      Cursor := V.Position (Item);
      loop
         Core.Describe_Node (Service, Cursor, Path (Cursor), Status);
         if Status /= V.Available then Result.Result := Stale_History; return; end if;
         On_Path (Cursor) := True;
         exit when Cursor = N.Root;
         Parent := V.Parent_Position (Path (Cursor));
         if Parent >= Cursor then Result.Result := Stale_History; return; end if;
         Cursor := Parent;
      end loop;
      for I in N.Root .. V.Position (Item) loop
         if On_Path (I) then
            -- Match the existing initializer's status-bearing object kinds.
            -- The candidate itself is still required to be a Device above.
            if V.Node_Kind (Path (I)) in N.Device_Object | N.Processor_Object | N.Thermal_Zone_Object then
               if Result.Charged = Budget then Result.Result := Budget_Exhausted; return; end if;
               Result.Last_Node := Path (I);
               Queries.Observe_Status
                 (Service, Path (I), Budget - Result.Charged, Result.Last_Status);
               if Result.Last_Status.Charged > Budget - Result.Charged then
                  Result.Result := Recovery_Required; return;
               end if;
               Result.Charged := Result.Charged + Result.Last_Status.Charged;
               if Result.Last_Status.Outcome in Queries.Cleanup_Failed | Queries.Provider_Contract_Error
                 or else Result.Last_Status.Execution in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate
               then Result.Result := Recovery_Required; return; end if;
               if not Current_Path then Result.Result := Stale_History; return; end if;
               if Result.Last_Status.Outcome = Queries.Budget_Exhausted then
                  Result.Result := Budget_Exhausted; return;
               elsif Result.Last_Status.Outcome /= Queries.Status_Available then
                  Result.Result := Status_Failed; return;
               end if;
               if I = V.Position (Item) then
                  if not Result.Last_Status.Present or else not Result.Last_Status.Enabled then
                     Result.Result := Excluded; return;
                  end if;
               elsif not Result.Last_Status.Present and then not Result.Last_Status.Functioning then
                  Result.Result := Excluded; return;
               end if;
            end if;
         end if;
      end loop;
      Result.Result := (if Result.History in Initialization.Initialization_Ready | Initialization.Initialization_With_Errors
        then Eligible else Initialization_Incomplete);
   end Check_Path;
end ACPI_HID_Initialization;
