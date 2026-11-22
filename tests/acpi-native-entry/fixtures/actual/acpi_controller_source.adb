with ACPI_Resource_Names;
package body ACPI_Controller_Source with SPARK_Mode is
   use type AML_Names.Parse_Status;
   use type Core.Values.Access_Status;
   use type Core.Namespace.Lookup_Status;
   use type Core.Namespace.Object_Kind;
   procedure Resolve (Service : Core.State; Context : Core.Node_Descriptor;
      Source : Firmware_Tables.Bytes; Result : out Report) is
      Path : constant AML_Names.Name_Result := ACPI_Resource_Names.Parse (Source);
      Scope : Core.Namespace.Node_ID;
      Item : Core.Node_Descriptor;
      Lookup : Core.Namespace.Lookup_Result;
      procedure Ascend (Available : out Boolean) is
      begin
         Available := False;
         if Scope = Core.Namespace.Root then return; end if;
         Core.Describe_Node (Service, Scope, Item, Result.Access_Result);
         if Result.Access_Result /= Core.Values.Available or else not Core.Current_Node (Service, Item) then return; end if;
         Scope := Core.Values.Parent_Position (Item); Available := True;
      end Ascend;
      Available : Boolean;
   begin
      Result := (others => <>);
      Result.Parsing := Path.Kind;
      if not Core.Current_Node (Service, Context)
        or else Core.Values.Node_Kind (Context) /= Core.Namespace.Device_Object
      then return; end if;
      if Path.Kind /= AML_Names.Accepted then Result.Status := Bad_Source; return; end if;
      Scope := (if Path.Rooted then Core.Namespace.Root else Core.Values.Position (Context));
      for I in 1 .. Path.Parents loop
         if Scope = Core.Namespace.Root then Result.Status := Above_Root; return; end if;
         Ascend (Available);
         if not Available then Result.Status := Lookup_Failed; return; end if;
      end loop;
      for I in 1 .. Path.Count loop
         loop
            Core.Find_Object (Service, Scope, Path.Parts (I), Lookup, Result.Access_Result);
            if Result.Access_Result /= Core.Values.Available then Result.Status := Lookup_Failed; return; end if;
            exit when Lookup.Status /= Core.Namespace.Not_Found;
            if I /= 1 or else Path.Count /= 1 or else Path.Rooted or else Path.Parents /= 0
              or else Scope = Core.Namespace.Root
            then Result.Status := Not_Found; return; end if;
            Ascend (Available);
            if not Available then Result.Status := Lookup_Failed; return; end if;
         end loop;
         if Lookup.Status /= Core.Namespace.Found then Result.Status := Lookup_Failed; return; end if;
         Scope := Lookup.Node;
      end loop;
      if Scope = Core.Namespace.Root then Result.Status := Not_Device; return; end if;
      Core.Describe_Node (Service, Scope, Item, Result.Access_Result);
      if Result.Access_Result /= Core.Values.Available or else not Core.Current_Node (Service, Item) then
         Result.Status := Lookup_Failed; return;
      end if;
      if Core.Values.Node_Kind (Item) /= Core.Namespace.Device_Object then Result.Status := Not_Device; return; end if;
      Result.Target := Item; Result.Status := Resolved;
   end Resolve;
end ACPI_Controller_Source;
