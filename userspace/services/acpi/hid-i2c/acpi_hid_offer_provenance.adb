pragma Ada_2022;
with Firmware_Tables.Identifiers;
package body ACPI_HID_Offer_Provenance with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   package G renames GPIO_Bootstrap_Payload;
   use type V.Access_Status, Core.Node_Descriptor, N.Definition_Identity;
   use type Builder.Outcome;
   use type Interfaces.Unsigned_64;
   procedure Build (Service : Core.State; Observation : Resolver.Observation;
      Admission_Device : Core.Node_Descriptor; Admission : G.Request;
      PCI : Wire.PCI_Observation; History : Stamp;
      Result : out Builder.Report; Status : out Outcome) is
      Context : Builder.Context;
      Device : constant Core.Node_Descriptor := Resolver.Device (Observation);
      Definition : N.Definition_Identity;
      Access_Result : V.Access_Status;
      Table : Natural := 0;
      Complete : Boolean;
      procedure Path (Item : Core.Node_Descriptor; Value : out Wire.Canonical_Path;
         Accepted : out Boolean) is
         Reverse_Path : Wire.Name_Path := [others => [others => Wire.NUL]];
         Count : Wire.Path_Count := 0;
         Cursor : N.Node_ID := V.Position (Item);
         Node : Core.Node_Descriptor;
         Access_Status : V.Access_Status;
      begin
         Value := (others => <>); Accepted := False; Status := Invalid_Path;
         while Cursor /= N.Root loop
            if Count = Wire.Max_Path_Segments then Status := Path_Too_Deep; return; end if;
            Core.Describe_Node (Service, Cursor, Node, Access_Status);
            if Access_Status /= V.Available or else not Core.Current_Node (Service, Node)
              or else V.Parent_Position (Node) >= Cursor then return; end if;
            Count := Count + 1;
            Reverse_Path (Count) := V.Node_Name (Node);
            Cursor := V.Parent_Position (Node);
         end loop;
         if Count = 0 then return; end if;
         Value.Count := Count;
         for I in 1 .. Count loop Value.Names (I) := Reverse_Path (Count + 1 - I); end loop;
         Accepted := True;
      end Path;
   begin
      Result := (others => <>); Status := Owner_Busy;
      if Core.Retained_Results (Service) /= 0 then return; end if;
      Status := Stale_Observation;
      if not Resolver.Current (Service, Observation)
        or else not Core.Current_Node (Service, Admission_Device) then return; end if;
      Status := Invalid_Admission;
      if not G.Valid (Admission) or else Admission.Epoch /= History.Epoch then return; end if;
      Status := GPIO_Binding_Mismatch;
      if Resolver.GPIO_Controller (Observation) /= Admission_Device then return; end if;
      Path (Device, Context.Device_Path, Complete); if not Complete then return; end if;
      Path (Resolver.I2C_Controller (Observation), Context.I2C_Path, Complete); if not Complete then return; end if;
      Path (Admission_Device, Context.GPIO_Path, Complete); if not Complete then return; end if;
      Status := GPIO_Binding_Mismatch;
      if Natural (Context.GPIO_Path.Count) /= Natural (Admission.Segments) then return; end if;
      for I in 1 .. Admission.Segments loop
         if Context.GPIO_Path.Names (I) /= Admission.Path (I) then return; end if;
      end loop;
      Core.Describe_Definition (Service, Device, Definition, Access_Result);
      Status := Unknown_Definition;
      if Access_Result /= V.Available or else not N.Has_Definition (Definition) then return; end if;
      for I in 1 .. Core.Table_Count (Service) loop
         if Core.Retained_Definition (Service, I) = Definition then
            if Table /= 0 then Status := Ambiguous_Definition; return; end if;
            Table := I;
         end if;
      end loop;
      if Table = 0 then return; end if;
      declare Identity : constant Firmware_Tables.Identifiers.Identity := Core.Table_Identity (Service, Table); begin
         Status := Unsupported_Table;
         if Identity.Name not in "DSDT" | "SSDT" then return; end if;
         Context.Table := Identity.Name; Context.Table_ID := Identity.OEM_Table;
      end;
      Context.Epoch := History.Epoch;
      Context.Catalog_Revision := History.Catalog_Revision;
      Context.Observation_Sequence := History.Observation_Sequence;
      Context.Initializer_Count := History.Initializer_Count;
      Context.Initializer_Digest := History.Initializer_Digest;
      Context.PCI := PCI;
      Context.GPIO_HID := Admission.HID;
      Context.GPIO_UID := Admission.UID;
      Context.GPIO_UID_Kind := (case Admission.UID_Type is
         when G.No_UID => Wire.No_UID,
         when G.Integer_UID => Wire.Integer_UID,
         when G.String_UID => Wire.String_UID);
      Status := Stale_Observation;
      if not Resolver.Current (Service, Observation)
        or else not Core.Current_Node (Service, Admission_Device) then return; end if;
      Status := Builder_Refused;
      Builder.Build (Resolver.Facts (Observation), Context, Result);
      if Result.Status = Builder.Built then Status := Prepared; end if;
   end Build;
end ACPI_HID_Offer_Provenance;
