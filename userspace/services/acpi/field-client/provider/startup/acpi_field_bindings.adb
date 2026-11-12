pragma Ada_2022;
with AML_Decode;
with AML_Field_Plans;
with AML_Field_Datums;
with AML_Field_Targets;
with AML_Register_Views;
package body ACPI_Field_Bindings is
   package N renames Core.Namespace;
   package V renames Core.Values;
   package L renames AML_Field_Layout;
   package Plans renames AML_Field_Plans;
   package Datums renames AML_Field_Datums;
   package A renames AML_Field_Access;
   use type N.Object_Kind, N.Field_Organization, V.Access_Status;
   use type AML_Field_Targets.Vector, AML_Identity.Identity;
   Maximum_Direct_Bits : constant N.Encoded_Field_Bits := N.Encoded_Field_Bits (Plans.Maximum_Bits);
   function Observe (Run : Context) return Report is (Run.Info);
   procedure Build
     (Run : in out Context; Service : Core.State; IO : in out Provider.Context; Result : out Report)
   is
      Needed : array (N.Node_ID) of Boolean := [others => False];
      Items : Fields.Binding_Set;
      Node : Core.Node_Descriptor;
      Field : V.Field_Description;
      Key : V.Field_Key;
      Region : N.Operation_Region_Attributes;
      Geometry : L.Register_Geometry;
      Status : V.Access_Status;
      Target : A.Target;
      Accepted, Reading, Writing : Boolean;
      Last : constant N.Node_ID := Core.Namespace_Node_Count (Service);
      procedure Fail (At_Node : N.Node_ID; Why : Outcome; Detail : V.Access_Status) is
      begin
         Run.Info.Status := Why; Run.Info.Failed_Node := At_Node;
         Run.Info.Failure := Detail; Run.Stage := Failed; Result := Run.Info;
      end Fail;
      procedure Select_Datums
        (Plan : Plans.Plan; Requested_Region : L.Region_Geometry; For_Write : Boolean;
         Targets : out AML_Field_Targets.Vector; Good : out Boolean)
      is
         Allowed : Boolean;
      begin
         Targets := [others => A.No_Target]; Good := False;
         for Position in Plans.Datum_Position range 1 .. Plans.Count (Plan) loop
            declare Datum : constant Datums.Datum := Plans.Datum_At (Plan, Position); begin
               Provider.Select_Target (IO, (Requested_Region,
                 AML_Decode.Integer_Value (Datum.Offset), Datum.Access_Width),
                 For_Write, Targets (Position), Allowed);
               if not Allowed then return; end if;
               if For_Write and then Datums.Needs_Read (Datum.Access_Width, Datum.Mask, Plans.Rule (Plan)) then
                  Provider.Authorize (IO, Targets (Position), False, Allowed);
                  if not Allowed then return; end if;
               end if;
            end;
         end loop;
         Good := True;
      end Select_Datums;
   begin
      Result := (Status => Already_Consumed, others => <>);
      if Run.Stage /= Fresh then return; end if;
      Run.Stage := Building;
      if not Provider.Ready (IO) then
         Fail (N.Root, Provider_Unavailable, V.Wrong_Phase); return;
      elsif Provider.Pending (IO) then
         Fail (N.Root, Pending_IO, V.Busy); return;
      end if;
      Core.Describe_Node (Service, N.Root, Run.Root, Status);
      if Status /= V.Available then Fail (N.Root, Namespace_Unavailable, Status); return; end if;
      Run.Provider_ID := Provider.Identity (IO); Fields.Clear (Items);
      -- Composed bank/wide fields need region entries. Indexed fields obtain
      -- their source registers through ordinary (or nested composed) fields.
      -- Do not spend the region quota on unrelated large metadata extents.
      for Index in N.Root .. Last loop
         Core.Describe_Node (Service, Index, Node, Status);
         if Status = V.Available and then V.Node_Kind (Node) = N.Region_Field_Object then
            Core.Read_Field_Descriptor (Service, Index, Field, Key, Status);
            if Status = V.Available and then
              (Field.Field.Organization = N.Banked_Region_Field
               or else (Field.Field.Organization = N.Direct_Region_Field and then Field.Field.Bits > Maximum_Direct_Bits))
            then Needed (Field.Field.Region) := True; end if;
         end if;
      end loop;
      for Index in N.Root .. Last loop
         if Needed (Index) then
            Core.Describe_Node (Service, Index, Node, Status);
            if Status /= V.Available then Fail (Index, Namespace_Unavailable, Status); return; end if;
            Core.Read_Region_Descriptor (Service, Node, Region, Status);
            if Status /= V.Available then Fail (Index, Namespace_Unavailable, Status); return; end if;
            for Ordinal in 1 .. Provider.Register_Count (IO) loop
               Provider.Export_Target (IO, Ordinal, Target, Accepted);
               if not Accepted then Fail (Index, Binding_Refused, V.Invalid_Value); return; end if;
               Provider.Describe (IO, Target, Geometry, Accepted);
               if not Accepted then Fail (Index, Binding_Refused, V.Invalid_Value); return; end if;
               if AML_Register_Views.Can_Rebase (Geometry,
                 (AML_Decode.Byte (Region.Space), Region.Address, Region.Length, Region.PCI)) then
                  Provider.Authorize (IO, Target, False, Reading);
                  Provider.Authorize (IO, Target, True, Writing);
                  if Reading or Writing then
                     Fields.Add_Region_Register (Service, IO, Node, Target, Reading, Writing, Items, Status);
                     if Status /= V.Available then Fail (Index, Binding_Refused, Status); return; end if;
                     Run.Info.Region_Entries := Run.Info.Region_Entries + 1;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      for Index in N.Root .. Last loop
         Core.Describe_Node (Service, Index, Node, Status);
         if Status = V.Available and then V.Node_Kind (Node) = N.Region_Field_Object then
            Core.Read_Field_Descriptor (Service, Index, Field, Key, Status);
            if Status /= V.Available then
               -- Indexed fields intentionally have no region Field_Key in
               -- this API. They are not given synthetic direct bindings.
               Run.Info.Unkeyed_Fields := Run.Info.Unkeyed_Fields + 1;
            elsif Field.Field.Organization = N.Direct_Region_Field and then Field.Field.Bits <= Maximum_Direct_Bits then
               Run.Info.Direct_Fields := Run.Info.Direct_Fields + 1;
               declare
                  Meta : constant L.Field_Geometry :=
                    (Region => (AML_Decode.Byte (Field.Region.Space), Field.Region.Address, Field.Region.Length, Field.Region.PCI),
                     Bit_Offset => AML_Decode.Integer_Value (Field.Field.Offset),
                     Bit_Length => AML_Decode.Integer_Value (Field.Field.Bits),
                     Flags => Field.Field.Raw_Flags, Effective_Access => Field.Field.Access_Type,
                     Attribute => Field.Field.Attribute, Access_Length => Field.Field.Access_Length, others => <>);
                  Plan : constant Plans.Plan := Plans.Prepare (Meta, Provider.Supports_Locked_NVS_Reads, Provider.Supports_Whole_Field_Locks);
                  Read_Targets, Write_Targets : AML_Field_Targets.Vector;
                  Can_Read, Can_Write, Any_Readable : Boolean;
               begin
                  if not Plans.Valid (Plan) then
                     Run.Info.Unsupported_Fields := Run.Info.Unsupported_Fields + 1;
                  else
                     Select_Datums (Plan, Meta.Region, False, Read_Targets, Can_Read);
                     Select_Datums (Plan, Meta.Region, True, Write_Targets, Can_Write);
                     if Can_Read and then Can_Write and then Read_Targets /= Write_Targets then
                        Run.Info.Direction_Conflicts := Run.Info.Direction_Conflicts + 1;
                        Can_Write := False;
                     elsif not Can_Read and Can_Write then
                        Any_Readable := False;
                        for Position in Plans.Datum_Position range 1 .. Plans.Count (Plan) loop
                           Provider.Authorize (IO, Write_Targets (Position), False, Accepted);
                           Any_Readable := Any_Readable or Accepted;
                        end loop;
                        if Any_Readable then
                           Run.Info.Direction_Conflicts := Run.Info.Direction_Conflicts + 1;
                           Can_Write := False;
                        end if;
                     end if;
                     if Can_Write then
                        Fields.Add_Write_Binding (Service, IO, Key, Write_Targets, Plans.Count (Plan), Items, Status);
                     elsif Can_Read then
                        Fields.Add_Read_Binding (Service, IO, Key, Read_Targets, Plans.Count (Plan), Items, Status);
                     end if;
                     if Can_Read or Can_Write then
                        if Status /= V.Available then Fail (Index, Binding_Refused, Status); return; end if;
                        if Can_Read then Run.Info.Read_Fields := Run.Info.Read_Fields + 1; end if;
                        if Can_Write then Run.Info.Write_Fields := Run.Info.Write_Fields + 1; end if;
                     else Run.Info.Missing_Fields := Run.Info.Missing_Fields + 1; end if;
                  end if;
               end;
            end if;
         end if;
      end loop;
      Run.Items := Items; Run.Stage := Published;
      Run.Info.Status := (if Run.Info.Missing_Fields = 0 and then Run.Info.Unsupported_Fields = 0
        and then Run.Info.Unkeyed_Fields = 0 and then Run.Info.Direction_Conflicts = 0
        then Built else Built_With_Gaps);
      Result := Run.Info;
   end Build;
   procedure Copy_Bindings
     (Run : Context; Service : Core.State; IO : Provider.Context;
      Items : out Fields.Binding_Set; Accepted : out Boolean) is
   begin
      Fields.Clear (Items);
      Accepted := Run.Stage = Published and then Run.Provider_ID = Provider.Identity (IO)
        and then Core.Current_Node (Service, Run.Root);
      if Accepted then Items := Run.Items; end if;
   end Copy_Bindings;
end ACPI_Field_Bindings;
