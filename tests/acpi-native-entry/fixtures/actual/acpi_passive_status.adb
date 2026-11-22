pragma Ada_2022;
with Firmware_Tables;
package body ACPI_Passive_Status with SPARK_Mode is
   use type Core.Values.Access_Status;
   use type Reader.Outcome;
   use type Reader.Atom_Kind;
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor;
      Result : out Fields.Field_Result; Observation : out Reader.Report) is
      Data : Firmware_Tables.Bytes (1 .. 0);
      Items : Reader.Atoms (1 .. 1);
      Item : Fields.Field_Record := (Name => Fields.Device_Status, others => <>);
   begin
      Result := (Accepted => False);
      Reader.Read (Service, Device, Reader.Device_Status, Data, Items, Observation);
      if Observation.Access_Result /= Core.Values.Available
        or else Observation.Cleanup_Result /= Core.Values.Available then return; end if;
      if Observation.Status = Reader.Static_Value then
         if not Observation.Size_Known or else Observation.Required_Atoms /= 1
           or else Observation.Copied_Atoms /= 1 or else Observation.Required_Bytes /= 0
           or else Observation.Copied_Bytes /= 0 or else Items (1).Kind /= Reader.Integer_Atom
           or else Items (1).Offset /= 0 or else Items (1).Length /= 0
         then return; end if;
         Item.Status := ACPI_Passive_Query.Present;
         Item.Kind := Fields.Integer_Value;
         Item.Number := ACPI_Passive_Query.Word (Items (1).Number);
      elsif Observation.Status in Reader.Missing | Reader.Method_Valued | Reader.Unsupported then
         if Observation.Size_Known or else Observation.Required_Atoms /= 0
           or else Observation.Copied_Atoms /= 0 or else Observation.Required_Bytes /= 0
           or else Observation.Copied_Bytes /= 0 or else Items (1).Kind /= Reader.No_Atom
         then return; end if;
         case Observation.Status is
            when Reader.Missing =>
               Item.Status := ACPI_Passive_Query.Absent;
               Item.Kind := Fields.Integer_Value;
               Item.Number := ACPI_Passive_Query.Default_STA;
               Item.Defaulted := True;
            when Reader.Method_Valued => Item.Status := ACPI_Passive_Query.Method_Valued;
            when Reader.Unsupported => Item.Status := ACPI_Passive_Query.Unsupported;
            when others => return;
         end case;
      else return;
      end if;
      if Fields.Valid (Item, ACPI_Passive_Query.Node_Limit (Core.Max_Namespace_Nodes), 0) then
         Result := (True, Item);
      end if;
   end Read;
end ACPI_Passive_Status;
