pragma Ada_2022;
package body ACPI_Cached_Power_Dispatch is
   use type ACPI_Cached_Power_Values.Generation;
   procedure Dispatch (Server : Owner_State; Stamp : ACPI_Protocol.Role_Tag;
      Request : Codec.Packet; Result : out Reply; Read_Allowed : Boolean) is
      Selected : constant Codec.Selection := Codec.Inspect (Request, Stamp);
      Value : ACPI_Cached_Power_Values.Snapshot;
   begin
      Result := (others => <>);
      Value := (others => <>);
      case Selected.Kind is
         when Codec.Not_Handled => null;
         when Codec.Denied => Result.Status := Denied;
         when Codec.Malformed => Result.Status := Malformed;
         when Codec.Read_Lid =>
            if Read_Allowed then Read_Power (Server, Selected.Maximum_Age, Value); end if;
            if Selected.Expected /= 0 and then Selected.Expected /= Value.Lid.Revision then
               Result.Status := Stale;
            else Result := (OK, Codec.Encode (Value.Lid, Selected.Page)); end if;
         when Codec.Read_Source =>
            if Read_Allowed then Read_Power (Server, Selected.Maximum_Age, Value); end if;
            if Selected.Expected /= 0 and then Selected.Expected /= Value.Source.Revision then
               Result.Status := Stale;
            else Result := (OK, Codec.Encode (Value.Source, Selected.Page)); end if;
      end case;
   end Dispatch;
end ACPI_Cached_Power_Dispatch;
