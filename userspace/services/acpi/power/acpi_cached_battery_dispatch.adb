pragma Ada_2022;
package body ACPI_Cached_Battery_Dispatch is
   function Resolve (Selected : Codec.Selection; Value : ACPI_Cached_Battery_Values.View) return Reply is
      Result : Reply := (others => <>);
   begin
      case Selected.Kind is
         when Codec.Not_Handled => null;
         when Codec.Denied => Result.Status := Denied;
         when Codec.Malformed => Result.Status := Malformed;
         when Codec.Read_Inventory =>
            if Selected.Expected /= 0 and then Selected.Expected /= Value.Revision then
               Result.Status := Stale;
            elsif Selected.Slot > 0 and then Codec.Effective_Status (Value) = ACPI_Cached_Power_Values.Available
              and then Selected.Slot > Value.Count then
               Result.Status := Index_Out_Of_Range;
            else Result := (OK, Codec.Encode (Value, Selected.Page, Selected.Slot));
            end if;
      end case;
      return Result;
   end Resolve;
   procedure Dispatch (Owner : Owner_State; Stamp : ACPI_Protocol.Role_Tag;
      Request : Codec.Packet; Result : out Reply; Read_Allowed : Boolean) is
      Selected : constant Codec.Selection := Codec.Inspect (Request, Stamp);
      Value : ACPI_Cached_Battery_Values.View;
   begin
      if not Read_Allowed or else Selected.Kind /= Codec.Read_Inventory then
         Result := Resolve (Selected, (others => <>));
         return;
      end if;
      Read_Batteries (Owner, Selected.Maximum_Age, Value);
      Result := Resolve (Selected, Value);
   end Dispatch;
end ACPI_Cached_Battery_Dispatch;
