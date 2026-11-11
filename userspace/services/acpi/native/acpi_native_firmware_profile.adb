pragma Ada_2022;
with Parameters;
with ACPI_Native_Log;
package body ACPI_Native_Firmware_Profile is
   package P renames ACPI_Firmware_Compatibility;
   function From_Declaration return P.Profile is
   begin
      -- Exhaustive bridge between two generated nominal enum types.
      -- Extending the launch declaration requires an explicit policy mapping.
      case Parameters.Value.kind is
         when Parameters.Neutral => return P.Declared.Neutral;
         when Parameters.Known_Windows => return P.Declared.Known_Windows;
      end case;
   end From_Declaration;
   Chosen : constant P.Profile := From_Declaration;
   Supported : constant AML_Interface_Policy.Policy := P.Policy_For (Chosen);
   Maximum_Shown_Bytes : constant Positive := 80;
   procedure Log_Selected is
   begin
      ACPI_Native_Log.Info ("acpi: stage firmware-profile ok profile=" & P.Name (Chosen) &
        " interfaces=" & AML_Interface_Policy.Count (Supported)'Image);
   end Log_Selected;
   procedure Observe_Query (Name : AML_Decode.Bytes; Supported, Truncated : Boolean) is
      Shown : constant Natural := Natural'Min (Name'Length, Maximum_Shown_Bytes);
      Text : String (1 .. Shown);
      use type AML_Decode.Byte;
   begin
      for I in Text'Range loop
         declare B : constant AML_Decode.Byte := Name (Name'First + I - 1); begin
            Text (I) := (if B in 32 .. 126 and then B /= Character'Pos ('"')
                         and then B /= Character'Pos ('\') then Character'Val (B) else '?');
         end;
      end loop;
      ACPI_Native_Log.Info ("acpi: stage osi-query ok name=""" & Text & """ answer=" &
        Boolean'Image (Supported) & " bytes=" & Natural'Image (Name'Length) &
        " prefix=" & Boolean'Image (Truncated) &
        " log-clipped=" & Boolean'Image (Shown < Name'Length));
   end Observe_Query;
   function Selected return P.Profile is (Chosen);
   function Interfaces return AML_Interface_Policy.Policy is (Supported);
end ACPI_Native_Firmware_Profile;
