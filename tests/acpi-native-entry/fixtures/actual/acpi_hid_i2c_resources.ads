pragma Ada_2022;
with ACPI_Resource_Data;
with ACPI_Resource_Data.Serial_Connections;
with ACPI_Resource_Data.GPIO_Connections;
with Firmware_Tables;
-- Descriptive association within one exact copied _CRS, never admission.
package ACPI_HID_I2C_Resources with Pure, SPARK_Mode is
   package R renames ACPI_Resource_Data;
   package S renames R.Serial_Connections;
   package G renames R.GPIO_Connections;
   use type R.Item_Count;
   use type R.Template_Status;
   use type S.Decode_Status;
   use type G.Decode_Status;
   use type S.Bus_Kind;
   use type G.Connection_Kind;
   type Analysis_Outcome is (Connections_Found, Invalid_Template,
      Missing_I2C, Missing_Interrupt, Ambiguous_Connections,
      Invalid_Connection, Dependent_Alternatives);
   type Analysis_Report is record
      Outcome : Analysis_Outcome := Invalid_Template;
      Framing : R.Scan_Result := (others => <>);
      Position : R.Byte_Offset := 0;
      I2C_Count, Interrupt_Count, Other_Items : R.Item_Count := 0;
      I2C_Offset, Interrupt_Offset : R.Byte_Offset := 0;
      I2C : S.Decode_Result := (Status => S.Unsupported_Descriptor);
      Interrupt : G.Decode_Result := (Status => G.Unsupported_Descriptor);
   end record;
   -- Full framing/checksum/item quota precedes association. Require exactly
   -- one I2C connection and one GPIO interrupt; preserve first observations
   -- plus counts for diagnostics on ambiguous layouts. Do not choose a resource
   -- alternative. Unrelated framed resources are counted, not admitted.
   -- Spans refer only to Data: keep this exact copy while consuming them.
   -- Caller separately establishes identity, _STA, initialization/dependencies,
   -- controller paths, ownership and hardware rights. No readiness is implied.
   procedure Analyze (Data : Firmware_Tables.Bytes; Report : out Analysis_Report;
      Maximum_Items : R.Item_Count := R.Default_Item_Limit)
     with Global => null,
       Post => Report.I2C_Count <= Maximum_Items
         and then Report.Interrupt_Count <= Maximum_Items
         and then Report.Other_Items <= Maximum_Items
         and then (if Report.Outcome = Connections_Found then
           Report.Framing.Status = R.Complete
           and then Report.I2C_Count = 1 and then Report.Interrupt_Count = 1
           and then Report.I2C.Status = S.Decoded
           and then Report.I2C.Information.Kind = S.I2C_Bus
           and then Report.Interrupt.Status = G.Decoded
           and then Report.Interrupt.Information.Kind = G.Interrupt_Connection);
end ACPI_HID_I2C_Resources;
