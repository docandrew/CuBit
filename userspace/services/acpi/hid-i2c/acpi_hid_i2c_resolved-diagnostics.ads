generic
   -- Pass the actual logstore text capacity at native instantiation. Default
   -- matches the current sink; no logger change or silent truncation here.
   Maximum_Line_Bytes : Positive := 512;
package ACPI_HID_I2C_Resolved.Diagnostics with SPARK_Mode is
   type Outcome is (Facts_Formatted, Discovery_Failed, No_Observation,
                    Stale_Binding, Invalid_Source_Text, Log_Capacity_Refused);
   subtype Text_Length is Natural range 0 .. Maximum_Line_Bytes;
   type Line is record
      Status : Outcome := No_Observation;
      Length : Text_Length := 0;
      Data : String (1 .. Maximum_Line_Bytes) := [others => ' '];
   end record;
   function Image (Item : Line) return String is (Item.Data (1 .. Item.Length));
   -- One printable ASCII stage line for existing logstore/serial sinks. Values
   -- come only from a complete observation with current descriptive bindings.
   -- No discovery/query, retry, IO, logging or authority grant occurs here.
   -- "ok" qualifies observed resource facts, never HID/input readiness.
   -- Capacity refusal returns a failure line if it fits, otherwise empty text
   -- with Log_Capacity_Refused. Never publish an incomplete success line.
   function Format (Service : Core.State; Item : Report) return Line
      with Pre => Core.Valid (Service);
end ACPI_HID_I2C_Resolved.Diagnostics;
