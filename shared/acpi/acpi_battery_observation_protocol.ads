pragma Ada_2022;
with Interfaces;
-- Read-only observer request [page,maximum-age-us,selector,expected-generation].
-- Slot numbers are positions within one inventory generation, never authority
-- or stable device identities. Auxiliary and entry reads require a generation.
package ACPI_Battery_Observation_Protocol with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   Read_Metrics_Label : constant Interfaces.Unsigned_32 := 0;
   Battery_Page : constant Interfaces.Unsigned_64 := 22;
   Selector_Stride : constant Interfaces.Unsigned_64 := 16;
   type Subpage is (Inventory, Sample_Time, Entry_Summary, Information,
      Capacities, Design_Voltage, Dynamic_State, Rate_And_Capacity, Dynamic_Voltage);
   -- Selector = slot*stride + Subpage'Pos(page). Slot0 permits the first two
   -- pages; positive slots permit only entry pages. All gaps are malformed.
   -- Replies are four signed-CCL-representable words. Except Sample_Time,
   -- [generation,availability,payload0,payload1]. Entry summary retains the
   -- inventory availability and carries entry status/reason. Other entry
   -- pages qualify availability with Missing/Failure for absent/failed entries
   -- and carry no payload; zero cannot impersonate a known measured zero.
   -- Sample_Time: [generation,time-known,time-low32,time-high32].
   Unknown_Quantity : constant Interfaces.Unsigned_64 := 2 ** Interfaces.Unsigned_32'Size;
   Critical_Flag : constant Interfaces.Unsigned_64 := 1;
   Charge_Limiting_Flag : constant Interfaces.Unsigned_64 := 2;
end ACPI_Battery_Observation_Protocol;
