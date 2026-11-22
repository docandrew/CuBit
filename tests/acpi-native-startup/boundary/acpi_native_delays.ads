pragma Ada_2022;
with Interfaces;
with AML_Delays;
-- Trusted process-lifetime startup domain. Only the native owner may open it,
-- before AML executes. The observer server closes it before registration.
-- No wire operation or AML operand can open/replenish this deadline.
package ACPI_Native_Delays is
   procedure Begin_Startup (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Close_Startup;
   procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome);
end ACPI_Native_Delays;
