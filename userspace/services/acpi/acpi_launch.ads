pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_Requests;
with ACPI_Endpoint;
-- Private launcher-to-service protocol. Knowing Bootstrap_Tag grants nothing:
-- only the trusted process manager may mint this tag on an ACPI endpoint.
-- The launcher must install Provider_Slot bound to the immutable table provider
-- before sending configuration. The decoder cannot verify that cspace binding.
package ACPI_Launch with SPARK_Mode is
   Bootstrap_Tag : constant Unsigned_64 := 16#4143_5049_424F_4F54#;
   Configure : constant Unsigned_32 := 16#4143#;
   subtype Provider_Slot_Number is Unsigned_64 range 0 .. 62;
   type Decision (Accepted : Boolean := False) is record
      case Accepted is
         when True =>
            Config : ACPI_Endpoint.Configuration;
            Provider_Slot : Provider_Slot_Number;
         when False => null;
      end case;
   end record;
   -- [observer tag, provider tag, provider capability slot, zero]. No PID,
   -- address or caller-written packet field can supply the authenticated stamp.
   function Decode (Stamp : Unsigned_64; Request : ACPI_Requests.Packet)
     return Decision with
     Post => (if Decode'Result.Accepted then Stamp = Bootstrap_Tag
       and then ACPI_Endpoint.Valid (Decode'Result.Config)
       and then Decode'Result.Config.Observer_Tag /= Bootstrap_Tag
       and then Decode'Result.Config.Provider_Tag /= Bootstrap_Tag
       and then Decode'Result.Config.Observer_Tag = Request.Data (0)
       and then Decode'Result.Config.Provider_Tag = Request.Data (1)
       and then Decode'Result.Provider_Slot = Request.Data (2));
end ACPI_Launch;
