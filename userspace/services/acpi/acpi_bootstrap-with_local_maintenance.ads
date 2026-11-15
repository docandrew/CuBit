pragma Ada_2022;
generic
   -- Trusted serialized process owner only. Preserve valid Core/catalog and
   -- all outstanding provider/result ownership. No reset, reimport, reentry,
   -- persistent Core alias or upward/peer synchronous IPC. Caller supplies an
   -- existing finite local runtime window whenever Step may execute AML/IO.
   with procedure Step (Service : aliased in out ACPI_Service.State; Completed : out Boolean);
package ACPI_Bootstrap.With_Local_Maintenance with SPARK_Mode is
   type Admission is (Not_Ready,Notifications_Pending,Results_Pending,Admitted);
   -- Deliberately no wire selector/label. Used for fresh native controller
   -- enrollment and normal region disconnect on the SAME private Core.
   procedure Advance (Session : in out State; Allowed : out Admission; Completed : out Boolean)
     with Pre => Valid(Session), Post => Valid(Session);
end ACPI_Bootstrap.With_Local_Maintenance;
