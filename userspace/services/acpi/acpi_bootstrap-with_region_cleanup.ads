pragma Ada_2022;
generic
   -- Trusted serialized shutdown owner only; never a runtime enrollment path.
   -- Step disconnects already connected groups on the same Core, preserving
   -- catalog/identity and all other owners. Caller supplies an existing finite
   -- I/O window and has stopped device evaluation. No reset/replay/upward IPC.
   with procedure Step (Service : aliased in out ACPI_Service.State; Completed : out Boolean);
package ACPI_Bootstrap.With_Region_Cleanup with SPARK_Mode is
   type Admission is (Not_Ready,Notifications_Pending,Results_Pending,Admitted);
   -- Admission only opens the shutdown path. Step must consult the owning
   -- region session: known Prepared successes may disconnect; failed/unknown
   -- notification effects must remain retained. No outcome is promoted here.
   procedure Advance (Session : in out State; Allowed : out Admission; Completed : out Boolean)
      with Pre=>Valid(Session),Post=>Valid(Session);
end ACPI_Bootstrap.With_Region_Cleanup;
