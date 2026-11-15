pragma Ada_2022;
package body ACPI_Bootstrap.PCI_Observations with SPARK_Mode is
   procedure Read (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Selected : Reader.Member;
      Budget : Natural; Allowed : out Admission; Result : out Reader.Report) is
   begin
      Result := (others => <>); Allowed := Not_Ready;
      if not Device_Walk_Finished (Session) then return; end if;
      Allowed := Owner_Busy;
      if ACPI_Service.Retained_Results (Session.Core) /= 0 then return; end if;
      Allowed := Admitted;
      Reader.Read (Session.Core, Device, Selected, Budget, Result);
   end Read;
   procedure Identify_Root (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Admission; Result : out Reader.Root_Report) is
   begin
      Result := (others => <>); Allowed := Not_Ready;
      if not Device_Walk_Finished (Session) then return; end if;
      Allowed := Owner_Busy;
      if ACPI_Service.Retained_Results (Session.Core) /= 0 then return; end if;
      Allowed := Admitted;
      Reader.Identify_Root (Session.Core, Device, Budget, Result);
   end Identify_Root;
   procedure Observe_Controller (Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Budget : Natural;
      Allowed : out Admission; Result : out Reader.Topology_Report) is
   begin
      Result := (others => <>); Allowed := Not_Ready;
      if not Device_Walk_Finished (Session) then return; end if;
      Allowed := Owner_Busy;
      if ACPI_Service.Retained_Results (Session.Core) /= 0 then return; end if;
      Allowed := Admitted;
      Reader.Observe_Controller (Session.Core, Device, Budget, Result);
   end Observe_Controller;
end ACPI_Bootstrap.PCI_Observations;
