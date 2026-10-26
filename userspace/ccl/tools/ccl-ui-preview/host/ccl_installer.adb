--  The Linux preview has no installer.svc: nothing is ever installed from
--  it (a Linux-hosted stand-in, not CuBit).
package body CCL_Installer is
   procedure Disk_Info
     (Number : IP.Disk_Ordinal; Item : out IP.Disk_Record; Present : out Boolean;
      Result : out Result_Kind)
   is
      pragma Unreferenced (Number);
   begin
      Item := (others => <>);
      Present := False;
      Result := Unavailable;
   end Disk_Info;

   procedure Install
     (Request : IP.Install_Request; Outcome : out IP.Install_Outcome;
      Files, Bytes : out Interfaces.Unsigned_64; Proof : out IP.Identity_Proof;
      Result : out Result_Kind)
   is
      pragma Unreferenced (Request);
   begin
      Outcome := IP.Bad_Request;
      Files := 0;
      Bytes := 0;
      Proof := IP.Serial_Number;
      Result := Unavailable;
   end Install;
end CCL_Installer;
