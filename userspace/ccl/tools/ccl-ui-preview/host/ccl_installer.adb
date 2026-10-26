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

   procedure Start
     (Request : IP.Install_Request; Started : out IP.Start_Result; Result : out Result_Kind)
   is
      pragma Unreferenced (Request);
   begin
      Started := IP.Rejected;
      Result := Unavailable;
   end Start;

   procedure Status (Progress : out IP.Install_Progress; Result : out Result_Kind) is
   begin
      Progress := (others => <>);
      Result := Unavailable;
   end Status;
end CCL_Installer;
