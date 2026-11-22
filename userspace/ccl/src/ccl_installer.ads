with CuBit.Installer_Protocol;

--  installer.svc for system.disks and system.install, one body per
--  platform: native/ asks the service; the Linux preview's host/ has no
--  installer (Unavailable), a Linux-hosted stand-in, not CuBit.
package CCL_Installer is
   package IP renames CuBit.Installer_Protocol;

   type Result_Kind is (Answered, Not_Granted, Unavailable);

   --  Disk Number's record; Present False when no disk has that number.
   procedure Disk_Info
     (Number : IP.Disk_Ordinal; Item : out IP.Disk_Record; Present : out Boolean;
      Result : out Result_Kind);

   --  Starts an install; the service answers at once.
   procedure Start
     (Request : IP.Install_Request; Started : out IP.Start_Result; Result : out Result_Kind);

   --  How the current or last install stands.
   procedure Status (Progress : out IP.Install_Progress; Result : out Result_Kind);
end CCL_Installer;
