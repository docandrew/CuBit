with Interfaces;
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

   procedure Install
     (Request : IP.Install_Request; Outcome : out IP.Install_Outcome;
      Files, Bytes : out Interfaces.Unsigned_64; Result : out Result_Kind);
end CCL_Installer;
