------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Installing CuBit onto one disk (SYS-001 installer v2, SYS-002, SYS-003):
--  the install as a library any holder of the disk-install authority can
--  run (installer.svc today; the graphical Installer app next), so the
--  logic lives here and not in a service.
--
--  @description
--  Run checks the request (the confirmation must name the disk's model,
--  serial and geometry exactly) and the payload (Install_Sources), then
--  takes Install_Steps in order through Layout_Writer: partition (GPT:
--  FAT32 ESP + ext2 root), format the root, fill it and its storehouse
--  (every component by SHA-256), write generation 1 and make it current,
--  read the root back, write the ESP, read it back, flush, and only then
--  write the ESP's boot sector: the disk is bootable from that last write
--  on, never before. A failed or interrupted run leaves an unbootable disk
--  and is restarted from the beginning (nothing on the disk is trusted).
--
--  Every request has an explicit deadline (IPC-000 rule 6, the 2026-11-06
--  laptop hang): Device_Request_Deadline_Ms toward the disk's driver,
--  Install_Sources.Request_Deadline_Ms toward the filesystem,
--  Service_Deadline_Ms toward devmgr and the clock. A request past its
--  deadline ends the install with a named outcome (Device_Timed_Out,
--  Source_Timed_Out) and the log line names the server and the request.
--
--  Serve is called between device requests so the holder keeps answering
--  its own clients (status queries) while an install runs; Log records
--  each step.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Block_Devices;
with CuBit.Device_Identities;
with CuBit.Handles;
with CuBit.Installer_Protocol;

generic
   with procedure Serve;
   with procedure Log (Text : String);
package System_Install is
   package IP renames CuBit.Installer_Protocol;

   --  Above the drivers' own 30 s command timeout (UFS, NVMe), so a late
   --  answer is heard and a lost one ends the install.
   Device_Request_Deadline_Ms : constant := 40_000;
   --  devmgr (Open_Disk) and clock.svc answer at once.
   Service_Deadline_Ms : constant := 10_000;

   type Endpoints is record
      Disk_Install : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Filesystem   : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Clock        : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;

   procedure Initialize (Through : Endpoints);

   --  Opens disk Number read-only and describes and identifies it.
   procedure Look
     (Number : IP.Disk_Ordinal; Found : out Boolean;
      Description : out CuBit.Block_Devices.Device_Description;
      Identity : out CuBit.Device_Identities.Identity; Known : out Boolean);

   --  Installs Item. Progress tells how far it is meanwhile (from Serve).
   procedure Run (Item : IP.Install_Request; Outcome : out IP.Install_Outcome; Proof : out IP.Identity_Proof);

   function Running return Boolean;
   --  The current or last install: state, step, files and bytes.
   function Progress return IP.Install_Progress;

   --  Stable names for the log.
   function Outcome_Name (Item : IP.Install_Outcome) return String;
end System_Install;
