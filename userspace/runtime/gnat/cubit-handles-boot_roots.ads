------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The device manager's roots (CAP-002): the handles the kernel placed in
--  devmgr's table before it ran, reported in its typed launch configuration
--  (Launch_Configuration.Device_Manager_Handles). Every capability devmgr
--  hands out is a narrowed copy of one of these, or a kernel object SPAWN
--  gave it. Only devmgr has them; for any other process Valid is False.
--
--  The roots are as many as the kernel found (LIM-001b): Get reports how
--  many, and Device_Root / Firmware_Root read each from the launch block.
------------------------------------------------------------------------------
pragma Ada_2022;
with Launch_Configuration;

package CuBit.Handles.Boot_Roots is

   --  The device-memory roots, ascending, then the firmware-memory roots
   --  (Capability_ABI.Firmware_*_Tag: ACPI NVS runs, the FACS page, the PCI
   --  configuration slice, then table-named firmware data).
   subtype Root_Count is Natural range 0 .. Launch_Configuration.Maximum_Roots;
   subtype Root_Index is Positive range 1 .. Launch_Configuration.Maximum_Roots;

   type Roots is record
      Valid             : Boolean := False;
      Self              : Endpoint;
      Self_Process      : Process;
      Self_Grantable    : Endpoint;       --  untagged, may badge and pass on
      Process_Root      : Process;        --  spawn and control, every process
      PCI_Ports         : IO_Ports;
      Registration      : Notification;   --  its own role (DRIVER_DEVMGR)
      Vector_Authority  : Interrupt;
      IO_Ports_Root     : IO_Ports;       --  the I/O port space
      Legacy_Interrupts : Interrupt;      --  the legacy lines
      Notifications     : Notification;   --  every service role
      Scheduling_Root   : Scheduling;
      Firmware_Snapshot_Root : Firmware_Snapshot;  --  READ and GRANT
      Count             : Root_Count := 0;
      Firmware_Count    : Root_Count := 0;
   end record;

   --  devmgr's roots, from its launch configuration.
   function Get return Roots;

   --  Device-memory root I and firmware-memory root I of R.
   function Device_Root (R : Roots; I : Root_Index) return Device_Memory
   with Pre => R.Valid and then I <= R.Count;
   function Firmware_Root (R : Roots; I : Root_Index) return Device_Memory
   with Pre => R.Valid and then I <= R.Firmware_Count;

   --  procmgr's handles, installed by devmgr (Launch_Configuration.
   --  Process_Manager_Handles); Valid False in any other process.
   type Launcher_Handles is record
      Valid          : Boolean := False;
      Self           : Endpoint;
      Self_Process   : Process;
      Self_Grantable : Endpoint;       --  untagged: badge copies for clients
      Process_Root   : Process;        --  spawn and control
      Registration   : Notification;   --  its own role (DRIVER_PROCMGR)
      Notifications  : Notification;   --  the roles it hands children
      Filesystem     : Endpoint;       --  untagged, Grant
      Config         : Endpoint;       --  untagged, Grant
      Netstack       : Endpoint;       --  untagged, Grant (scope endpoints)
      Network_Policy : Endpoint;       --  netstack's policy authority
      Mixer          : Endpoint;       --  untagged, Grant; may be null
      Device_Manager : Endpoint;       --  devmgr's launcher (render) broker
      Ready          : Endpoint;       --  devmgr's "ready" endpoint
      Framebuffer    : Device_Memory;  --  the boot framebuffer; may be null
      Device_Inspection : Endpoint;    --  devmgr, untagged, Grant (inventory)
      Firmware_Snapshot_Reader : Firmware_Snapshot;  --  READ and GRANT
   end record;

   function Process_Manager return Launcher_Handles;

end CuBit.Handles.Boot_Roots;
