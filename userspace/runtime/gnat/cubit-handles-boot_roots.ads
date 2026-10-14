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
------------------------------------------------------------------------------
pragma Ada_2022;
with Launch_Configuration;

package CuBit.Handles.Boot_Roots is

   Maximum_Device_Roots : constant := Launch_Configuration.Maximum_Device_Roots;
   subtype Root_Count is Natural range 0 .. Maximum_Device_Roots;
   subtype Root_Index is Positive range 1 .. Maximum_Device_Roots;
   type Device_Roots is array (Root_Index) of Device_Memory;

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
      Count             : Root_Count := 0;
      Memory            : Device_Roots := [others => No_Device_Memory];
   end record;

   --  devmgr's roots, from its launch configuration.
   function Get return Roots;

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
   end record;

   function Process_Manager return Launcher_Handles;

end CuBit.Handles.Boot_Roots;
