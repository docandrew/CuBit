------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Boot_Roots - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Kernel;
with CuBit.Launch_Configurations;

package body CuBit.Handles.Boot_Roots is

   package K renames CuBit.Handles.Kernel;

   function Get return Roots is
      Configuration : Launch_Configuration.Device_Manager_Configuration;
      Valid : Boolean;
      Result : Roots;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Valid);
      if not Valid then
         return Result;
      end if;
      declare
         H : Launch_Configuration.Device_Manager_Handles renames Configuration.Handles;
      begin
         Result :=
           (Valid             => True,
            Self              => K.To_Endpoint (H.Self),
            Self_Process      => K.To_Process (H.Self_Process),
            Self_Grantable    => K.To_Endpoint (H.Self_Grantable),
            Process_Root      => K.To_Process (H.Process_Root),
            PCI_Ports         => K.To_IO_Ports (H.PCI_Ports),
            Registration      => K.To_Notification (H.Registration),
            Vector_Authority  => K.To_Interrupt (H.Vector_Authority),
            IO_Ports_Root     => K.To_IO_Ports (H.IO_Ports),
            Legacy_Interrupts => K.To_Interrupt (H.Legacy_Interrupts),
            Notifications     => K.To_Notification (H.Notifications),
            Scheduling_Root   => K.To_Scheduling (H.Scheduling),
            Count             => H.Root_Count,
            Memory            =>
              [for R in Root_Index => K.To_Device_Memory (H.Device_Roots (R))]);
      end;
      return Result;
   end Get;

   function Process_Manager return Launcher_Handles is
      Configuration : Launch_Configuration.Process_Manager_Configuration;
      Valid : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Valid);
      if not Valid then
         return (others => <>);
      end if;
      declare
         H : Launch_Configuration.Process_Manager_Handles renames Configuration.Handles;
      begin
         return
           (Valid          => True,
            Self           => K.To_Endpoint (H.Self),
            Self_Process   => K.To_Process (H.Self_Process),
            Self_Grantable => K.To_Endpoint (H.Self_Grantable),
            Process_Root   => K.To_Process (H.Process_Root),
            Registration   => K.To_Notification (H.Registration),
            Notifications  => K.To_Notification (H.Notifications),
            Filesystem     => K.To_Endpoint (H.Filesystem),
            Config         => K.To_Endpoint (H.Config),
            Netstack       => K.To_Endpoint (H.Netstack),
            Network_Policy => K.To_Endpoint (H.Network_Policy),
            Mixer          => K.To_Endpoint (H.Mixer),
            Device_Manager => K.To_Endpoint (H.Device_Manager),
            Ready          => K.To_Endpoint (H.Ready),
            Framebuffer    => K.To_Device_Memory (H.Framebuffer),
            Device_Inspection => K.To_Endpoint (H.Device_Inspection));
      end;
   end Process_Manager;

end CuBit.Handles.Boot_Roots;
