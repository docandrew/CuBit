pragma Ada_2022;
with CuBit.Handles.Kernel;
with CuBit.Launch_Block;
pragma Elaborate_All (CuBit.Launch_Block);

package body CuBit.Handles.Startup with SPARK_Mode => Off is
   package K renames CuBit.Handles.Kernel;
   --  Withed for its elaboration: the startup table is loaded first.
   Loaded_First : constant Boolean := CuBit.Launch_Block.Table_Present;
   pragma Unreferenced (Loaded_First);

   function Endpoint_Named (Label : String; Kind : ST.Capability_Kind := ST.Endpoint)
     return Endpoint is
     (K.To_Endpoint (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Endpoint_Named (Label : String; Kind : ST.Capability_Kind := ST.Endpoint)
     return Optional_Endpoint
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Endpoint (Found));
   end Optional_Endpoint_Named;

   function Device_Memory_Named (Label : String; Kind : ST.Capability_Kind := ST.Device_Memory)
     return Device_Memory is
     (K.To_Device_Memory (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Device_Memory_Named (Label : String; Kind : ST.Capability_Kind := ST.Device_Memory)
     return Optional_Device_Memory
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Device_Memory (Found));
   end Optional_Device_Memory_Named;

   function IO_Ports_Named (Label : String; Kind : ST.Capability_Kind := ST.IO_Ports)
     return IO_Ports is
     (K.To_IO_Ports (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_IO_Ports_Named (Label : String; Kind : ST.Capability_Kind := ST.IO_Ports)
     return Optional_IO_Ports
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_IO_Ports (Found));
   end Optional_IO_Ports_Named;

   function Interrupt_Named (Label : String; Kind : ST.Capability_Kind := ST.Interrupt)
     return Interrupt is
     (K.To_Interrupt (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Interrupt_Named (Label : String; Kind : ST.Capability_Kind := ST.Interrupt)
     return Optional_Interrupt
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Interrupt (Found));
   end Optional_Interrupt_Named;

   function Notification_Named (Label : String; Kind : ST.Capability_Kind := ST.Notification)
     return Notification is
     (K.To_Notification (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Notification_Named (Label : String; Kind : ST.Capability_Kind := ST.Notification)
     return Optional_Notification
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Notification (Found));
   end Optional_Notification_Named;

   function Scheduling_Named (Label : String; Kind : ST.Capability_Kind := ST.Scheduling)
     return Scheduling is
     (K.To_Scheduling (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Scheduling_Named (Label : String; Kind : ST.Capability_Kind := ST.Scheduling)
     return Optional_Scheduling
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Scheduling (Found));
   end Optional_Scheduling_Named;

   function Process_Named (Label : String; Kind : ST.Capability_Kind := ST.Process)
     return Process is
     (K.To_Process (CuBit.Startup_State.Granted (Label, Kind)));

   function Optional_Process_Named (Label : String; Kind : ST.Capability_Kind := ST.Process)
     return Optional_Process
   is
      Found : constant Unsigned_64 := CuBit.Startup_State.Granted (Label, Kind);
   begin
      if Found = ST.No_Handle then
         return (Present => False);
      end if;
      return (Present => True, Value => K.To_Process (Found));
   end Optional_Process_Named;

end CuBit.Handles.Startup;
