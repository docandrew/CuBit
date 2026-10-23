------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Startup capabilities by name (CAP-002): the handles this process's
--  launcher placed in its table and recorded in its startup table.
--
--  @description
--  The generated Startup package of a program's manifest calls these, one
--  function per declared name; programs normally use that package rather
--  than spelling names here. A required name that the table does not grant
--  (of the kind asked) gives the null handle, which the kernel never
--  resolves; the generated package's startup check reports it before the
--  program runs (CAP-003). An optional one is
--  Present or not, and the caller must look.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Startup_Tables;
with CuBit.Startup_State;

package CuBit.Handles.Startup with SPARK_Mode is
   package ST renames CuBit.Startup_Tables;

   type Optional_Endpoint (Present : Boolean := False) is record
      case Present is
         when True  => Value : Endpoint;
         when False => null;
      end case;
   end record;
   function Endpoint_Named (Label : String; Kind : ST.Capability_Kind := ST.Endpoint)
     return Endpoint
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Endpoint | ST.Render | ST.Network;
   function Optional_Endpoint_Named (Label : String; Kind : ST.Capability_Kind := ST.Endpoint)
     return Optional_Endpoint
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Endpoint | ST.Render | ST.Network;

   type Optional_Device_Memory (Present : Boolean := False) is record
      case Present is
         when True  => Value : Device_Memory;
         when False => null;
      end case;
   end record;
   function Device_Memory_Named (Label : String; Kind : ST.Capability_Kind := ST.Device_Memory)
     return Device_Memory
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Device_Memory | ST.Framebuffer;
   function Optional_Device_Memory_Named (Label : String; Kind : ST.Capability_Kind := ST.Device_Memory)
     return Optional_Device_Memory
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Device_Memory | ST.Framebuffer;

   type Optional_IO_Ports (Present : Boolean := False) is record
      case Present is
         when True  => Value : IO_Ports;
         when False => null;
      end case;
   end record;
   function IO_Ports_Named (Label : String; Kind : ST.Capability_Kind := ST.IO_Ports)
     return IO_Ports
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.IO_Ports;
   function Optional_IO_Ports_Named (Label : String; Kind : ST.Capability_Kind := ST.IO_Ports)
     return Optional_IO_Ports
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.IO_Ports;

   type Optional_Interrupt (Present : Boolean := False) is record
      case Present is
         when True  => Value : Interrupt;
         when False => null;
      end case;
   end record;
   function Interrupt_Named (Label : String; Kind : ST.Capability_Kind := ST.Interrupt)
     return Interrupt
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Interrupt;
   function Optional_Interrupt_Named (Label : String; Kind : ST.Capability_Kind := ST.Interrupt)
     return Optional_Interrupt
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Interrupt;

   type Optional_Notification (Present : Boolean := False) is record
      case Present is
         when True  => Value : Notification;
         when False => null;
      end case;
   end record;
   function Notification_Named (Label : String; Kind : ST.Capability_Kind := ST.Notification)
     return Notification
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Notification;
   function Optional_Notification_Named (Label : String; Kind : ST.Capability_Kind := ST.Notification)
     return Optional_Notification
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Notification;

   type Optional_Scheduling (Present : Boolean := False) is record
      case Present is
         when True  => Value : Scheduling;
         when False => null;
      end case;
   end record;
   function Scheduling_Named (Label : String; Kind : ST.Capability_Kind := ST.Scheduling)
     return Scheduling
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Scheduling;
   function Optional_Scheduling_Named (Label : String; Kind : ST.Capability_Kind := ST.Scheduling)
     return Optional_Scheduling
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Scheduling;

   type Optional_Process (Present : Boolean := False) is record
      case Present is
         when True  => Value : Process;
         when False => null;
      end case;
   end record;
   function Process_Named (Label : String; Kind : ST.Capability_Kind := ST.Process)
     return Process
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Process;
   function Optional_Process_Named (Label : String; Kind : ST.Capability_Kind := ST.Process)
     return Optional_Process
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Kind in ST.Process;

end CuBit.Handles.Startup;
