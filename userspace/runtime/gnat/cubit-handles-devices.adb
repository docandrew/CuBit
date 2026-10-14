------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Devices - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with Boot_Timeline_Layout;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Devices is

   package K renames CuBit.Handles.Kernel;
   package BT renames Boot_Timeline_Layout;

   Latency_Realtime : constant Unsigned_64 := 3;   --  kernel LATENCY_REALTIME

   function Tag_Word is new Ada.Unchecked_Conversion
     (CuBit.Messages.MessageTag, Unsigned_64);

   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean) return Boolean is
     (K.Call (K.Map_Device, Physical, Virtual, Pages,
              (if Read_Only then 1 else 0), K.Word (Region)) = 0);

   function Map_Framebuffer (Region : Device_Memory) return Unsigned_64 is
     (K.Call (K.Map_Framebuffer, K.Word (Region)));

   function In_8 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64 is
     (K.Call (K.In_8, Unsigned_64 (Port), 0, K.Word (Ports)));
   function In_16 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64 is
     (K.Call (K.In_16, Unsigned_64 (Port), 0, K.Word (Ports)));
   function In_32 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64 is
     (K.Call (K.In_32, Unsigned_64 (Port), 0, K.Word (Ports)));
   function Out_8 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_8) return Boolean is
     (K.Call (K.Out_8, Unsigned_64 (Port), Unsigned_64 (Value), K.Word (Ports)) = 0);
   function Out_16 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_16) return Boolean is
     (K.Call (K.Out_16, Unsigned_64 (Port), Unsigned_64 (Value), K.Word (Ports)) = 0);
   function Out_32 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_32) return Boolean is
     (K.Call (K.Out_32, Unsigned_64 (Port), Unsigned_64 (Value), K.Word (Ports)) = 0);

   function Register (Role : Unsigned_64; Authority : Notification) return Unsigned_64 is
     (K.Call (K.Register_Driver, Role, K.Word (Authority)));

   function Sending
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Word : Unsigned_64) return Boolean;
   function Sending
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Word : Unsigned_64) return Boolean is
     (K.Call (K.Send_Event, CuBit.Messages.To_Word (Destination), Tag_Word (Msg.tag),
              Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3), Word) = 1);

   function Send_Event
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Authority : Notification) return Boolean is
     (Sending (Destination, Msg, K.Word (Authority)));
   function Send_Event
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Authority : Endpoint) return Boolean is
     (Sending (Destination, Msg, K.Word (Authority)));

   function Reserve_Realtime
     (Period_Us, Budget_Us, Flags : Unsigned_64; Authority : Scheduling) return Unsigned_64 is
     (K.Call (K.Set_Latency_Contract, Latency_Realtime, Period_Us, Budget_Us, Flags,
              K.Word (Authority)));

   function Read_Timeline
     (Authority : Process; Buffer, Offset, Bytes : Unsigned_64) return Unsigned_64 is
     (K.Call (K.Boot_Timeline, BT.Operation'Enum_Rep (BT.Read), Buffer, Bytes, Offset,
              K.Word (Authority)));

   function Mark_Milestone (Milestone : Unsigned_64; Authority : Notification)
     return Boolean is
     (K.Call (K.Boot_Timeline, BT.Operation'Enum_Rep (BT.Mark), Milestone, 0, 0,
              K.Word (Authority)) = 0);

end CuBit.Handles.Devices;
