with Interfaces;
package body Config_Worker_Startup with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_8;
   function Authorized_Config_Request
     (Sender : CuBit.Messages.Process_ID; Tag : Interfaces.Unsigned_64;
      Registered_Config : CuBit.Messages.Process_ID) return Boolean is
     (CuBit.Messages."/=" (Registered_Config, CuBit.Messages.No_Process) and then
      CuBit.Messages."=" (Sender, Registered_Config) and then
      Tag = CuBit.Messages.To_Word (Registered_Config));
   function Valid_Attachment (Request : CuBit.Messages.Message) return Boolean is
     (Request.tag.label = Operation'Enum_Rep (Attach_Worker)
      and then Request.tag.length = 1 and then Request.tag.flags = 0
      and then Request.tag.reserved = 0 and then Request.words (0) /= 0
      and then Request.words (1) = 0 and then Request.words (2) = 0
      and then Request.words (3) = 0);
end Config_Worker_Startup;
