with CuBit.Messages;
with CuBit.Memory_Grants;

--  Private Config/worker transport, not a public arbitrary-storage endpoint.
--  Kernel endpoint/reply authority authenticates transport, never these labels.
package Config_Worker_Messages is
   type Operation is (Exchange_Frame, Provision_Schema, Exchange_Type);
   for Operation use (Exchange_Frame => 16#0610#, Provision_Schema => 16#0615#,
                      Exchange_Type => 16#0617#);
   type Status is (Frame_Ready, Invalid_Request, Unavailable, Denied, Schema_Ready, Type_Ready);
   for Status use (Frame_Ready => 16#F000#, Invalid_Request => 16#F001#,
                   Schema_Ready => 16#F010#, Type_Ready => 16#F011#,
                   Unavailable => 16#F004#, Denied => 16#F007#);
   function Type_Request (Grant : CuBit.Memory_Grants.Grant_Reference) return CuBit.Messages.Message;
   function Valid_Type_Request (Item : CuBit.Messages.Message) return Boolean;
   function Type_Acknowledgment return CuBit.Messages.Message;
   function Valid_Type_Acknowledgment (Item : CuBit.Messages.Message) return Boolean;
   function Request (Grant : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message;
   function Acknowledgment return CuBit.Messages.Message;
   function Error (Code : Status) return CuBit.Messages.Message;
   function Valid_Request (Item : CuBit.Messages.Message) return Boolean;
   function Valid_Acknowledgment (Item : CuBit.Messages.Message) return Boolean;
   function Schema_Request (Grant : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message;
   function Valid_Schema_Request (Item : CuBit.Messages.Message) return Boolean;
   function Schema_Acknowledgment return CuBit.Messages.Message;
   function Valid_Schema_Acknowledgment (Item : CuBit.Messages.Message) return Boolean;
end Config_Worker_Messages;
