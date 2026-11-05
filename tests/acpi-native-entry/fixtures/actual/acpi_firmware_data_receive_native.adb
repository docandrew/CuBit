pragma Ada_2022;
with Interfaces;
with ACPI_Register_Roles;
with ACPI_Firmware_Data_Framing;
package body ACPI_Firmware_Data_Receive_Native is
   package M renames CuBit.Messages;
   package P renames ACPI_Firmware_Data_Replies;
   package F renames P.F;
   package IPC renames P.IPC;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   procedure Handle (From : M.Process_ID; Request : M.Message;
      Response : in out M.Message; Handled : in out Boolean) is
      Input, Output : IPC.Packet;
      Selected : Boolean;
      Caller : P.Word := 0;
   begin
      if Handled or else Request.tag.label < F.First_Label
        or else Request.tag.label > F.Label_For (F.Operation'Last) then return; end if;
      Handled := True;
      Input := ACPI_Firmware_Data_Framing.Normalize_Request
         ((Request.tag.label,Request.tag.length,M.Flag_Byte (Request.tag),
           Request.tag.reserved,IPC.Words (Request.words)));
      if M.Is_Process (From) and then ACPI_Register_Roles.Is_Frontend
         (ACPI_Register_Roles.Role_Tag (Request.authorityTag)) then Caller := M.To_Word (From); end if;
      -- Only Capability_Offered may arrive on a legitimate request. Do not
      -- accept it. Its nonzero flag remains visible and framing refuses it.
      -- Transferred/refused flags also remain invalid. No reply-handle decoder
      -- or close function is applied to the incoming authority stamp.
      Dispatch (Caller,Input,False,Output,Selected);
      Response := M.NULL_MESSAGE;
      Response.tag.label := Request.tag.label + F.Reply_Delta;
      if Selected and then Output.Label = Response.tag.label and then
         Output.Length in 0 | 4 and then Output.Flags = 0 and then Output.Reserved = 0
      then
         Response.tag.length := Output.Length;
         if Output.Length = 4 then Response.words := M.MessageWords (Output.Data); end if;
      end if;
   end Handle;
end ACPI_Firmware_Data_Receive_Native;
