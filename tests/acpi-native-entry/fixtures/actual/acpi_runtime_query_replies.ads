pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
generic
   type Details is private;
   -- The caller binds the generated typed IPC reply encoder. No numeric
   -- labels, caller PIDs or handle words are invented by this adapter.
   with function Encode (Value : Details) return CuBit.Messages.Message;
package ACPI_Runtime_Query_Replies with SPARK_Mode => Off is
   procedure Save (Handle : out CuBit.Handles.Reply; Accepted : out Boolean);
   procedure Answer (Handle : CuBit.Handles.Reply; Value : Details; Delivered : out Boolean);
end ACPI_Runtime_Query_Replies;
