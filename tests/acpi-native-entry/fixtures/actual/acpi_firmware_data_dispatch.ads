pragma Ada_2022;
with ACPI_Firmware_Data_Replies;
generic
   with function Authorized (Caller : ACPI_Firmware_Data_Replies.Word) return Boolean;
   -- Trusted serialized backend composition of existing retained owners.
   -- Publish/Describe/Recover -> publication owner; scope operations -> scope
   -- owner; Read/Write/Query -> operation history. Only History.Execute may
   -- call Scopes.Perform -> Records.Access_Record. Query is always passive.
   -- No callback-selected correlation, label, flags or capability is exposed.
   with procedure Handle_Request
     (Caller : ACPI_Firmware_Data_Replies.Word;
      Request : ACPI_Firmware_Data_Replies.F.Request;
      Status : out ACPI_Firmware_Data_Replies.Outcome;
      Effect : out ACPI_Firmware_Data_Replies.Effect_Kind;
      Value : out ACPI_Firmware_Data_Replies.Word);
package ACPI_Firmware_Data_Dispatch is
   package P renames ACPI_Firmware_Data_Replies;
   package F renames P.F;
   package IPC renames F.IPC;
   type Context is limited private;
   type Statistics is record
      Accepted, Malformed, Denied, Reentered : P.Word;
      Working : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   -- Caller and Has_Capability are authenticated receive metadata, not
   -- payloads. The native loop owns/disposes any unexpected carried handle.
   procedure Dispatch (Owner : in out Context; Caller : P.Word;
      Request : IPC.Packet; Has_Capability : Boolean;
      Response : out IPC.Packet; Handled : out Boolean);
private
   type Context is limited record
      Working : Boolean := False;
      Accepted, Malformed, Denied, Reentered : P.Word := 0;
   end record;
end ACPI_Firmware_Data_Dispatch;
