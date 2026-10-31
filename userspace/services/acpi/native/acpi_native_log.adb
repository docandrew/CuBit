with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;
with Startup;
with ACPI_Log_Lifetime;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
package body ACPI_Native_Log is
   use type CuBit.Handles.ABI.Kind;
   function Logstore_Peer return CuBit.Process_IDs.Process_ID is
      D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe
        (CuBit.Logging.Logstore_Handles.Handle (Startup.Logstore));
   begin
      if D.Current and then D.Kind = CuBit.Handles.ABI.Endpoint then
         declare P : constant CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.From_Word (D.Object); begin
            if CuBit.Process_IDs.Is_Process (P) then return P; end if;
         end;
      end if;
      return CuBit.Process_IDs.No_Process;
   end Logstore_Peer;
   Peer : constant CuBit.Process_IDs.Process_ID := Logstore_Peer;
   Enabled : Boolean := True;
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   procedure Write (Text : String; Level : CuBit.Log_Records.Severity) is
      Value : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make (Text, Level);
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
      if Enabled and then Value.Success then
         CuBit.Logging.Emit (Logger, Value.Value, Submitted);
      end if;
   end Write;
   procedure Info (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Information);
   end Info;
   procedure Warning (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Warning);
   end Warning;
   procedure Critical (Text : String) is
   begin
      Write (Text, CuBit.Log_Records.Critical);
   end Critical;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      Finished : Boolean;
   begin
      if Enabled and then ACPI_Log_Lifetime.Related (Peer, Item) then
         Enabled := False;
         CuBit.Logging.Disconnect (Logger, Finished);
      end if;
   end Grant_Notice;
end ACPI_Native_Log;
