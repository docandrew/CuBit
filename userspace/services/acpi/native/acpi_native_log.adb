with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;
with Startup;
with ACPI_Log_Lifetime;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
package body ACPI_Native_Log is
   use type Interfaces.Unsigned_64;
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
   Publisher_Started : Boolean := False;
   Copied_Loss : Interfaces.Unsigned_64 := 0;
   procedure Drop_Copy is
   begin
      if Copied_Loss<Interfaces.Unsigned_64'Last then Copied_Loss:=Copied_Loss+1; end if;
   end Drop_Copy;
   function Copied_Dropped return Interfaces.Unsigned_64 is (Copied_Loss);
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   function Publisher_Dropped return Interfaces.Unsigned_64 is
     (CuBit.Logging.Dropped (Logger));
   procedure Write (Text : String; Level : CuBit.Log_Records.Severity;
      Allow_Open : Boolean := True; Is_Copy : Boolean := False) is
      Value : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make (Text, Level);
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
      if not Allow_Open and then not Publisher_Started then
         if Is_Copy then Drop_Copy; end if;
         return;
      end if;
      if Enabled and then Value.Success then
         CuBit.Logging.Emit (Logger, Value.Value, Submitted);
         -- Emit's only blocking path is the one-time Uninitialized open.
         -- After return it is Ready or Disabled, never Uninitialized again.
         -- Channels.Put copies/sheds and may issue a non-blocking capSubmit
         -- wakeup. No Flush, channel open or synchronous peer call below.
         Publisher_Started := True;
         if Is_Copy and then not Submitted then Drop_Copy; end if;
      elsif Is_Copy then
         Drop_Copy;
      end if;
   end Write;
   procedure Copied (Text : String; Peer_Quiet : Boolean; Success : Boolean := False) is
   begin
      Write(Text,(if Success then CuBit.Log_Records.Information else CuBit.Log_Records.Warning),
         Allow_Open=>Peer_Quiet,Is_Copy=>True);
   end Copied;
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
   Diagnostic_Drain_Milliseconds : constant Natural := 200;
   procedure Drain_Diagnostics (IO_Pending, IO_Uncertain : Boolean; Drained : out Boolean) is
   begin
      Drained := False;
      if IO_Pending or else IO_Uncertain or else not Enabled then return; end if;
      CuBit.Logging.Flush (Logger, Drained, Wait_Ms => Diagnostic_Drain_Milliseconds);
   end Drain_Diagnostics;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      Finished : Boolean;
   begin
      if Enabled and then ACPI_Log_Lifetime.Related (Peer, Item) then
         Enabled := False;
         CuBit.Logging.Disconnect (Logger, Finished);
      end if;
   end Grant_Notice;
end ACPI_Native_Log;
