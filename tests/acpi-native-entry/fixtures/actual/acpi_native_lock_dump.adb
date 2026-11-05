pragma Ada_2022;
with Interfaces; with CuBit.Messages; with CuBit.Monotonic; with CuBit.Handles.IPC;
with ACPI_Lock_Trace_Protocol; with ACPI_Region_Protocol; with ACPI_Native_Log;
package body ACPI_Native_Lock_Dump is
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64, CuBit.Handles.Endpoint;
   Attempted : Boolean := False;
   Diagnostic_Timeout_Milliseconds : constant Interfaces.Unsigned_64 := 250;
   procedure Request (Endpoint : CuBit.Handles.Startup.Optional_Endpoint) is
      package P renames ACPI_Lock_Trace_Protocol;
      Frame : constant P.Packet := P.Request;
      Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Now : Interfaces.Unsigned_64;
      Reply : P.Reply_Result;
   begin
      if Attempted then return; end if; Attempted := True;
      if not Endpoint.Present or else Endpoint.Value = CuBit.Handles.No_Endpoint then return; end if;
      Now := CuBit.Monotonic.Milliseconds;
      if Now >= Interfaces.Unsigned_64'Last-Diagnostic_Timeout_Milliseconds then
         ACPI_Native_Log.Info ("acpi: stage lock-trace fail CLOCK_RANGE"); return;
      end if;
      Message.tag := (label => Frame.Label,length => Frame.Length,protocol => 0,ipc => <>,reserved => 0);
      Message.words := CuBit.Messages.MessageWords (Frame.Data);
      Message.tag := CuBit.Handles.IPC.Call (Endpoint.Value,Message,Now+Diagnostic_Timeout_Milliseconds);
      if Message.tag.label >= CuBit.Messages.Kernel_Reply_First then
         ACPI_Native_Log.Info ("acpi: stage lock-trace fail IPC_UNAVAILABLE"); return;
      end if;
      Reply := P.Decode ((Message.tag.label,Message.tag.length,CuBit.Messages.Flag_Byte (Message.tag),
        Message.tag.reserved,ACPI_Region_Protocol.Words (Message.words)));
      if not Reply.Valid then ACPI_Native_Log.Info ("acpi: stage lock-trace fail MALFORMED_REPLY"); return; end if;
      case Reply.Status is
         when P.Accepted => ACPI_Native_Log.Info ("acpi: stage lock-trace ok DUMP_REQUESTED");
         when P.Already_Dumped => ACPI_Native_Log.Info ("acpi: stage lock-trace ok ALREADY_REQUESTED");
         when P.Unauthorized => ACPI_Native_Log.Info ("acpi: stage lock-trace fail UNAUTHORIZED");
         when P.Malformed => ACPI_Native_Log.Info ("acpi: stage lock-trace fail MALFORMED_REQUEST");
      end case;
   end Request;
end ACPI_Native_Lock_Dump;
