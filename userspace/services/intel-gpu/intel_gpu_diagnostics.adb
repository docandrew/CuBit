with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
with Intel_GPU_Diagnostic_Capture;
package body Intel_GPU_Diagnostics is
   use Interfaces;
   package Buffering is new Intel_GPU_Diagnostic_Capture (512, 512);
   Buffer : Buffering.Queue;
   Writer : CuBit.Logging.Publisher;
   type State is (Need_Grant, Grant_Pending, Ready, Stopped);
   Phase : State := Need_Grant;
   Due : Unsigned_64 := 0;
   Summary_Due : Boolean := False;
   Grant_Token : constant Unsigned_64 := 16#4947_0001#;
   --  Records published per tick: a copy each into the publisher's ring
   --  (CuBit.Logging), so a tick can empty much of the capture buffer.
   Batch_Records : constant := 64;
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is
   begin
      debugPrint (Text & ASCII.LF);
      if Text'Length in 1 .. 512 then
         Buffering.Append (Buffer, Text, Level);
         Summary_Due := True;
      end if;
   end Capture;
   procedure Tick is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Msg : Message := NULL_MESSAGE;
      Value : Buffering.Captured_Record;
      Found, Ignore_Accepted : Boolean;
   begin
      if Phase in Grant_Pending | Stopped or else Now < Due then
         return;
      end if;
      Due := (if Now <= Unsigned_64'Last - 100 then Now + 100 else Unsigned_64'Last);
      if Phase = Need_Grant then
         if CuBit.Messages."=" (Registered_Driver (DRIVER_LOGSTORE), No_Process) then return; end if;
         Msg.tag := (16#0229#, 0, 0, 0);
         Phase := (if capSubmit (15, Msg, Grant_Token) then Grant_Pending else Stopped);
         return;
      end if;
      for Count in 1 .. Batch_Records loop
         Buffering.Take (Buffer, Value, Found);
         exit when not Found and then not Summary_Due;
         declare
            Record_Value : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
              ((if Found then Value.Text (1 .. Value.Length) else
               "intel-gpu: diagnostic capture overflow" & Unsigned_64'Image (Buffering.Lost (Buffer)) &
               " publication losses" & Unsigned_64'Image (CuBit.Logging.Dropped (Writer))),
               (if Found then Value.Level
                elsif Buffering.Lost (Buffer) /= 0 or else CuBit.Logging.Dropped (Writer) /= 0
                then CuBit.Log_Records.Warning else CuBit.Log_Records.Debug));
         begin
            if not Found then Summary_Due := False; end if;
            if Record_Value.Success then
               --  A shed record is counted (Dropped, in the summary).
               CuBit.Logging.Emit (Writer, Record_Value.Value, Ignore_Accepted);
            end if;
         end;
         exit when not Found;
      end loop;
   end Tick;
   function Poll_Driver (Result : System.Address) return Unsigned_64 is
      Receipt : CompletionEntry with Import, Address => Result;
   begin
      -- Bound work even if malformed/duplicate logger replies arrive.
      for I in 1 .. 8 loop
         if Poll_Completion (Result) = 0 then return 0; end if;
         if Receipt.token = Grant_Token then
            if Phase = Grant_Pending and then Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then Phase := Ready;
            else Phase := Stopped; end if;
         else
            return 1;
         end if;
      end loop;
      return 0;
   end Poll_Driver;
end Intel_GPU_Diagnostics;
