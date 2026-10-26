with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Monotonic;
with CuBit.Handles;
with CuBit.Handles.Services;
with Intel_GPU_Diagnostic_Capture;
with Intel_GPU_Log_Grant;
with Intel_GPU_Supervisor;
package body Intel_GPU_Diagnostics is
   use Interfaces;
   package G renames Intel_GPU_Log_Grant;
   use type G.Phase;
   use type G.Reply_Kind;
   use type G.Event_Count;
   Maximum_Line : constant := 512;
   Capture_Capacity : constant := 512;
   package Buffering is new Intel_GPU_Diagnostic_Capture (Capture_Capacity, Maximum_Line);
   Buffer : Buffering.Queue;
   --  devmgr grants logstore at run time (Intel_GPU_Log_Grant).
   Writer : CuBit.Logging.Publisher := CuBit.Logging.Awaiting_Grant;
   Grant : G.State;
   Grant_Started : Boolean := False;
   Summary_Due : Boolean := False;
   Grant_Token : constant Unsigned_64 := 16#4947_0001#;
   Grant_Request_Label : constant := 16#0229#;
   --  Records published per tick: a copy each into the publisher's ring
   --  (CuBit.Logging), so a tick can empty much of the capture buffer.
   Batch_Records : constant := 64;
   --  Completions polled per call, so malformed or duplicate replies
   --  cannot keep a caller here.
   Poll_Batch : constant := 8;

   --  Completions Tick polled while looking for the grant reply: kept in
   --  order for Poll_Driver's callers (never dropped, never reordered).
   Kept_Capacity : constant := 16;
   type Kept_Index is mod Kept_Capacity;
   subtype Kept_Count is Natural range 0 .. Kept_Capacity;
   Kept : array (Kept_Index) of CompletionEntry;
   Kept_Head : Kept_Index := 0;
   Kept_Used : Kept_Count := 0;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Counts return String is
     ("requests=" & Image (Unsigned_64 (Grant.Requests)) &
      " submit-failures=" & Image (Unsigned_64 (Grant.Submit_Failures)) &
      " busy=" & Image (Unsigned_64 (Grant.Busy_Replies)) &
      " refused=" & Image (Unsigned_64 (Grant.Refusals)) &
      " malformed=" & Image (Unsigned_64 (Grant.Malformed_Replies)));

   procedure Handle_Grant (Receipt : CompletionEntry; Now : Unsigned_64) is
      Tag : constant MessageTag := Intel_GPU_Supervisor.Reply_Tag (Receipt);
      Logstore : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Kind : G.Reply_Kind;
   begin
      if Grant.Current /= G.Requested then
         return;   --  Never asked twice: nothing else carries this token.
      end if;
      if Receipt.status = COMPLETION_OK and then
        Unsigned_64 (Tag.label) = G.Supervisor_Granted
      then
         Logstore := Intel_GPU_Supervisor.Granted_Endpoint (Receipt);
      end if;
      Kind := G.Classify
        (Completed      => Receipt.status = COMPLETION_OK,
         Label          => Unsigned_64 (Tag.label),
         Length         => Unsigned_64 (Tag.length),
         Words_Zero     => Receipt.msg.words = [0, 0, 0, 0],
         Endpoint_Valid => CuBit.Handles.Valid (Logstore));
      if Kind = G.Granted then
         CuBit.Logging.Attach (Writer, CuBit.Handles.Services.Logstore.Granted (Logstore));
      else
         debugPrint ("intel-gpu: logstore grant " & G.Kind_Name (Kind) &
           " label=" & Image (Unsigned_64 (Tag.label)) & " status=" & Image (Receipt.status) &
           "; retrying" & ASCII.LF);
      end if;
      G.Answered (Grant, Now, Kind);
   end Handle_Grant;

   --  Take the grant reply if it came; keep every other completion.
   procedure Look_For_Grant (Now : Unsigned_64) is
      Receipt : aliased CompletionEntry;
   begin
      for Count in 1 .. Poll_Batch loop
         exit when Grant.Current /= G.Requested or else Kept_Used = Kept_Capacity;
         exit when Poll_Completion (Receipt'Address) = 0;
         if Receipt.token = Grant_Token then
            Handle_Grant (Receipt, Now);
         else
            Kept (Kept_Head + Kept_Index (Kept_Used)) := Receipt;
            Kept_Used := Kept_Used + 1;
         end if;
      end loop;
   end Look_For_Grant;

   procedure Publish is
      Value : Buffering.Captured_Record;
      Found, Ignore_Accepted, Report : Boolean;
   begin
      G.Take_Report (Grant, Report);
      if Report then
         declare
            Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
              ("intel-gpu: logstore attached after " &
               Image (G.Elapsed (Grant, Grant.Attached_At)) & " ms (" & Counts & ")",
               (if Grant.Requests > 1 then CuBit.Log_Records.Warning
                else CuBit.Log_Records.Information));
         begin
            if Made.Success then
               CuBit.Logging.Emit (Writer, Made.Value, Ignore_Accepted);
            end if;
         end;
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
   end Publish;

   procedure Tick is
      --  The published clock: no system call in the request loop's hot path.
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Msg : Message := NULL_MESSAGE;
      Notice : Boolean;
   begin
      if not Grant_Started then
         Grant := G.Initial (Now);
         Grant_Started := True;
      end if;
      G.Observe_Time (Grant, Now);
      G.Take_Notice (Grant, Notice);
      if Notice then
         debugPrint ("intel-gpu: logstore not available after" &
           Unsigned_64'Image (G.Availability_Deadline_Ms) & " ms (" & Counts &
           "); lines stay on serial; asking every" &
           Unsigned_64'Image (G.Late_Retry_Interval_Ms) & " ms" & ASCII.LF);
      end if;
      if G.Request_Due (Grant, Now) then
         if Registered_Driver (DRIVER_LOGSTORE) = No_Process then
            G.Not_Registered (Grant, Now);
         else
            Msg.tag := (label => Grant_Request_Label, length => 0, protocol => 0,
                        ipc => <>, reserved => 0);
            --  devmgr answers with a badged logstore endpoint (CAP-002).
            G.Submitted (Grant, Now, Intel_GPU_Supervisor.Submit_Accepting (Msg, Grant_Token));
         end if;
      end if;
      Look_For_Grant (Now);
      if G.Publishing (Grant) then
         Publish;
      end if;
   end Tick;

   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is
   begin
      debugPrint (Text & ASCII.LF);
      if Text'Length in 1 .. Maximum_Line then
         Buffering.Append (Buffer, Text, Level);
         Summary_Due := True;
      end if;
      Tick;
   end Capture;

   function Poll_Driver (Result : System.Address) return Unsigned_64 is
      Receipt : CompletionEntry with Import, Address => Result;
   begin
      if Kept_Used > 0 then
         Receipt := Kept (Kept_Head);
         Kept_Head := Kept_Head + 1;
         Kept_Used := Kept_Used - 1;
         return 1;
      end if;
      for I in 1 .. Poll_Batch loop
         if Poll_Completion (Result) = 0 then return 0; end if;
         if Receipt.token /= Grant_Token then
            return 1;
         end if;
         Handle_Grant (Receipt, CuBit.Monotonic.Milliseconds);
      end loop;
      return 0;
   end Poll_Driver;

   function Pending return Boolean is
     ((Buffering.Count (Buffer) > 0 or else Summary_Due) and then G.Worth_Waiting (Grant));
end Intel_GPU_Diagnostics;
