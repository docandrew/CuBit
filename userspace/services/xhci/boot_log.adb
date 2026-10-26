with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Log_Records;
with Driver_Log_Grant;
with Startup;

package body Boot_Log is
   package G renames Driver_Log_Grant;
   use type G.Phase;
   use type G.Reply_Kind;

   Capture_Capacity : constant := 256;
   subtype Line_Length is Natural range 0 .. Maximum_Line;
   type Captured is record
      Text    : String (1 .. Maximum_Line) := [others => ' '];
      Length  : Line_Length := 0;
      Warning : Boolean := False;
   end record;
   Lines : array (1 .. Capture_Capacity) of Captured;
   Used, Sent : Natural range 0 .. Capture_Capacity := 0;
   Lost : Natural := 0;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Awaiting_Grant;
   Grant : G.State;
   Grant_Started : Boolean := False;
   Report_Pending : Boolean := False;
   Unexpected_Completions : Natural := 0;
   Unexpected_Reported : Natural := 0;

   Grant_Token : constant Unsigned_64 := 16#584C_0001#;
   --  devmgr's log-publisher grant for xhci instances (devmgr main.adb).
   Grant_Request_Label : constant Unsigned_32 := 16#0228#;
   --  Completions taken per Poll, so a burst cannot keep the loop here.
   Poll_Batch : constant := 8;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Emit (Text : String; Warning : Boolean) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        (Text, (if Warning then CuBit.Log_Records.Warning
                else CuBit.Log_Records.Information));
      Ignore_Accepted : Boolean;
   begin
      if Made.Success then
         CuBit.Logging.Emit (Writer, Made.Value, Ignore_Accepted);
      end if;
   end Emit;

   procedure Serial (Text : String) is
   begin
      debugPrint (Text & ASCII.LF);
   end Serial;

   procedure Line (Text : String; Warning : Boolean := False) is
      Kept : constant Natural := Natural'Min (Text'Length, Maximum_Line);
   begin
      debugPrint (Text & ASCII.LF);
      if G.Publishing (Grant) and then Sent = Used then
         Emit (Text (Text'First .. Text'First + Kept - 1), Warning);
      elsif Used < Capture_Capacity then
         Used := Used + 1;
         Lines (Used).Text (1 .. Kept) := Text (Text'First .. Text'First + Kept - 1);
         Lines (Used).Length := Kept;
         Lines (Used).Warning := Warning;
      else
         Lost := Lost + 1;
      end if;
   end Line;

   procedure Handle_Reply (Receipt : CompletionEntry; Now : Unsigned_64) is
      Kind : G.Reply_Kind;
      Collector : CuBit.Handles.IPC.Accepted_Endpoint;
      Valid : Boolean := False;
   begin
      if Grant.Current /= G.Requested then
         return;   --  Never asked twice: nothing else carries this token.
      end if;
      if Receipt.status = COMPLETION_OK and then
        Unsigned_64 (Receipt.msg.tag.label) = G.Supervisor_Granted
      then
         Collector := CuBit.Handles.IPC.Accept_Endpoint
           (CuBit.Handles.IPC.Carried_By (Receipt.msg));
         Valid := Collector.OK;
      end if;
      Kind := G.Classify
        (Completed      => Receipt.status = COMPLETION_OK,
         Label          => Unsigned_64 (Receipt.msg.tag.label),
         Length         => Unsigned_64 (Receipt.msg.tag.length),
         Words_Zero     => Receipt.msg.words = [0, 0, 0, 0],
         Endpoint_Valid => Valid);
      if Kind = G.Granted then
         CuBit.Logging.Attach
           (Writer, CuBit.Handles.Services.Logstore.Granted (Collector.Value));
      else
         debugPrint ("xhci: logstore grant " & G.Kind_Name (Kind) & "; retrying" & ASCII.LF);
      end if;
      G.Answered (Grant, Now, Kind);
   end Handle_Reply;

   procedure Poll is
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Receipt : aliased CompletionEntry;
      Msg : Message := NULL_MESSAGE;
      Notice, Report : Boolean;
   begin
      if not Grant_Started then
         Grant := G.Initial (Now);
         Grant_Started := True;
      end if;
      G.Observe_Time (Grant, Now);
      G.Take_Notice (Grant, Notice);
      if Notice then
         debugPrint ("xhci: logstore not granted after" &
           Unsigned_64'Image (G.Availability_Deadline_Ms) &
           " ms; lines stay on serial; still asking" & ASCII.LF);
      end if;
      if G.Request_Due (Grant, Now) then
         if Registered_Driver (DRIVER_LOGSTORE) = No_Process then
            G.Not_Registered (Grant, Now);
         else
            Msg.tag := (label => Grant_Request_Label, length => 0,
                        protocol => 0, ipc => <>, reserved => 0);
            G.Submitted (Grant, Now,
              CuBit.Handles.IPC.Submit_Accepting (Startup.Ready, Msg, Grant_Token));
         end if;
      end if;
      for Count in 1 .. Poll_Batch loop
         exit when Poll_Completion (Receipt'Address) = 0;
         if Receipt.token = Grant_Token then
            Handle_Reply (Receipt, Now);
         else
            --  xhci submits nothing else that completes; count it, say so.
            Unexpected_Completions := Unexpected_Completions + 1;
         end if;
      end loop;
      if not G.Publishing (Grant) then
         return;
      end if;
      G.Take_Report (Grant, Report);
      if Report then
         Report_Pending := True;
      end if;
      while Sent < Used loop
         Sent := Sent + 1;
         Emit (Lines (Sent).Text (1 .. Lines (Sent).Length), Lines (Sent).Warning);
      end loop;
      if Report_Pending then
         Report_Pending := False;
         Emit ("xhci: logstore attached after " &
           Image (Natural (Unsigned_64'Min (G.Elapsed (Grant, Grant.Attached_At),
                                            Unsigned_64 (Natural'Last)))) &
           " ms; captured " & Image (Used) & " lines, lost " & Image (Lost) &
           ", grant requests " & Image (Natural (Grant.Requests)),
           Lost > 0);
      end if;
      if Unexpected_Completions /= Unexpected_Reported then
         Unexpected_Reported := Unexpected_Completions;
         Emit ("xhci: unexpected completions " & Image (Unexpected_Completions), True);
      end if;
   end Poll;

   function Deadline return Unsigned_64 is
     (if G.Publishing (Grant) then Unsigned_64'Last
      elsif not Grant_Started then 0
      elsif Grant.Current = G.Requested then Unsigned_64'Last
      else Grant.Next_Request);
end Boot_Log;
