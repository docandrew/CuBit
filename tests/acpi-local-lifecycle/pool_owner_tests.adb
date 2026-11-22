pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with Capability_ABI;
with ACPI_Platform_Pool_Owner;
procedure Pool_Owner_Tests is
   subtype Word is Interfaces.Unsigned_64;
   use type Word, Interfaces.Unsigned_32, Interfaces.Unsigned_8;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Peer : constant Word := 16#F123_4567_0000_1234#;
   type Carried is (Absent, Present, Refused);
   Inspections, Closes, Exchanges, Clock_Reads, Checks : Natural := 0;
   Allow_Close : Boolean := Mode /= "cleanup";
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "check" & Checks'Image & " " & Mode; end if;
   end Check;
   function Inspect_Endpoint (Target : Word; Expected_Owner : Word) return Boolean;
   function Is_Present (Item : Carried) return Boolean is (Item = Present);
   function Is_Refused (Item : Carried) return Boolean is (Item = Refused);
   procedure Inspect_Pool (Item : Carried; Value : out Word; Accepted : out Boolean);
   procedure Dispose (Item : in out Carried; Closed : out Boolean);
   procedure Clock (Milliseconds : out Word; Available : out Boolean);
   package P is new ACPI_Platform_Pool_Owner
     (Word,Word,0,Carried,Absent,Inspect_Endpoint,Is_Present,Is_Refused,Inspect_Pool,Dispose,Clock);
   use type P.Phase, P.Outcome;
   State : P.Context;
   procedure Exchange (Target : Word; Request, Deadline_Milliseconds : Word;
      Reply : out P.Response; Received : out Carried);
   procedure Get is new P.Acquire (Exchange);
   procedure Try_Reentry is
      Accepted : Boolean;
   begin
      if Mode = "reentry" then
         Get (State,17,Peer,99,100,Accepted);
         Check (not Accepted and P.Observe (State).Stage = P.Working);
         P.Close_Admission (State);
         Check (P.Observe (State).Stage = P.Working);
         P.Retry_Disposal (State,Accepted);
         Check (not Accepted);
      end if;
   end Try_Reentry;
   function Inspect_Endpoint (Target : Word; Expected_Owner : Word) return Boolean is
   begin
      Inspections := Inspections + 1;
      Check (P.Observe (State).Stage = P.Working);
      Try_Reentry;
      return Target = 17 and Expected_Owner = Peer and Mode /= "endpoint";
   end Inspect_Endpoint;
   procedure Inspect_Pool (Item : Carried; Value : out Word; Accepted : out Boolean) is
   begin
      Check (Item = Present and P.Observe (State).Stage = P.Working);
      Try_Reentry;
      Accepted := Mode /= "bad-pool" and Mode /= "cleanup";
      Value := (if Accepted then 16#FEED_0000_0000_0123# else 0);
   end Inspect_Pool;
   procedure Dispose (Item : in out Carried; Closed : out Boolean) is
   begin
      Closes := Closes + 1;
      Check (Item = Present and P.Observe (State).Stage = P.Working);
      Try_Reentry;
      Closed := Allow_Close;
      if Closed then Item := Absent; end if;
   end Dispose;
   procedure Clock (Milliseconds : out Word; Available : out Boolean) is
   begin
      Clock_Reads := Clock_Reads + 1;
      Try_Reentry;
      Available := Mode /= "no-clock" and not (Mode = "lost-clock" and Clock_Reads = 2);
      Milliseconds := 10;
      if Mode = "expired-before" then Milliseconds := 100;
      elsif Mode = "expired-after" and Clock_Reads = 2 then Milliseconds := 100;
      elsif Mode = "reversed" and Clock_Reads = 2 then Milliseconds := 9;
      end if;
   end Clock;
   procedure Exchange (Target : Word; Request, Deadline_Milliseconds : Word;
      Reply : out P.Response; Received : out Carried) is
   begin
      Exchanges := Exchanges + 1;
      Check (Target = 17 and Request = 41 and Deadline_Milliseconds = 100);
      Check (P.Observe (State).Stage = P.Working and P.Pool (State) = 0);
      Try_Reentry;
      Reply := (Ending => P.Answered, Label => P.Wire.Pool_Request,
         Length => P.Wire.Reply_Words, IPC => (Capability_Transferred => True, others => False),
         Data => [41,1,4,0], others => <>);
      Received := Present;
      if Mode = "not-sent" then Reply.Ending := P.Not_Dispatched;
      elsif Mode = "timeout" then Reply.Ending := P.Completion_Unknown;
      elsif Mode = "wrong-label" then Reply.Label := Reply.Label + 1;
      elsif Mode = "wrong-length" then Reply.Length := 4;
      elsif Mode = "protocol" then Reply.Protocol := 1;
      elsif Mode = "reserved" then Reply.Reserved := 1;
      elsif Mode = "accepts" then Reply.IPC.Accepts_Capability := True;
      elsif Mode = "offered" then Reply.IPC.Capability_Offered := True;
      elsif Mode = "both-flags" then Reply.IPC.Capability_Refused := True;
      elsif Mode = "missing-flag" then Reply.IPC.Capability_Transferred := False;
      elsif Mode = "wrong-echo" then Reply.Data (0) := 42;
      elsif Mode = "padding" then Reply.Data (3) := 1;
      elsif Mode = "disposition" then Reply.Data (1) := Word'Last;
      elsif Mode = "members" then Reply.Data (2) := Word'Last;
      elsif Mode = "empty" then Reply.Data (2) := 0;
      elsif Mode = "missing-cap" then Received := Absent; Reply.IPC := Capability_ABI.No_IPC_Flags;
      elsif Mode = "transfer-refused" then
         Received := Refused; Reply.IPC := (Capability_Refused => True, others => False);
      elsif Mode = "server-refused" then
         Received := Absent; Reply.Data (1) := 4; Reply.IPC := Capability_ABI.No_IPC_Flags;
      elsif Mode = "denial-cap" then Reply.Data (1) := 2;
      end if;
   end Exchange;
   Accepted, Done : Boolean;
   Request : Word := 41;
   Owner : Word := Peer;
   Deadline : Word := 100;
   Expected : P.Outcome := P.Bad_Reply;
begin
   if Mode = "zero-request" then Request := 0;
   elsif Mode = "zero-owner" then Owner := 0;
   elsif Mode = "forever" then Deadline := Word'Last;
   elsif Mode = "zero-deadline" then Deadline := 0;
   end if;
   Check (P.Observe (State).Stage = P.Fresh and P.Pool (State) = 0);
   Get (State,17,Owner,Request,Deadline,Accepted);
   if Mode = "normal" or Mode = "empty" or Mode = "reentry" then
      Check (Accepted and P.Observe (State).Stage = P.Ready);
      Check (P.Pool (State) = 16#FEED_0000_0000_0123#);
      Check (P.Observe (State).Peer = Peer and P.Observe (State).Request = 41);
      Check (P.Observe (State).Retained_Carriage and Closes = 0 and Exchanges = 1);
      Check (P.Observe (State).Members = (if Mode = "empty" then 0 else 4));
      P.Close_Admission (State);
      Check (P.Observe (State).Stage = P.Closed and P.Pool (State) = 0);
      Check (P.Observe (State).Retained_Carriage and Closes = 0);
   else
      if Mode = "zero-owner" or Mode = "zero-request" then Expected := P.Bad_Request;
      elsif Mode = "forever" or Mode = "zero-deadline" then Expected := P.Invalid_Deadline;
      elsif Mode = "endpoint" then Expected := P.Bad_Endpoint;
      elsif Mode = "no-clock" or Mode = "lost-clock" then Expected := P.Clock_Unavailable;
      elsif Mode = "expired-before" or Mode = "expired-after" then Expected := P.Deadline_Expired;
      elsif Mode = "reversed" then Expected := P.Clock_Reversed;
      elsif Mode = "not-sent" then Expected := P.Not_Sent;
      elsif Mode = "timeout" then Expected := P.Unknown_Completion;
      elsif Mode = "server-refused" then Expected := P.Server_Refused;
      elsif Mode = "transfer-refused" then Expected := P.Transfer_Refused;
      end if;
      Check (not Accepted and P.Pool (State) = 0 and P.Observe (State).Result = Expected);
      if Mode = "cleanup" then
         Check (P.Observe (State).Stage = P.Cleanup_Required);
         Check (P.Observe (State).Retained_Carriage and Closes = 1);
         P.Close_Admission (State);
         Check (P.Observe (State).Stage = P.Cleanup_Required);
         P.Retry_Disposal (State,Done);
         Check (not Done and Closes = 2 and P.Observe (State).Retained_Carriage);
         Allow_Close := True;
         P.Retry_Disposal (State,Done);
         Check (Done and Closes = 3 and not P.Observe (State).Retained_Carriage);
         Check (P.Observe (State).Stage = P.Rejected and P.Observe (State).Result = Expected);
      else
         Check (not P.Observe (State).Retained_Carriage);
      end if;
      if Mode = "timeout" or Mode = "lost-clock" or Mode = "reversed" or Mode = "expired-after" then
         Check (P.Observe (State).Stage = P.Uncertain);
         P.Close_Admission (State);
         Check (P.Observe (State).Stage = P.Uncertain and P.Observe (State).Result = Expected);
         P.Close_Admission (State);
         Check (P.Observe (State).Stage = P.Uncertain);
      end if;
   end if;
   declare Before : constant Natural := Exchanges; Old_Closes : constant Natural := Closes; begin
      Get (State,17,Peer,42,100,Accepted);
      Check (not Accepted and Exchanges = Before and Closes = Old_Closes);
      P.Retry_Disposal (State,Done);
      Check (not Done and Closes = Old_Closes);
   end;
   if Inspections = 0 then Check (Exchanges = 0 and Clock_Reads = 0); end if;
   Ada.Text_IO.Put_Line ("POOL OWNER " & Mode & " checks" & Checks'Image);
end Pool_Owner_Tests;
