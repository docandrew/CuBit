with Interfaces; use Interfaces;
with CuBit.Failures;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Service_Names;
with CCL_Processes;
with CCL.Interfaces.Logs;
with CCL_Native_Bindings;
with CuBit.Handles;
with CuBit.Handles.Services;

--  logs.recent on CuBit: for each process the name has named during this
--  boot (LOG-003: exited ones too, as logstore's launchers recorded them), a
--  subscription to logstore filtered to it, which replays its retained
--  records and is closed again; the newest that fit one result are kept.
package body CCL_Log_IO is
   procedure Announce (Text : String) is
      Published : Boolean;
   begin
      if CCL_Native_Bindings.Logstore.Present then
         CuBit.Logging.Announce (Text, Published, CCL_Native_Bindings.Logstore.Value);
      end if;
   end Announce;

   procedure Note (Text : String) is
   begin
      Announce (Text);
      CuBit.Messages.debugPrint (Text & ASCII.LF);
   end Note;
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   package Logs renames CCL.Interfaces.Logs;
   package Failures renames CuBit.Failures;
   use type P.Status;

   function Spelling (Status : P.Status) return String is
     (case Status is
         when P.OK => "ok", when P.Denied => "denied",
         when P.Invalid_Request => "invalid request", when P.Exhausted => "no subscription free",
         when P.Empty => "empty", when P.Gap => "records lost",
         when P.Unavailable => "unavailable", when P.Rate_Limited => "rate limited",
         when P.Below_Minimum => "below the minimum logstore keeps");

   function Logstore return CuBit.Messages.Process_ID is (CuBit.Service_Names.Process_Of ("logstore"));

   --  Observing logs needs the program's log-observer endpoint and a
   --  running logstore.
   function Available return Boolean is
     (CCL_Native_Bindings.Log_Observer.Present
      and then CuBit.Messages."/=" (Logstore, CuBit.Messages.No_Process));

   --  A service name, or a process written in decimal (its Image): the log
   --  source word the log protocol carries.
   --  A program by name: a running process whose name is Service, or
   --  whose name before its extension is ("rtw89" for rtw89.drv), or whose
   --  manifest identity ends in "." & Service (com.cubit.wifi); the
   --  newest such process. 0 when none runs (or the process list is not
   --  granted).
   function First_Dot (Name : String) return Natural is
   begin
      for C in Name'Range loop
         if Name (C) = '.' then
            return C;
         end if;
      end loop;
      return 0;
   end First_Dot;

   function Program_Source (Service : String) return Unsigned_64 is
      Found : Unsigned_64 := 0;
      Newest : Unsigned_64 := Unsigned_64'Last;
      Seen, Total : Natural;
      Result : CCL_Processes.Result_Kind;
      Suffix : constant String := "." & Service;
      --  The newest process whose name, name stem or manifest identity's
      --  last part is Service.
      procedure Consider (Item : CCL_Processes.Listed) is
         Name : constant String := Item.Name (1 .. Item.Name_Length);
         Identity : constant String := Item.Identity (1 .. Item.Identity_Length);
         Dot : constant Natural := First_Dot (Name);
         Stem : constant String := (if Dot = 0 then Name else Name (Name'First .. Dot - 1));
      begin
         if (Name = Service or else Stem = Service
             or else (Identity'Length > Suffix'Length
                      and then Identity (Identity'Last - Suffix'Length + 1 .. Identity'Last) = Suffix))
           and then Item.Age_Ms <= Newest
         then
            Found := Item.Process;
            Newest := Item.Age_Ms;
         end if;
      end Consider;
      procedure Search is new CCL_Processes.Each (Consider);
   begin
      if Service'Length = 0 then
         return 0;
      end if;
      --  Every process is looked at (a part read while the table changed
      --  still names the ones it saw).
      Search (Seen, Total, Result);
      return Found;
   end Program_Source;

   function Source_Of (Service : String) return Unsigned_64 is
      Process : CuBit.Messages.Process_ID;
      Valid : Boolean;
   begin
      CuBit.Process_IDs.Parse (Service, Process, Valid);
      if Valid then
         --  A pid as :ps shows it (its low 32 bits): the running process it
         --  names, by its whole identity.
         declare
            Word : constant Unsigned_64 := CuBit.Messages.To_Word (Process);
            Whole : Unsigned_64 := Word;
            Seen, Total : Natural;
            Result : CCL_Processes.Result_Kind;
            procedure Consider (Item : CCL_Processes.Listed) is
            begin
               if Unsigned_64 (Item.Pid) = Word then
                  Whole := Item.Process;
               end if;
            end Consider;
            procedure Search is new CCL_Processes.Each (Consider);
         begin
            if Word <= Unsigned_64 (Unsigned_32'Last) then
               Search (Seen, Total, Result);
            end if;
            return Whole;
         end;
      end if;
      Process := CuBit.Service_Names.Process_Of (Service);
      if CuBit.Messages."/=" (Process, CuBit.Messages.No_Process) then
         return CuBit.Messages.To_Word (Process);
      end if;
      return Program_Source (Service);
   end Source_Of;

   function Level_Of (Level : L.Severity) return Logs.Severity is
     (case Level is
         when L.Trace => Logs.Trace, when L.Debug => Logs.Debug,
         when L.Information => Logs.Information, when L.Warning => Logs.Warning,
         when L.Error => Logs.Error, when L.Critical => Logs.Critical);

   --  A log store status as a failure: Denied is a grant question, the
   --  rest say what the store reported.
   function Refusal (Status : P.Status; Context : String) return Failures.Failure is
     (case Status is
         when P.Denied => Failures.Failed
           (Failures.Not_Granted, Context,
            "the program's manifest must request the log-observer service " &
            "(request-service log-observer read-write log-observer)"),
         when P.Exhausted | P.Rate_Limited => Failures.Failed
           (Failures.Exhausted, Context & " (" & Spelling (Status) & ")",
            "close another log subscription or try again shortly"),
         when P.Unavailable => Failures.Failed
           (Failures.Unavailable, Context & " (the log store is not running)"),
         when others => Failures.Failed
           (Failures.Refused, Context & " (" & Spelling (Status) & ")"));

   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Image; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
      Reader : CuBit.Logging.Reader := CuBit.Logging.Bound_To (CCL_Native_Bindings.Log_Observer);
      Result : P.Status;
      Value : P.Event;
      Lost : Unsigned_64;
      --  The sources Service names, newest instance first (LOG-003): every
      --  process that carried the name during this boot, live or exited,
      --  as logstore's launchers recorded them, then a running process the
      --  name resolves to now. A result holds at most MAX_ENTRIES records,
      --  so more instances could add nothing.
      Instances : CuBit.Logging.Source_Entries (1 .. Logs.MAX_ENTRIES);
      Sources : array (1 .. Logs.MAX_ENTRIES) of Unsigned_64 := [others => 0];
      Source_Count : Natural range 0 .. Logs.MAX_ENTRIES := 0;
      Found : Natural;
      Process : CuBit.Messages.Process_ID;
      Is_Number : Boolean;
      --  Records kept, Held (1 .. Used): each instance's newest that still
      --  fit, newest instance first; Order then puts them oldest first.
      Held : array (1 .. Logs.MAX_ENTRIES) of P.Event;
      Used : Natural range 0 .. Logs.MAX_ENTRIES := 0;
      Order : array (1 .. Logs.MAX_ENTRIES) of Positive;
      Closed : P.Status;
      Added : Boolean;

      procedure Add_Source (Source : Unsigned_64) is
      begin
         if Source /= 0 and then Source_Count < Logs.MAX_ENTRIES
           and then (for all K in 1 .. Source_Count => Sources (K) /= Source)
         then
            Source_Count := Source_Count + 1;
            Sources (Source_Count) := Source;
         end if;
      end Add_Source;

      --  Source's retained records into Held (Used + 1 .. ): the newest
      --  Room of them, oldest first.
      procedure Replay (Source : Unsigned_64; Ok : out Boolean) is
         Room : constant Positive := Logs.MAX_ENTRIES - Used;
         Base : constant Natural := Used;
         Next : Positive := 1;
         Count : Natural := 0;
      begin
         CuBit.Logging.Subscribe (Reader, Result, Source => Source);
         Ok := Result = P.OK;
         if not Ok then
            return;
         end if;
         --  The replay holds at most a queue's worth of records; the region
         --  Held (Base + 1 .. Base + Room) is a ring until it ends.
         for Step in 1 .. P.Observer_Queue_Records + 1 loop
            CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
            exit when Result not in P.OK | P.Gap;
            if Result = P.OK then
               Held (Base + Next) := Value;
               Next := (if Next = Room then 1 else Next + 1);
               Count := Natural'Min (Count + 1, Room);
            end if;
         end loop;
         Ok := Result = P.Empty;
         CuBit.Logging.Close (Reader, Closed);
         if not Ok then
            return;
         end if;
         --  Unwind the ring: oldest first.
         if Count = Room and then Next /= 1 then
            declare
               Ring : constant array (1 .. Room) of P.Event := [for I in 1 .. Room => Held (Base + I)];
            begin
               for I in 1 .. Room loop
                  Held (Base + I) := Ring ((Next + I - 2) mod Room + 1);
               end loop;
            end;
         end if;
         Used := Used + Count;
      end Replay;
   begin
      Logs.Start (Contract, Image);
      Why := (others => <>);
      CuBit.Process_IDs.Parse (Service, Process, Is_Number);
      if not Is_Number then
         CuBit.Logging.Find_Sources (Reader, Service, Instances, Found, Result);
         if Result = P.OK then
            for I in 1 .. Found loop
               Add_Source (Instances (I).Who.Source);
            end loop;
         end if;
      end if;
      Add_Source (Source_Of (Service));
      Success := Source_Count > 0;
      if not Success then
         Why := Failures.Failed
           (Failures.Not_Found, "no service or program named """ & Service & """ has run during this boot",
            "name a service or program (rtw89, wifi, acpi), or give its process number");
         return;
      end if;
      for I in 1 .. Source_Count loop
         exit when Used = Logs.MAX_ENTRIES;
         Replay (Sources (I), Success);
         if not Success then
            Why := Refusal (Result, "the log store would not replay " & Service);
            return;
         end if;
      end loop;
      --  Oldest first across instances (stable: equal times keep their order).
      for I in 1 .. Used loop
         Order (I) := I;
         for J in reverse 1 .. I - 1 loop
            exit when Held (Order (J)).Monotonic_Ms <= Held (Order (J + 1)).Monotonic_Ms;
            declare
               Swap : constant Positive := Order (J);
            begin
               Order (J) := Order (J + 1);
               Order (J + 1) := Swap;
            end;
         end loop;
      end loop;
      --  Add stops when the image is full; the records it drops are then
      --  the newest, so keep the newest instead: skip the oldest until the
      --  rest fit.
      declare
         Text_Room : Natural := CCL.Objects.Maximum_Text_Bytes;
         Skip : Natural := Used;
      begin
         for Back in reverse 1 .. Used loop
            declare
               Length : constant Natural := L.Text (Held (Order (Back)).Data)'Length;
            begin
               exit when Length > Text_Room;
               Text_Room := Text_Room - Length;
               Skip := Skip - 1;
            end;
         end loop;
         for Position in Skip + 1 .. Used loop
            declare
               Item : P.Event renames Held (Order (Position));
            begin
               Logs.Add (Image, Item.Monotonic_Ms, Level_Of (L.Level (Item.Data)), Item.Source,
                         L.Text (Item.Data), Added);
               exit when not Added;
            end;
         end loop;
      end;
   end Recent;

   procedure Minimum
     (Level : out Logs.Severity; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Kept : L.Severity;
      Result : P.Status;
   begin
      CuBit.Logging.Get_Minimum
        (Kept, Result,
         (if CCL_Native_Bindings.Logstore.Present
          then CuBit.Handles.Services.Logstore.Handle (CCL_Native_Bindings.Logstore.Value)
          else CuBit.Handles.No_Endpoint));
      Level := Logs.Severity'Val (L.Severity'Pos (Kept));
      Success := Result = P.OK;
      Why := (if Success then (others => <>)
              else Refusal (Result, "the log store would not say what it keeps"));
   end Minimum;

   procedure Set_Minimum
     (Level : Logs.Severity; Previous : out Logs.Severity;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Before : L.Severity;
      Result : P.Status;
   begin
      CuBit.Logging.Set_Minimum (L.Severity'Val (Logs.Severity'Pos (Level)), Before, Result, CCL_Native_Bindings.Log_Control);
      Previous := Logs.Severity'Val (L.Severity'Pos (Before));
      Success := Result = P.OK;
      if Success then
         Why := (others => <>);
      elsif Result in P.Denied | P.Unavailable then
         --  No log-control endpoint in its slot reads as unavailable too.
         Why := Failures.Failed
           (Failures.Not_Granted, "changing what the log store keeps is not granted to this program",
            "the program's manifest must request the log-control service " &
            "(request-service log-control read-write log-control)");
      else
         Why := Refusal (Result, "the log store did not change what it keeps");
      end if;
   end Set_Minimum;
end CCL_Log_IO;
