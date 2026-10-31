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

--  logs.recent on CuBit: a subscription to logstore filtered to one process,
--  which replays that process's retained records; the newest that fit one
--  result are kept, and the subscription is closed again.
package body CCL_Log_IO is
   procedure Announce (Text : String) is
      Published : Boolean;
   begin
      if CCL_Native_Bindings.Logstore.Present then
         CuBit.Logging.Announce (Text, Published, CCL_Native_Bindings.Logstore.Value);
      end if;
   end Announce;
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
      Entries : CCL_Processes.Listing;
      Count : CCL_Processes.Listed_Count;
      Total : Natural;
      Result : CCL_Processes.Result_Kind;
      Found : Unsigned_64 := 0;
      Newest : Unsigned_64 := Unsigned_64'Last;
      use type CCL_Processes.Result_Kind;
   begin
      if Service'Length = 0 then
         return 0;
      end if;
      CCL_Processes.List (Entries, Count, Total, Result);
      if Result /= CCL_Processes.Listed_All then
         return 0;
      end if;
      for I in 1 .. Count loop
         declare
            Name : constant String := Entries (I).Name (1 .. Entries (I).Name_Length);
            Identity : constant String := Entries (I).Identity (1 .. Entries (I).Identity_Length);
            Dot : constant Natural := First_Dot (Name);
            Stem : constant String := (if Dot = 0 then Name else Name (Name'First .. Dot - 1));
            Suffix : constant String := "." & Service;
         begin
            if (Name = Service or else Stem = Service
                or else (Identity'Length > Suffix'Length
                         and then Identity (Identity'Last - Suffix'Length + 1 .. Identity'Last) = Suffix))
              and then Entries (I).Age_Ms <= Newest
            then
               Found := Entries (I).Process;
               Newest := Entries (I).Age_Ms;
            end if;
         end;
      end loop;
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
            Entries : CCL_Processes.Listing;
            Count : CCL_Processes.Listed_Count;
            Total : Natural;
            Result : CCL_Processes.Result_Kind;
            use type CCL_Processes.Result_Kind;
         begin
            if Word <= Unsigned_64 (Unsigned_32'Last) then
               CCL_Processes.List (Entries, Count, Total, Result);
               if Result = CCL_Processes.Listed_All then
                  for I in 1 .. Count loop
                     if Unsigned_64 (Entries (I).Pid) = Word then
                        return Entries (I).Process;
                     end if;
                  end loop;
               end if;
            end if;
            return Word;
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
      Source : constant Unsigned_64 := Source_Of (Service);
      Reader : CuBit.Logging.Reader := CuBit.Logging.Bound_To (CCL_Native_Bindings.Log_Observer);
      Result : P.Status;
      Value : P.Event;
      Lost : Unsigned_64;
      --  The newest records seen, oldest first once the replay ends.
      Ring : array (1 .. Logs.MAX_ENTRIES) of P.Event;
      Next : Positive range 1 .. Logs.MAX_ENTRIES := 1;
      Held : Natural range 0 .. Logs.MAX_ENTRIES := 0;
      Closed : P.Status;
      Added : Boolean;
   begin
      Logs.Start (Contract, Image);
      Success := Source /= 0;
      Why := (others => <>);
      if not Success then
         Why := Failures.Failed
           (Failures.Not_Found, "no running service or program is named """ & Service & """",
            "name a running service or program (rtw89, wifi), or give its process number");
         return;
      end if;
      CuBit.Logging.Subscribe (Reader, Result, Source => Source);
      Success := Result = P.OK;
      if not Success then
         Why := Refusal (Result, "the log store would not let this program observe " & Service);
         return;
      end if;
      --  The replay holds at most a queue's worth of records.
      for Step in 1 .. P.Observer_Queue_Records + 1 loop
         CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
         exit when Result not in P.OK | P.Gap;
         if Result = P.OK then
            Ring (Next) := Value;
            Next := (if Next = Logs.MAX_ENTRIES then 1 else Next + 1);
            Held := Natural'Min (Held + 1, Logs.MAX_ENTRIES);
         end if;
      end loop;
      Success := Result = P.Empty;
      CuBit.Logging.Close (Reader, Closed);
      if not Success then
         Why := Refusal (Result, "the log store stopped replaying " & Service);
         return;
      end if;
      --  Oldest held first. Add stops when the image is full; the records it
      --  drops are then the newest, so keep the newest instead: skip the
      --  oldest until the rest fit.
      declare
         First : constant Positive := (if Held < Logs.MAX_ENTRIES then 1 else Next);
         Text_Room : Natural := CCL.Objects.Maximum_Text_Bytes;
         Skip : Natural := Held;
      begin
         for Back in 0 .. Held - 1 loop
            declare
               Length : constant Natural :=
                 L.Text (Ring ((First + Held - 1 - Back - 1) mod Logs.MAX_ENTRIES + 1).Data)'Length;
            begin
               exit when Length > Text_Room;
               Text_Room := Text_Room - Length;
               Skip := Skip - 1;
            end;
         end loop;
         for Offset in Skip .. Held - 1 loop
            declare
               Item : P.Event renames Ring ((First + Offset - 1) mod Logs.MAX_ENTRIES + 1);
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
