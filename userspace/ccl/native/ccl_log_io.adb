with Interfaces; use Interfaces;
with CuBit.Failures;
with CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Service_Names;
with CCL.Interfaces.Logs;

--  logs.recent on CuBit: a subscription to logstore filtered to one process,
--  which replays that process's retained records; the newest that fit one
--  result are kept, and the subscription is closed again.
package body CCL_Log_IO is
   procedure Announce (Text : String) is
      Published : Boolean;
   begin
      CuBit.Logging.Announce (Text, Published);
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

   function Logstore return Unsigned_64 is (CuBit.Service_Names.Process_Of ("logstore"));

   function Available return Boolean is
      Slot : CuBit.Messages.CapabilitySlot;
      Found : Boolean;
   begin
      if Logstore = 0 then return False; end if;
      CuBit.Messages.Find_Endpoint_Capability (CuBit.Messages.ProcessID (Logstore), Slot, Found);
      return Found;
   end Available;

   --  A service name, or a process number written in decimal.
   function Source_Of (Service : String) return Unsigned_64 is
      Number : Unsigned_64 := 0;
   begin
      if Service'Length = 0 then return 0; end if;
      for C of Service loop
         if C not in '0' .. '9' or else Number > (Unsigned_64'Last - 9) / 10 then
            return CuBit.Service_Names.Process_Of (Service);
         end if;
         Number := Number * 10 + Character'Pos (C) - Character'Pos ('0');
      end loop;
      return Number;
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
      Reader : CuBit.Logging.Reader;
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
           (Failures.Not_Found, "no running service is named """ & Service & """",
            "name a running service, or give its process number");
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
      CuBit.Logging.Get_Minimum (Kept, Result);
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
      CuBit.Logging.Set_Minimum (L.Severity'Val (Logs.Severity'Pos (Level)), Before, Result);
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
