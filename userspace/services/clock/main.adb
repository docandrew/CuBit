with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Clocks;
with CuBit.Clock_Control;
with CuBit.Logging;
with CuBit.Log_Records;
with Wall_Clock;

procedure Main is
   use ASCII;

   REPLY_OK    : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR : constant Unsigned_32 := 16#F001#;
   MAX_CCL_MILLISECONDS : constant Unsigned_64 :=
     Unsigned_64 (Integer_64'Last);

   From     : ProcessID;
   Request  : Message;
   Response : Message;
   Ignore   : Unsigned_64;
   Now_Ms   : Unsigned_64;
   Logger : CuBit.Logging.Publisher;
   Completion : CompletionEntry;
   Found, Handled, Submitted : Boolean;
begin
   debugPrint ("clock: starting" & LF);
   Wall_Clock.Initialize;
   Ignore := registerDriver (DRIVER_CLOCK);
   if Ignore = Unsigned_64'Last then
      debugPrint ("clock: registration failed" & LF);
      return;
   end if;
   debugPrint ("clock: registered" & LF);
   declare
      Entry_Value : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make ("clock: service ready");
   begin
      if Entry_Value.Success then
         CuBit.Logging.Emit (Logger, Entry_Value.Value, 1, Submitted);
      end if;
   end;

   loop
      if Poll_Completion (Completion'Address) = 1 then
         CuBit.Logging.Complete (Logger, Completion, Handled);
      end if;
      --  This clock service has no device/event subscriptions. Deliberately
      --  consume all IPC here so lifecycle notifications cannot cause a spin.
      Poll_Any_Ipc (From, Request, Found);
      if not Found then
         if Wait_For_Activity_Until (Unsigned_64'Last) = Unavailable then
            debugPrint ("clock: activity wait unavailable" & LF);
            return;
         end if;
      else
         Response := NULL_MESSAGE;
         if Request.tag.label = CLOCK_OP_MONOTONIC_MS and then
           Request.tag.length = 1 and then Request.words (0) = 0
         then
            Now_Ms := syscall (SYSCALL_GETTIME);
            Response.tag :=
              (label => REPLY_OK, length => 1, flags => 0, reserved => 0);
            Response.words (0) := Unsigned_64'Min
              (Now_Ms, MAX_CCL_MILLISECONDS);
         elsif Request.tag.label = CuBit.Clock_Control.Submit_Sample then
            --  Adjustment requires the separately minted control endpoint;
            --  the kernel stamps authorityTag, so callers cannot forge it.
            declare
               Candidate : CuBit.Clock_Control.Sample;
               Decoded : Boolean := False;
               Result : CuBit.Clock_Control.Outcome;
               Quality : CuBit.Clocks.Time_Quality;
               use type CuBit.Clock_Control.Outcome;
            begin
               if Request.authorityTag = CuBit.Clock_Control.Authority_Tag and then
                 Request.tag.length = 4 and then Request.tag.flags = 0 and then
                 Request.tag.reserved = 0
               then
                  CuBit.Clock_Control.Decode
                    ([Request.words (0), Request.words (1),
                      Request.words (2), Request.words (3)],
                     Candidate, Decoded);
               end if;
               if Decoded then
                  Wall_Clock.Adjust (Candidate, Result, Quality);
                  Response.tag :=
                    (label => REPLY_OK, length => 2, flags => 0, reserved => 0);
                  Response.words (0) :=
                    Unsigned_64 (CuBit.Clock_Control.Outcome'Enum_Rep (Result));
                  Response.words (1) :=
                    Unsigned_64 (CuBit.Clocks.Time_Quality'Enum_Rep (Quality));
                  debugPrint ((if Result = CuBit.Clock_Control.Stepped
                               then "clock: wall time set from sample"
                               else "clock: time sample rejected: " &
                                    CuBit.Clock_Control.Name (Result)) & LF);
               else
                  debugPrint ("clock: adjustment denied" & LF);
                  Response.tag :=
                    (label => REPLY_ERROR, length => 0, flags => 0, reserved => 0);
               end if;
            end;
         elsif Request.tag.label = CuBit.Clocks.Snapshot_Operation and then
           Request.tag.length = 0 and then Request.tag.flags = 0 and then
           Request.tag.reserved = 0 and then Request.words = [0, 0, 0, 0]
         then
            Response := Wall_Clock.Snapshot;
         else
            Response.tag :=
              (label => REPLY_ERROR, length => 0, flags => 0, reserved => 0);
         end if;
         Ignore := reply (From, Response);
      end if;
   end loop;
end Main;
