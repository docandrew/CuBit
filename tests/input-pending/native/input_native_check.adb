with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Input;
with Input_Pending;
procedure Input_Native_Check is
   package P renames Input_Pending;
   package I renames CuBit.Input;
   use type Activity_Result;
   use type P.Item;
   PID : constant Process_ID := syscall (SYSCALL_GETPID);
   -- The inherited slot-0 self endpoint is selected before any later slot.
   Authority : constant Unsigned_64 := PID;
   Ignore : Unsigned_64;
   procedure Check (OK : Boolean; Detail : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL native input retention: " & Detail & ASCII.LF);
         loop Ignore := syscall (SYSCALL_SLEEP, 1_000); end loop;
      end if;
   end Check;
   function Packet (Number : Positive; Overflow : Boolean) return Unsigned_64 is
     (if not Overflow and Number > 4 then (if Number = 5 then 1 else 0)
      else 16#FBA# * 256);
   function Encode (Value : P.Item) return Message is
     (I.Encode ((sourceAuthorityTag => 0, sequence => Value.Sequence,
        generation => 1, device => I.RELATIVE_POINTER,
        delivery => I.ACCUMULABLE_DISPLACEMENT,
        flags => [I.RESYNCHRONIZE => Value.Recover],
        payload => Value.Payload, snapshot => Value.Payload and 16#FF#)));
   procedure Run (Overflow : Boolean) is
      Queue : P.Queue;
      Msg, Received : Message := NULL_MESSAGE;
      Report : I.Source_Report;
      Valid, Lost, Refused : Boolean := False;
      Filled, Drained, Delivered : Natural := 0;
      Number : Natural;
      Before_Drops : constant Unsigned_64 := getInfo (1401);
      Deadline : Unsigned_64;
      Count : constant Positive := (if Overflow then 40 else 6);
      First : constant Positive := (if Overflow then 33 else 1);
      Saved : P.Item;
      Position : Integer := 360;
      Activity : Activity_Result;
   begin
      Check (not Poll_Event (Received), "initial mailbox not empty");
      Msg.tag := (16#0B00#, 1, 0, 0);
      -- Refusal must come from a genuinely full mailbox, not absent authority.
      for Attempt in 1 .. 4096 loop
         Msg.words (0) := Unsigned_64 (Attempt);
         if not trySendEvent (PID, Msg) then Refused := True; exit; end if;
         Filled := Filled + 1;
      end loop;
      Check (Refused and Filled > 0, "did not saturate mailbox");
      for N in 1 .. Count loop
         P.Append (Queue, Packet (N, Overflow), Lost);
         Check (Lost = (Overflow and N = 33), "local overflow boundary");
         Saved := P.Element (Queue, 0);
         Check (not trySendEvent (PID, Encode (Saved)), "full mailbox admitted report");
         Check (P.Element (Queue, 0) = Saved, "refusal changed pending head");
      end loop;
      Check (getInfo (1401) - Before_Drops = Unsigned_64 (Count + 1),
             "mailbox rejection metric");
      -- Act as receiver, freeing capacity without any further source report.
      for N in 1 .. Filled loop
         Check (Poll_Event (Received), "missing saturation packet");
         Check (Received.tag.label = 16#0B00# and
           Received.words (0) = Unsigned_64 (N), "saturation order");
         Check (Received.authorityTag = Authority, "filler authority stamp");
         Drained := Drained + 1;
      end loop;
      Check (Drained = Filled and not Poll_Event (Received), "mailbox not drained");
      Deadline := P.Wake_Deadline (Queue, syscall (SYSCALL_GETTIME), Unsigned_64'Last);
      Activity := Wait_For_Activity_Until (Deadline);
      Check (Activity = Deadline_Reached, "final retry did not wake on deadline");
      Check (syscall (SYSCALL_GETTIME) >= Deadline, "early retry deadline");
      -- Bounded publication, exactly as the driver: acknowledge acceptance only.
      for Attempt in 1 .. P.Capacity loop
         exit when P.Count (Queue) = 0;
         Saved := P.Element (Queue, 0);
         Check (trySendEvent (PID, Encode (Saved)), "retry after space rejected");
         P.Acknowledge (Queue);
         Check (Poll_Event (Received), "accepted report missing");
         I.Decode (Received, Report, Valid);
         Number := First + Delivered;
         Check (Valid and Report.sourceAuthorityTag = Authority, "typed authority");
         Check (Report.sequence = Unsigned_64 (Number), "source sequence/order");
         Check (Report.payload = Packet (Number, Overflow), "displacement/button payload");
         Check (Report.snapshot = (Report.payload and 16#FF#), "button snapshot");
         Check (Report.flags (I.RESYNCHRONIZE) = (Overflow and Delivered = 0),
                "false or missing recovery");
         if Number <= 4 or Overflow then Position := Position - 70; end if;
         Delivered := Delivered + 1;
      end loop;
      Check (Delivered = Count - First + 1 and P.Count (Queue) = 0,
             "retained report count");
      Check (not Poll_Event (Received), "duplicate report");
      Check (Position = (if Overflow then -200 else 80), "retained displacement sum");
      debugPrint ("native input retention: " &
        (if Overflow then "overflow" else "transient") &
        " mailbox=" & Filled'Image & " refused=" & Count'Image &
        " delivered=" & Delivered'Image & " PASS" & ASCII.LF);
   end Run;
begin
   debugPrint ("native input retention: real loopback IPC (NO HID/ISOLATION)" & ASCII.LF);
   Check (PID /= No_Process, "self identity");
   Run (False);
   Run (True);
   debugPrint ("TEST: PASS native input retention mailbox/refusal/deadline/authority" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1_000); end loop;
end Input_Native_Check;
