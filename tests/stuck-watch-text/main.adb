-------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How a stuck-call watch row reads (kernel Stuck_Watch_Text).
--
--  The display-stall misreading (2026-11-26): the row
--    STUCK-WATCH: * display.svc(31) t31 recv cpu 0% quiet 56s rip=... ->virtio-gpu.drv(22) lbl=00000A0D
--  was read as "display.svc blocked 56 s on a virtio-gpu Identify call",
--  but display.svc sat idle in its own receive; the call was its last one,
--  answered at startup. Rows of that shape must now say so, and say why
--  the watch printed at all.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Stuck_Watch_Text; use Stuck_Watch_Text;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Has (Text, Part : String) return Boolean is
     (Ada.Strings.Fixed.Index (Text, Part) /= 0);

   --  A watch row as Process.IPC.reportStuckCalls builds it.
   function Row (Class : Wait_Class; Hot, Quiet : Boolean) return String is
     ("* display.svc(31) t31 " & State_Word (Class) &
      " cpu 0% quiet 56s rip=000000000042F3F6" & Fired_Words (Hot, Quiet) &
      Call_Words (Class) & "virtio-gpu.drv(22) lbl=00000A0D");

   Idle : constant String := Row (Idle_Receive, False, False);
   Calling : constant String := Row (Waiting_For_Reply, False, False);
   Queued : constant String := Row (Queued_In_Call, False, False);
begin
   --  The misread shape: idle in receive, last call answered.
   Check (Has (Idle, " idle-recv "), "an idle receive reads idle-recv: " & Idle);
   Check (Has (Idle, " last-call ->virtio-gpu.drv"), "an answered call reads last-call: " & Idle);
   Check (not Has (Idle, "in-call"), "an idle thread is not in a call: " & Idle);
   Check (not Has (Idle, " recv cpu"), "no bare recv state left: " & Idle);

   --  A thread really waiting for its answer, or queued at the server.
   Check (Has (Calling, " call ") and then Has (Calling, " in-call ->virtio-gpu.drv"),
          "a pending call reads in-call: " & Calling);
   Check (Has (Queued, " send ") and then Has (Queued, " in-call ->virtio-gpu.drv"),
          "a queued call reads in-call: " & Queued);
   for Class in Wait_Class loop
      Check (In_Call (Class) = (Class in Queued_In_Call | Waiting_For_Reply),
             "in call only while sending or waiting for the reply: " & Class'Image);
      Check (State_Word (Class)'Length > 0, "every state has a word: " & Class'Image);
      for Other in Wait_Class loop
         Check (Class = Other or else State_Word (Class) /= State_Word (Other),
                "state words are distinct: " & Class'Image & " " & Other'Image);
      end loop;
   end loop;

   --  Why it printed: each reason named, none invented.
   Check (Reason_Words ((others => False)) = "", "no reason, no words");
   Check (Reason_Words ((Stuck_Call => True, others => False)) = " stuck-call", "stuck call");
   Check (Reason_Words ((Hot_Thread => True, others => False)) = " hot-thread", "hot thread");
   Check (Reason_Words ((Quiet_Main => True, others => False)) = " quiet-main-thread", "quiet main");
   Check (Reason_Words ((others => True)) = " stuck-call hot-thread quiet-main-thread", "all reasons");

   --  The thread that fired is marked; context rows are not.
   Check (not Has (Idle, "FIRED"), "a context row carries no mark: " & Idle);
   Check (Has (Row (Futex_Waiting, False, True), " FIRED:quiet-main "), "quiet main thread marked");
   Check (Has (Row (Running, True, False), " FIRED:hot "), "hot thread marked");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("stuck-watch-text: PASS");
      Ada.Text_IO.Put_Line ("  " & Idle);
      Ada.Text_IO.Put_Line ("  " & Calling);
   else
      Ada.Text_IO.Put_Line ("stuck-watch-text: FAIL (" & Failures'Image & " )");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
