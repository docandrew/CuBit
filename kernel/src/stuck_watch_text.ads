------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The words of the stuck-call watch (Process.IPC.reportStuckCalls), kept
--  apart so a hosted test can check how a row reads.
--
--  A watch row names a thread, its state and the call it is in or last
--  made. The state and the call must not be confusable: a server idle in its
--  own receive whose last call was answered long ago once read as "blocked
--  on that call" (display-stall, 2026-11-26). So:
--  - a thread waiting in receive reads "idle-recv";
--  - a call still unanswered reads "in-call ->", an answered one
--    "last-call ->";
--  - the watch says why it printed at all.
------------------------------------------------------------------------------
pragma Ada_2022;

package Stuck_Watch_Text with SPARK_Mode, Pure is

   --  The thread's state, as the watch tells it apart.
   type Wait_Class is
     (Running, Ready, Sleeping, Waiting, Waiting_For_Event, Queued_In_Call,
      Waiting_For_Reply, Idle_Receive, Waiting_For_Completion, Suspended,
      Futex_Waiting, Guest_Halted, Gone);

   function State_Word (Class : Wait_Class) return String is
     (case Class is
        when Gone                   => "gone",
        when Ready                  => "ready",
        when Running                => "running",
        when Sleeping               => "sleep",
        when Waiting                => "wait",
        when Waiting_For_Event      => "event",
        when Queued_In_Call         => "send",
        when Idle_Receive           => "idle-recv",
        when Waiting_For_Reply      => "call",
        when Waiting_For_Completion => "compl",
        when Suspended              => "susp",
        when Futex_Waiting          => "futex",
        when Guest_Halted           => "guest-halted");

   --  Whether the thread is inside a synchronous call right now.
   function In_Call (Class : Wait_Class) return Boolean is
     (Class in Queued_In_Call | Waiting_For_Reply);

   --  The words before the server's name: the call it is in, else the
   --  last one it made (answered or abandoned, no longer waited on).
   function Call_Words (Class : Wait_Class) return String is
     (if In_Call (Class) then " in-call ->" else " last-call ->");

   --  Why a watch report printed: a thread stuck in a call for the stuck
   --  time, a watched thread hot for consecutive reports, a watched main
   --  thread waiting undispatched for the stuck time.
   type Reasons is record
      Stuck_Call : Boolean := False;
      Hot_Thread : Boolean := False;
      Quiet_Main : Boolean := False;
   end record;

   function Reason_Words (R : Reasons) return String is
     ((if R.Stuck_Call then " stuck-call" else "") &
      (if R.Hot_Thread then " hot-thread" else "") &
      (if R.Quiet_Main then " quiet-main-thread" else ""));

   --  The mark after a row's thread when that thread fired the watch.
   function Fired_Words (Hot, Quiet_Main : Boolean) return String is
     ((if Hot then " FIRED:hot" else "") &
      (if Quiet_Main then " FIRED:quiet-main" else ""));

end Stuck_Watch_Text;
