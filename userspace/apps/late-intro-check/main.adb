------------------------------------------------------------------------------
--  late-intro-check (ABI-002): late introduction of an optional service.
--
--  Started before logstore (tests/headless/init-late-introduction.ccl), so
--  its optional "logstore" entry is Absent. It routes its debug text by
--  that entry (CuBit.Console_Route.Route_Named): the lines queue, and a
--  thread of its own waits for procmgr to introduce logstore
--  (CuBit.Introductions). It also asks procmgr, over the same introducer,
--  for an entry procmgr kept no decision for (its introducer entry itself,
--  which is Granted): procmgr must refuse it (Not_Declared). The markers
--  reach the serial console only through logstore's test mirror, so they
--  show the introduction happened.
--
--  Nothing lost (no lost events): Lines_Each_Side numbered lines queued
--  before the introduction and as many published after it, all of which
--  must reach logstore while the program still runs (the headless check
--  wants each before the kernel reclaims the process), then a pause before
--  it exits, so a line that only an exit's last drain delivers fails.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Console_Route;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Late_Introduction;
with CuBit.Monotonic;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with CuBit.Waits;

procedure Main is
   package LI renames CuBit.Late_Introduction;
   package ST renames CuBit.Startup_Tables;
   --  How long the introduction may take (logstore starts right after).
   Attach_Deadline_Ms : constant := 20_000;

   --  The answer to an Await for an index procmgr kept nothing for.
   function Undeclared_Answer return String is
      Via : constant CuBit.Handles.Startup.Optional_Endpoint :=
        CuBit.Handles.Startup.Optional_Endpoint_Named (LI.Binding);
      Index : constant ST.Entry_Count :=
        ST.Find (CuBit.Startup_State.Current, ST.To_Name (LI.Binding));
      Request : Message := NULL_MESSAGE;
      Received : CuBit.Handles.IPC.Carried;
      Tag : MessageTag;
      use type CuBit.Handles.IPC.Carried_Kind;
   begin
      if not Via.Present or else Index = 0 then
         return "NO_INTRODUCER";
      end if;
      Request.tag := (label => LI.Await_Request, length => LI.Await_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (LI.Index_Word) := Unsigned_64 (Index);
      Tag := CuBit.Handles.IPC.Call_Accepting
        (Via.Value, Request, CuBit.Messages.Deadline_After (5_000), Received);
      if Received.Kind = CuBit.Handles.IPC.Present then
         return "CARRIED_A_CAPABILITY";
      elsif Tag.label = LI.Not_Declared then
         return "NOT_DECLARED";
      elsif Tag.label = LI.Introduced then
         return "INTRODUCED";
      else
         return "OTHER";
      end if;
   end Undeclared_Answer;

   Lines_Each_Side : constant := 20;
   --  Alive after the last line: logstore's passes, not the exit, deliver.
   Linger_Ms : constant := 2_000;

   procedure Numbered (Side : String) is
   begin
      for N in 1 .. Lines_Each_Side loop
         debugPrint ("LATE-INTRO: " & Side & Integer'Image (N) & ASCII.LF);
      end loop;
   end Numbered;

   Start : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
   Ignore : Unsigned_64;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   if not CuBit.Console_Route.Routed then
      --  Only without late introduction: straight to the kernel console.
      debugPrint ("LATE-INTRO: not routed" & ASCII.LF);
      return;
   end if;
   debugPrint ("LATE-INTRO: queued "
               & (if CuBit.Console_Route.Awaiting then "before" else "after") & " logstore" & ASCII.LF);
   Numbered ("before");
   debugPrint ("LATE-INTRO: undeclared " & Undeclared_Answer & ASCII.LF);
   while CuBit.Console_Route.Awaiting
     and then CuBit.Monotonic.Milliseconds - Start < Attach_Deadline_Ms
   loop
      Ignore := CuBit.Waits.Sleep_For (10);
   end loop;
   debugPrint ("LATE-INTRO: "
               & (if CuBit.Console_Route.Awaiting then "never attached" else "attached")
               & ASCII.LF);
   Numbered ("after");
   debugPrint ("LATE-INTRO: lost=" & Unsigned_64'Image (CuBit.Console_Route.Lost) & ASCII.LF);
   Ignore := CuBit.Waits.Sleep_For (Linger_Ms);
   debugPrint ("LATE-INTRO: exiting" & ASCII.LF);
   CuBit.Console_Route.Flush;
end Main;
