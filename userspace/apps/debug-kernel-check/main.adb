------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger engine through the kernel (DBG-001, CAP-012; headless --test
--  debug-kernel). debug-plain has no stub: launched into the user session,
--  its launch handle's Debug right gets Debug_Observe from procmgr, and the
--  engine reads its threads and wait reasons, unwinds frames from the
--  registers the kernel saved, reads locals and a global through views,
--  and sees the global move. Then clock.svc, a system service started by
--  the plan, through the session's debug authority: its threads and frames.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Debug; use CuBit.Debug;
with CuBit.Debug_Target;
with CuBit.Handles;
with CuBit.Handles.Debug;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Kernel_ABI;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launch_Handles;
with CuBit.Launching;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Waits;
with Debug_ABI;
with Startup;

procedure Main is
   package LH renames CuBit.Launch_Handles;
   package HD renames CuBit.Handles.Debug;
   package IPC renames CuBit.Handles.IPC;
   package DT renames CuBit.Debug_Target;
   use type LH.Outcome, DT.Wait_Code, CuBit.Launching.Launch_Result;

   Attach_Ms     : constant := 4_000;
   Mint_Ms       : constant := 2_000;
   Settle_Ms     : constant := 500;
   Run_Between_Ms : constant := 900;
   Exit_Failure  : constant := 1;
   Memory_Bytes  : constant := 16;

   Failures : Natural := 0;

   procedure Say (Text : String) is
   begin
      debugPrint ("debug-kernel: " & Text & ASCII.LF);
   end Say;

   procedure Check (Name : String; Condition : Boolean; Detail : String := "") is
   begin
      if Condition then
         Say (Name & " PASS");
      else
         Failures := Failures + 1;
         Say (Name & " FAIL " & Detail);
      end if;
   end Check;

   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   Plain : CuBit.Launching.Child;
   Launched : CuBit.Launching.Launch_Result;
   Failure : CuBit.Launch_Arguments.Launch_Failure;
   Minted : HD.Session;
   Mint_Outcome : LH.Outcome;
   S : Session;
   R : Outcome;
   Info : Program_Info;
   Ts : Thread_List;
   Ignore : Unsigned_64;

   --  The thread whose frames include Wanted; Frames into Fs.
   procedure Find_Frame
     (Wanted : String; Thread : out Thread_Id; Fs : out Frame_List; At_Frame : out Natural) is
   begin
      Thread := 0;
      At_Frame := Frame_Ceiling;
      Fs := (others => <>);
      for T of Ts.Items (1 .. Ts.Count) loop
         Frames (S, T.Id, Fs, R);
         Say ("frames of thread" & T.Id'Image & " (" & DT.Wait_Code'Image (T.Wait) & "): "
              & Outcome'Image (R) & Fs.Count'Image);
         for I in 0 .. Fs.Count - 1 loop
            Say ("  #" & I'Image & " " & Image (Fs.Items (I).Function_Name) & " at "
                 & Image (Fs.Items (I).File) & ":" & Fs.Items (I).Line'Image);
            if Image (Fs.Items (I).Function_Name) = Wanted and then At_Frame = Frame_Ceiling then
               At_Frame := I;
            end if;
         end loop;
         if At_Frame < Fs.Count then
            Thread := T.Id;
            return;
         end if;
      end loop;
   end Find_Frame;

   --  The display of local Name in frame F (empty when it has none).
   function Local_Display (F : Frame; Name : String) return String is
      Ls : Ref_List;
      Node : Value_Node;
   begin
      Locals (S, F, Ls, R);
      for L of Ls.Items (1 .. Ls.Count) loop
         Value (S, L, Node, R);
         Say ("    " & Image (L.Name) & " : " & Image (Node.Type_Name) & " = " & Image (Node.Display));
         if Image (L.Name) = Name then
            return Image (Node.Display);
         end if;
      end loop;
      return "";
   end Local_Display;

   function Rounds_Display (Totals : Value_Ref) return String is
      Kids : Node_List;
   begin
      Children (S, Totals, 0, 1, Kids, R);
      return (if Kids.Count = 1 then Image (Kids.Items (1).Display) else "");
   end Rounds_Display;

   --  The identity a Describe of the endpoint names (its server).
   function Server_Of (Item : CuBit.Handles.Endpoint) return Unsigned_64 is
      D : constant IPC.Description := IPC.Describe (Item);
   begin
      return (if D.Current then D.Object else 0);
   end Server_Of;

   procedure Session_Debug (Target : Unsigned_64; Session_Out : out HD.Session; Outcome : out LH.Outcome) is
      Badge : constant CuBit.Handles.Endpoint :=
        CuBit.Handles.Services.Process_Manager.Handle (Startup.Debug_Session);
      Reply : Message := NULL_MESSAGE;
      Received : IPC.Carried;
      Answer : MessageTag;
   begin
      Session_Out := HD.From_Word (0);
      Reply.tag := (label => LH.Session_Debug_Operation, length => LH.Session_Debug_Words,
                    protocol => 0, ipc => <>, reserved => 0);
      Reply.words := [Target, Debug_ABI.Kind_Word (Debug_ABI.Observe), 0, 0];
      Answer := IPC.Call_Accepting (Badge, Reply, Deadline_After (Mint_Ms), Received);
      Outcome := (if Answer.label = CuBit.Kernel_ABI.Reply_OK and then LH.Is_Outcome_Word (Reply.words (0))
                  then LH.To_Outcome (Reply.words (0)) else LH.Failed);
      if Outcome = LH.Done then
         declare
            Taken : constant HD.Accepted_Session := HD.Accept_Session (Received, Debug_ABI.Observe);
         begin
            if Taken.OK then
               Session_Out := Taken.Value;
            else
               Outcome := LH.Failed;
            end if;
         end;
      end if;
   end Session_Debug;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Say ("starting");
   CuBit.Launching.Launch (Launcher, "debug-plain.app", [], [], Plain, Launched, Failure);
   Check ("debug-plain (no stub) starts into the user session with a launch handle",
          Launched = CuBit.Launching.Launched and then CuBit.Handles.Valid (Plain.Handle)
          and then Plain.Rights (LH.Debug));
   if Launched /= CuBit.Launching.Launched then
      Say ("FAIL");
      Ignore := syscall (SYSCALL_EXIT, Exit_Failure);
   end if;
   CuBit.Waits.Sleep_For (Settle_Ms);

   CuBit.Launching.Debug (Plain, Deadline_After (Mint_Ms), Minted, Mint_Outcome);
   Check ("the launch handle's Debug right gets Debug_Observe from procmgr",
          Mint_Outcome = LH.Done and then HD.Valid (Minted), LH.Outcome'Image (Mint_Outcome));

   Attach ((Target => CuBit.Handles.No_Endpoint, Session => Minted, Filesystem => Startup.Filesystem),
           Observe, Deadline_After (Attach_Ms), S, R);
   Check ("attach to debug-plain through the kernel", R = Ok, Outcome'Image (R));
   if R = Ok then
      Describe (S, Info, R);
      Say ("program " & Image (Info.Program) & " build-id " & Info.Build_Id & " debug file "
           & Image (Info.Debug_File) & ", " & Image (Info.Attach_Mode));
      Check ("attach is through the kernel", Through_Kernel (S)
             and then Image (Info.Attach_Mode) = "kernel attach (Debug_Observe)",
             Image (Info.Attach_Mode));
      Check ("the build-id comes from the target's own memory", Info.Build_Id /= [Info.Build_Id'Range => '0']);

      Threads (S, Ts, R);
      Check ("debug-plain's thread is listed", R = Ok and then Ts.Count >= 1, Ts.Count'Image);
      for T of Ts.Items (1 .. Ts.Count) loop
         Say ("thread" & T.Id'Image & " " & Image (T.Name) & " waits " & DT.Wait_Code'Image (T.Wait));
      end loop;
      Check ("the thread waits Sleeping or on a futex",
             (for some T of Ts.Items (1 .. Ts.Count) => T.Wait in DT.Sleeping | DT.Futex));

      declare
         Fs : Frame_List;
         At_Frame : Natural;
         Thread : Thread_Id;
         Found_Run : Boolean := False;
      begin
         Find_Frame ("Plain_Work.Pause_Between", Thread, Fs, At_Frame);
         Check ("Plain_Work.Pause_Between is on the stack in plain_work.adb, unwound from the kernel's registers",
                At_Frame < Fs.Count and then Image (Fs.Items (At_Frame).File) = "plain_work.adb"
                and then Fs.Items (At_Frame).Line > 0);
         for I in 0 .. Fs.Count - 1 loop
            Found_Run := Found_Run or else Image (Fs.Items (I).Function_Name) = "Plain_Work.Run";
         end loop;
         Check ("its caller Plain_Work.Run is on the stack too", Found_Run);
         if At_Frame < Fs.Count then
            declare
               Text : constant String := Local_Display (Fs.Items (At_Frame), "Remaining");
            begin
               Check ("local Remaining of Plain_Work.Pause_Between has a value",
                      Text'Length > 0 and then Text (Text'First) /= '<', Text);
            end;
         end if;
      end;

      declare
         Gs : Ref_List;
         Totals : Value_Ref;
         Have : Boolean := False;
         Node : Value_Node;
         Bytes : Byte_Array (1 .. Memory_Bytes);
         Got : Memory_Length;
      begin
         Globals (S, "Plain_Work", Gs, R);
         for X of Gs.Items (1 .. Gs.Count) loop
            if Image (X.Name) = "Plain_Work.Totals" then
               Totals := X;
               Have := True;
            end if;
         end loop;
         Check ("global Plain_Work.Totals", Have);
         if Have then
            Value (S, Totals, Node, R);
            Say ("Plain_Work.Totals = " & Image (Node.Display));
            Check ("Totals read live through a view", R = Ok and then Node.Kind = Record_Value,
                   Image (Node.Display));
            Memory (S, Node.Address, Memory_Bytes, Bytes, Got, R);
            Check ("memory at Totals reads through the kernel", R = Ok and then Got = Memory_Bytes,
                   Got'Image);
            declare
               Before : constant String := Rounds_Display (Totals);
            begin
               CuBit.Waits.Sleep_For (Run_Between_Ms);
               --  A new epoch: the views are mapped again, so the bytes are now's.
               Threads (S, Ts, R);
               declare
                  Fs : Frame_List;
                  Ignore_Frame : Natural;
                  Ignore_Thread : Thread_Id;
               begin
                  Find_Frame ("Plain_Work.Pause_Between", Ignore_Thread, Fs, Ignore_Frame);
               end;
               Check ("Totals.Rounds moved while it ran (" & Before & " then " & Rounds_Display (Totals) & ")",
                      Before'Length > 0 and then Rounds_Display (Totals) /= Before);
            end;
         end if;
      end;
      Pause (S, Deadline_After (Mint_Ms), R);
      Check ("pause through the kernel is not built yet (refused, the target runs on)", R = Refused,
             Outcome'Image (R));
      Detach (S);
   end if;

   --  A system service, through the session's debug authority.
   declare
      Clock_Session : HD.Session;
      Minted_As : LH.Outcome;
      Fs : Frame_List;
   begin
      Session_Debug (Server_Of (Startup.Clock), Clock_Session, Minted_As);
      Check ("the session's debug authority gets Debug_Observe on clock.svc", Minted_As = LH.Done,
             LH.Outcome'Image (Minted_As));
      if Minted_As = LH.Done then
         Attach ((Target => CuBit.Handles.No_Endpoint, Session => Clock_Session,
                  Filesystem => Startup.Filesystem), Observe, Deadline_After (Attach_Ms), S, R);
         Check ("attach to clock.svc through the kernel", R = Ok, Outcome'Image (R));
         if R = Ok then
            Threads (S, Ts, R);
            Check ("clock.svc's threads", R = Ok and then Ts.Count >= 1, Ts.Count'Image);
            for T of Ts.Items (1 .. Ts.Count) loop
               Say ("clock thread" & T.Id'Image & " waits " & DT.Wait_Code'Image (T.Wait));
            end loop;
            if Ts.Count >= 1 then
               Frames (S, Ts.Items (1).Id, Fs, R);
               for I in 0 .. Fs.Count - 1 loop
                  Say ("  #" & I'Image & " " & Image (Fs.Items (I).Function_Name) & " at "
                       & Image (Fs.Items (I).File) & ":" & Fs.Items (I).Line'Image);
               end loop;
               Check ("clock.svc's frames, unwound with its debug information",
                      R = Ok and then Fs.Count >= 2
                      and then (for some I in 0 .. Fs.Count - 1 => Fs.Items (I).Has_Debug_Info),
                      Outcome'Image (R) & Fs.Count'Image);
            end if;
            Detach (S);
         end if;
      end if;
   end;

   declare
      Ended : CuBit.Launching.Launch_Result;
   begin
      CuBit.Launching.End_Child (Plain, Ended);
   end;
   Say (if Failures = 0 then "ALL PASS" else "FAIL");
end Main;
