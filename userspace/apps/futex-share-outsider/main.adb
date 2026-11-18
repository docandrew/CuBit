------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared futex checks (IPC-010, headless --test futex-share): a process
--  that is neither party of the grant learns a shared word (from the
--  server) and may neither wake nor wait on it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Waits;
with Wait_ABI;
with Futex_ABI;
with Startup;

procedure Main is
   use ASCII;
   package W renames CuBit.Waits;

   OP_WORD  : constant Unsigned_32 := 16#0A14#;
   REPLY_OK : constant Unsigned_32 := 16#F000#;
   Attempts : constant := 500;
   Retry_Ms : constant := 10;
   Answer_Ms : constant := 2_000;

   Server : constant CuBit.Handles.Endpoint := Startup.Ipc_Test;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("futex-share-outsider: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("futex-share-outsider: " & Name & " FAIL" & LF);
      end if;
   end Check;

   --  A shared futex name with any word, well formed or not.
   function Raw (Word : Unsigned_64) return W.Futex_Name is
      Name : W.Futex_Name;
   begin
      Name.Shared := True;
      Name.Word := Word;
      return Name;
   end Raw;

   M : Message;
   Tag : MessageTag;
   Shared : W.Futex_Name := Raw (0);
   Learned : Boolean := False;
begin
   for Attempt in 1 .. Attempts loop
      M := NULL_MESSAGE;
      M.tag := (label => OP_WORD, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Tag := capCall (Server, M, Deadline_After (Answer_Ms));
      if Tag.label = REPLY_OK then
         Shared.Word := M.words (0);
         Learned := True;
         exit;
      end if;
      W.Sleep_For (Retry_Ms);
   end loop;
   Check (Learned and then Futex_ABI.Well_Formed (Shared.Word), "learned a live shared word");
   Check (W.Futex_Wake (Shared, 1) = Futex_ABI.Wake_Refused, "wake without the grant refused");
   Check (W.Futex_Wait (Shared, 0, W.Now) = Wait_ABI.Futex_Fault, "wait without the grant refused");
   Check (W.Hand_Off (Shared, W.Private_Word (Shared'Address), 0, W.Now) = Wait_ABI.Futex_Fault,
          "hand-off waking a word without the grant refused");
   if Failures = 0 then
      debugPrint ("futex-share-outsider: ALL PASS" & LF);
   else
      debugPrint ("futex-share-outsider: SOME FAIL" & LF);
   end if;
   loop
      W.Sleep_For (1_000);
   end loop;
end Main;
