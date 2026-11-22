------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared futex checks (IPC-010, headless --test futex-share): the grantee.
--
--  @description
--  The client grants this process two pages it owns and asks it, call by
--  call, to: answer a ping-pong on two words of the first page (with
--  wake-plus-wait or with a wake and a wait); wake waiters of a third word
--  one at a time (their order is the client's check); and wait on a word
--  of the second page until the client revokes that grant (the answer,
--  Futex_Revoked, goes back in the first page). The outsider asks it for
--  a shared word it holds no grant for.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Monotonic;
with CuBit.Waits;
with Wait_ABI;
with Futex_ABI;
with Startup;

procedure Main is
   use ASCII;
   package W renames CuBit.Waits;
   package GR renames CuBit.Grant_References;

   --  The protocol (the client's main.adb has the same numbers).
   OP_SHARE     : constant Unsigned_32 := 16#0A10#;
   OP_PINGPONG  : constant Unsigned_32 := 16#0A11#;
   OP_FIFO_WAKE : constant Unsigned_32 := 16#0A12#;
   OP_WAIT_END  : constant Unsigned_32 := 16#0A13#;
   OP_WORD      : constant Unsigned_32 := 16#0A14#;
   OP_PEER_WAKE : constant Unsigned_32 := 16#0A15#;
   OP_CHURN     : constant Unsigned_32 := 16#0A16#;
   Peer_Wake_Delay_Ms : constant := 20;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_NOT_YET : constant Unsigned_32 := 16#F001#;
   REPLY_FAILED : constant Unsigned_32 := 16#F002#;
   Mode_Hand_Off : constant Unsigned_64 := 0;

   --  The first page's words (byte offsets).
   Ping_At     : constant := 0;    --  the client writes, this side waits
   Pong_At     : constant := 64;   --  this side writes, the client waits
   Fifo_At     : constant := 128;  --  the FIFO waiters' word
   Taken_At    : constant := 192;  --  how many FIFO waiters have run
   Revoked_At  : constant := 256;  --  this side's revoke result, + 1
   --  The second page's word.
   Doomed_At   : constant := 0;

   Answer_Ms : constant := 5_000;
   Run_Ms    : constant := 30_000;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   package Words is new System.Address_To_Access_Conversions (Word);
   subtype Word_Access is Words.Object_Pointer;

   --  An aligned word of a page shared with one peer: every read is one
   --  atomic load, and the peer may change it at any time.
   function At_Offset (Base : System.Address; Offset : Natural) return Word_Access is
     (Words.To_Pointer (Base + Storage_Offset (Offset)));

   First, Second : GR.Reference;
   --  A grant made with the notify flag (a channel's): either party may
   --  wake the other's wake word through it (IPC-010 option B).
   Notified : GR.Reference;
   Has_Notified : Boolean := False;
   First_Base : System.Address := System.Null_Address;
   Shared : Boolean := False;
   from : Process_ID;
   msg  : Message;
   Ignore : Unsigned_64;
   Ok : Boolean;

   function Deadline return Unsigned_64 is
     (CuBit.Messages.Deadline_After (Answer_Ms));

   function Reply (Label : Unsigned_32; Value : Unsigned_64 := 0) return Message is
      R : Message := NULL_MESSAGE;
   begin
      R.tag := (label => Label, length => 1, protocol => 0, ipc => <>, reserved => 0);
      R.words (0) := Value;
      return R;
   end Reply;

   --  The pong side: answer Rounds pings; each round waits for Ping to
   --  move past the last value, then writes it to Pong and wakes the
   --  client, with one hand-off or with a wake and a wait.
   procedure Pong (Rounds, Mode, Start : Unsigned_64; Failed : out Boolean) is
      Ping : constant Word_Access := At_Offset (First_Base, Ping_At);
      Pong_W : constant Word_Access := At_Offset (First_Base, Pong_At);
      Ping_Name : constant W.Futex_Name := W.Shared_Word (First, Ping_At);
      Pong_Name : constant W.Futex_Name := W.Shared_Word (First, Pong_At);
      Last : Unsigned_32 := Unsigned_32 (Start mod 2 ** 32);
      --  One deadline for the whole run (see the client).
      Run_Deadline : constant Unsigned_64 := Deadline_After (Run_Ms);
      Result : Unsigned_64;
      Handed : Boolean := False;
   begin
      Failed := False;
      for Round in 1 .. Rounds loop
         loop
            exit when Unsigned_32 (Ping.all) /= Last;
            if Handed and then Mode = Mode_Hand_Off then
               Result := W.Hand_Off (Pong_Name, Ping_Name, Unsigned_64 (Last), Run_Deadline);
               Handed := False;
            else
               Result := W.Futex_Wait (Ping_Name, Unsigned_64 (Last), Run_Deadline);
            end if;
            if Result = Wait_ABI.Futex_Timed_Out or else Result = Wait_ABI.Futex_Fault
              or else Result = Wait_ABI.Futex_Revoked
            then
               Failed := True;
               return;
            end if;
         end loop;
         Last := Unsigned_32 (Ping.all);
         Pong_W.all := Word (Last);
         if Mode = Mode_Hand_Off and then Round < Rounds then
            --  The wake rides on the next wait (Hand_Off above).
            Handed := True;
         else
            Ignore := W.Futex_Wake (Pong_Name, 1);
         end if;
      end loop;
      if Handed then
         Ignore := W.Futex_Wake (Pong_Name, 1);
      end if;
   end Pong;

begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Ignore := registerDriver (DRIVER_IPCTEST, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("futex-share-server: register FAIL" & LF);
      loop
         W.Sleep_For (1_000);
      end loop;
   end if;
   debugPrint ("futex-share-server: registered" & LF);

   receive (from, msg);
   loop
      if msg.tag.label = OP_SHARE then
         --  words: the two grants' references.
         First := GR.Decode (msg.words (0));
         Second := GR.Decode (msg.words (1));
         declare
            Mapped : System.Address;
            Ok2 : Boolean;
            Ignore_Mapped : System.Address;
         begin
            CuBit.Memory_Grants.Acquire
              (First, from, 0, CuBit.Memory_Grants.PAGE_SIZE,
               CuBit.Memory_Grants.Write_Access, Mapped, Ok);
            CuBit.Memory_Grants.Acquire
              (Second, from, 0, CuBit.Memory_Grants.PAGE_SIZE,
               CuBit.Memory_Grants.Write_Access, Ignore_Mapped, Ok2);
            Shared := Ok and then Ok2;
            if Shared then
               First_Base := Mapped;
            end if;
         end;
         replyWait (from, Reply (if Shared then REPLY_OK else REPLY_FAILED), from, msg);

      elsif msg.tag.label = OP_PINGPONG and then Shared then
         --  words: rounds, mode, the ping value before the first round.
         --  Answer at once, then pong.
         declare
            Rounds : constant Unsigned_64 := msg.words (0);
            Mode : constant Unsigned_64 := msg.words (1);
            Start : constant Unsigned_64 := msg.words (2);
            Failed : Boolean;
         begin
            Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
            Pong (Rounds, Mode, Start, Failed);
            if Failed then
               debugPrint ("futex-share-server: pong FAIL" & LF);
            end if;
         end;
         receive (from, msg);

      elsif msg.tag.label = OP_FIFO_WAKE and then Shared then
         --  words: how many waiters. Wake one, wait until it has run, then
         --  the next: the client checks they ran in the order they waited.
         declare
            Count : constant Unsigned_64 := msg.words (0);
            Taken : constant Word_Access := At_Offset (First_Base, Taken_At);
            Fifo_Name : constant W.Futex_Name := W.Shared_Word (First, Fifo_At);
            Started : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
            Good : Boolean := True;
         begin
            for I in 1 .. Count loop
               if W.Futex_Wake (Fifo_Name, 1) /= 1 then
                  Good := False;
                  exit;
               end if;
               while Unsigned_64 (Taken.all) < I loop
                  if CuBit.Monotonic.Microseconds - Started > Answer_Ms * 1_000 then
                     Good := False;
                     exit;
                  end if;
                  W.Sleep_For (1);
               end loop;
               exit when not Good;
            end loop;
            replyWait (from, Reply (if Good then REPLY_OK else REPLY_FAILED), from, msg);
         end;

      elsif msg.tag.label = OP_WAIT_END and then Shared then
         --  Answer at once, then wait on the second page's word until its
         --  grant ends; the result (+ 1) goes to the first page.
         declare
            Result : Unsigned_64;
            Out_Word : constant Word_Access := At_Offset (First_Base, Revoked_At);
         begin
            Ignore := CuBit.Messages.reply (from, Reply (REPLY_OK));
            Result := W.Futex_Wait (W.Shared_Word (Second, Doomed_At), 0, Deadline);
            Out_Word.all := Word (Result + 1);
            Ignore := W.Futex_Wake (W.Shared_Word (First, Revoked_At), 1);
         end;
         receive (from, msg);

      elsif msg.tag.label = OP_WORD then
         --  The outsider: the first page's Ping word, as a shared word, and
         --  the notified grant's reference.
         if Shared and then Has_Notified then
            declare
               R : Message := Reply (REPLY_OK, Futex_ABI.Shared_Word (First.slot, Ping_At));
            begin
               R.tag.length := 2;
               R.words (1) := GR.Encode (Notified);
               replyWait (from, R, from, msg);
            end;
         else
            replyWait (from, Reply (REPLY_NOT_YET), from, msg);
         end if;

      elsif msg.tag.label = OP_PEER_WAKE then
         --  words: the client's notified grant. Answer whether a peer wake
         --  through a grant made without the notify flag is refused (1),
         --  then wake the client's wake word through the notified one,
         --  after it has gone to sleep.
         Notified := GR.Decode (msg.words (0));
         Has_Notified := True;
         Ignore := CuBit.Messages.reply
           (from, Reply (REPLY_OK, (if W.Wake_Peer (First) then 0 else 1)));
         W.Sleep_For (Peer_Wake_Delay_Ms);
         if not W.Wake_Peer (Notified) then
            debugPrint ("futex-share-server: peer wake FAIL" & LF);
         end if;
         receive (from, msg);

      elsif msg.tag.label = OP_CHURN then
         --  words: a notified grant. Take it and give it back: the client
         --  hears of it (a grant-return notice).
         declare
            Churned : constant GR.Reference := GR.Decode (msg.words (0));
            Mapped : System.Address;
            Taken, Given : Boolean := False;
         begin
            CuBit.Memory_Grants.Acquire
              (Churned, from, 0, CuBit.Memory_Grants.PAGE_SIZE,
               CuBit.Memory_Grants.Read_Access, Mapped, Taken);
            if Taken then
               CuBit.Memory_Grants.Return_Acquisition (Churned, Given);
            end if;
            replyWait (from, Reply (if Taken and then Given then REPLY_OK else REPLY_FAILED),
                       from, msg);
         end;

      else
         replyWait (from, Reply (REPLY_FAILED), from, msg);
      end if;
   end loop;
end Main;
