with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Startup_State;
with Startup;

procedure Main is
   use ASCII;

   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR  : constant Unsigned_32 := 16#F001#;

   From   : Process_ID;
   Request : Message;
   Response : Message;
   Ignore : Unsigned_64;
   -- Native Rust ABI fixtures, isolated from normal CCL operations. Two
   -- separately launched probes must complete their own full-width echo
   -- before reporting opposite Clock-authority outcomes.
   RUST_ECHO : constant Unsigned_32 := 16#0C10#;
   RUST_REPORT : constant Unsigned_32 := 16#0C11#;
   RUST_ALLOCATION_REPORT : constant Unsigned_32 := 16#0C12#;
   RUST_HEAP_REPORT : constant Unsigned_32 := 16#0C13#;
   Rust_Peers : array (1 .. 2) of Process_ID := [others => No_Process];
   Rust_Allocation_Passed : array (1 .. 2) of Boolean := [others => False];
   Rust_Allowed, Rust_Denied : Process_ID := No_Process;
   --  The first probe's request, answered when its peer finishes.
   Rust_Reply : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   Defer_Reply : Boolean := False;
   Hello : constant MessageWords :=
     [16#7266_206F_6C6C_6548#, 16#2174_7375_5220_6D6F#,
      16#8000_0000_FFFF_FFFF#, 16#1234_5678_9ABC_DEF0#];
begin
   debugPrint ("ccl-test-host: starting" & LF);
   if not CuBit.Startup_State.Ready (Startup.Verify) then
      debugPrint ("ccl-test-host: startup capabilities missing" & LF);
      return;
   end if;
   Ignore := registerDriver (DRIVER_CCL_TEST, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("ccl-test-host: registration failed" & LF);
      return;
   end if;
   debugPrint ("ccl-test-host: registered" & LF);

   loop
      receive (From, Request);
      Response := NULL_MESSAGE;
      Defer_Reply := False;
      if Request.tag.label = CCL_TEST_OP_INCREMENT and then
        Request.tag.length >= 1
      then
         debugPrint ("ccl-test-host: import invoked" & LF);
         Response.tag :=
           (label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0);
         Response.words (0) := Request.words (0) + 1;
      elsif Request.tag = (label => RUST_ECHO, length => 4, protocol => 0, ipc => <>, reserved => 0) and then
        Request.words = Hello and then From /= No_Process
      then
         for Peer of Rust_Peers loop
            if Peer = No_Process or else Peer = From then
               Peer := From;
               Response := Request;
               Response.tag.label := REPLY_OK;
               exit;
            end if;
         end loop;
         debugPrint ("rust-probe: Hello from Rust! (IPC)" & LF);
      elsif Request.tag = (label => RUST_ALLOCATION_REPORT, length => 1, protocol => 0, ipc => <>, reserved => 0) and then
        Request.words = [1, 0, 0, 0] and then From /= No_Process
      then
         Response.tag := (label => REPLY_ERROR, length => 0,
                          protocol => 0, ipc => <>, reserved => 0);
         for P in Rust_Peers'Range loop
            if Rust_Peers (P) = From and then not Rust_Allocation_Passed (P) then
               Rust_Allocation_Passed (P) := True;
               Response.tag := (label => REPLY_OK, length => 0,
                                protocol => 0, ipc => <>, reserved => 0);
               debugPrint ("TEST: PASS rust-allocator peer" & Integer'Image (P) & LF);
               exit;
            end if;
         end loop;
      elsif Request.tag = (label => RUST_HEAP_REPORT, length => 1, protocol => 0, ipc => <>, reserved => 0) and then
        Request.words = [1, 0, 0, 0] and then From /= No_Process and then
        From = Rust_Allowed and then Rust_Denied /= No_Process
      then
         Response.tag := (label => REPLY_OK, length => 0, protocol => 0, ipc => <>, reserved => 0);
         debugPrint ("TEST: PASS rust-heap-rollback" & LF);
      elsif Request.tag = (label => RUST_REPORT, length => 2, protocol => 0, ipc => <>, reserved => 0) and then
        Request.words (2) = 0 and then Request.words (3) = 0 and then
        From /= No_Process and then
        ((From = Rust_Peers (1) and then Rust_Allocation_Passed (1)) or else
         (From = Rust_Peers (2) and then Rust_Allocation_Passed (2)) or else
         (Request.words (0) = 0 and then
          (From = Rust_Peers (1) or else From = Rust_Peers (2))))
      then
         if Request.words (0) = 1 and then Rust_Allowed = No_Process and then
           From /= Rust_Denied
         then
            Rust_Allowed := From;
            debugPrint ("TEST: PASS rust-clock-authorized" & LF);
         elsif Request.words (0) = 2 and then Request.words (1) = 0 and then
           Rust_Denied = No_Process and then From /= Rust_Allowed
         then
            Rust_Denied := From;
            debugPrint ("TEST: PASS rust-clock-denied" & LF);
         else
            debugPrint ("TEST: FAIL rust-probe outcome" &
                        Unsigned_64'Image (Request.words (0)) & " detail" &
                        Unsigned_64'Image (Request.words (1)) & LF);
         end if;
         Response.tag := (label => REPLY_OK, length => 0, protocol => 0, ipc => <>, reserved => 0);
         if Rust_Allowed /= No_Process and then Rust_Denied /= No_Process then
            Ignore := replyCap (Rust_Reply, Response);
            debugPrint ("TEST: PASS rust-native" & LF);
         elsif Request.words (0) in 1 | 2 then
            -- Keep the first process alive until its peer finishes. A PID is
            -- reusable after exit; it is not a permanent process identity.
            declare
               Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
            begin
               if Saved.Saved then
                  Rust_Reply := Saved.Handle;
                  Defer_Reply := True;
               end if;
            end;
            if not Defer_Reply then
               debugPrint ("TEST: FAIL rust-probe deferred reply" & LF);
            end if;
         end if;
      else
         Response.tag :=
           (label => REPLY_ERROR, length => 0, protocol => 0, ipc => <>, reserved => 0);
      end if;
      if not Defer_Reply then Ignore := reply (From, Response); end if;
   end loop;
end Main;
