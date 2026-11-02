------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  startup-wait-provider (tests/startup-wait, headless test startup-wait):
--  provides the test-only "startup-wait-test" service late, or ends without
--  ever registering it, so procmgr's startup waits (CAP-003) meet a
--  provider that is starting, one that ends first, and one that misses its
--  ready deadline. It answers the client's probe once registered.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_Wait_Test;
with Parameters;
with Startup;

procedure Main is
   use ASCII;
   use Parameters;
   package SW renames CuBit.Startup_Wait_Test;

   REPLY_UNKNOWN : constant Unsigned_32 := 16#F001#;

   P : constant Provider_Parameters_Value := Value;

   function Image (N : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   From : Process_ID;
   Request, Answer : Message;
   Ignore : Unsigned_64;
begin
   case P.behaviour is
      when Never_Register =>
         debugPrint ("startup-wait-provider: ends in " & Image (Unsigned_64 (P.after)) &
                     " ms without registering" & LF);
         Ignore := syscall (SYSCALL_SLEEP, Unsigned_64 (P.after));
         debugPrint ("startup-wait-provider: ending unregistered" & LF);
         return;
      when Register_Late =>
         debugPrint ("startup-wait-provider: registers in " & Image (Unsigned_64 (P.after)) &
                     " ms" & LF);
         Ignore := syscall (SYSCALL_SLEEP, Unsigned_64 (P.after));
   end case;
   if registerDriver (SW.Service_Role, Startup.Registration) = Unsigned_64'Last then
      debugPrint ("startup-wait-provider: registration failed" & LF);
      return;
   end if;
   debugPrint ("startup-wait-provider: registered" & LF);
   loop
      receive (From, Request);
      Answer := NULL_MESSAGE;
      if Request.tag.label = SW.Probe_Label then
         Answer.tag := (label => SW.Probe_Answer, length => 0, protocol => 0, ipc => <>,
                        reserved => 0);
      else
         Answer.tag := (label => REPLY_UNKNOWN, length => 0, protocol => 0, ipc => <>,
                        reserved => 0);
      end if;
      Ignore := reply (From, Answer);
   end loop;
end Main;
