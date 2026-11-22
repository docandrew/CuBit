with Ada.Text_IO; with Interfaces; with System;
with CuBit.Request_Channels; with Lexicon_Channel; with IPCtest_Channel;
procedure Tests is
   use Interfaces;
   use type System.Address;
   package R is new CuBit.Request_Channels
     (Unsigned_64, Unsigned_64, 0, 0, Client_Extra_Pages => 16, Server_Extra_Pages => 16);
   use type R.Payload_View, R.Token;
   type Bytes is array (Natural range <>) of Unsigned_8 with Component_Size => 8;
   Guard : constant := 4_096;
   Client_Memory : aliased Bytes (0 .. (R.Client_Pages + 2) * R.Page_Bytes - 1) := [others => 0]
     with Alignment => 4_096;
   Server_Memory : aliased Bytes (0 .. (R.Server_Pages + 2) * R.Page_Bytes - 1) := [others => 0]
     with Alignment => 4_096;
   C : R.Client; S : R.Server;
   LC : Lexicon_Channel.RC.Client; LS : Lexicon_Channel.RC.Server;
   IC : IPCtest_Channel.RC.Client; ISrv : IPCtest_Channel.RC.Server;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   Submitted, Rung, Got : Boolean;
   Request : R.Queues.Submission; Answer : R.Queues.Completion;
begin
   Check (R.Write_Payload (C) = R.Empty_Payload); Check (R.Read_Payload (S) = R.Empty_Payload);
   Check (R.Client_Pages = 18 and R.Server_Pages = 18);
   Check (R.Client_Payload_At = 8192 and R.Server_Payload_At = 8192);
   R.Attach_Client (C, Client_Memory (Guard)'Address, Server_Memory (Guard)'Address);
   R.Attach_Server (S, Server_Memory (Guard)'Address, Client_Memory (Guard)'Address);
   Check (R.Write_Payload (C).Base = Client_Memory (Guard + R.Client_Payload_At)'Address);
   Check (R.Read_Payload (S).Base = R.Write_Payload (C).Base);
   Check (R.Read_Payload (C).Base = R.Write_Payload (S).Base);
   Check (R.Write_Payload (C).Writable and R.Write_Payload (S).Writable);
   Check (not R.Read_Payload (C).Writable and not R.Read_Payload (S).Writable);
   Check (R.Write_Payload (C).Bytes = 65_536 and R.Write_Payload (S).Bytes = 65_536);
   declare
      Write_Data : Bytes (0 .. 65_535) with Import, Address => R.Write_Payload (C).Base;
      Read_Data : Bytes (0 .. 65_535) with Import, Address => R.Read_Payload (S).Base;
      Output : Bytes (0 .. 65_535) with Import, Address => R.Write_Payload (S).Base;
      Returned : Bytes (0 .. 65_535) with Import, Address => R.Read_Payload (C).Base;
   begin
      for I in Write_Data'Range loop Write_Data (I) := Unsigned_8 (I mod 251); end loop;
      for Turn in 1 .. 128 loop
         R.Submit (C, R.Token (Turn), Unsigned_64 (Turn), Submitted, Rung);
         Check (Submitted); Check (not R.Can_Submit (C));
         R.Take (S, Request, Got, Rung); Check (Got); Check (Request.Tag = R.Token (Turn));
         Check (Request.Item = Unsigned_64 (Turn));
         for I in Read_Data'Range loop
            Check (Read_Data (I) = Unsigned_8 (I mod 251));
            Output (I) := Read_Data (I) xor 16#A5#;
         end loop;
         R.Complete (S, Request.Tag, Unsigned_64 (Turn) * 3, Rung);
         R.Reap (C, Answer, Got); Check (Got); Check (Answer.Tag = R.Token (Turn));
         Check (Answer.Answer = Unsigned_64 (Turn) * 3);
         Check (Returned (0) = 16#A5# and Returned (65_535) = (Unsigned_8 (65_535 mod 251) xor 16#A5#));
      end loop;
   end;
   for I in 0 .. Guard - 1 loop
      Check (Client_Memory (I) = 0 and Server_Memory (I) = 0);
      Check (Client_Memory (Client_Memory'Last - I) = 0 and Server_Memory (Server_Memory'Last - I) = 0);
   end loop;
   R.Detach_Client (C); R.Detach_Server (S);
   Check (R.Write_Payload (C) = R.Empty_Payload and R.Read_Payload (C) = R.Empty_Payload);
   Check (R.Write_Payload (S) = R.Empty_Payload and R.Read_Payload (S) = R.Empty_Payload);
   Lexicon_Channel.RC.Attach_Client (LC, Client_Memory (Guard)'Address, Server_Memory (Guard)'Address);
   Lexicon_Channel.RC.Attach_Server (LS, Server_Memory (Guard)'Address, Client_Memory (Guard)'Address);
   Check (Lexicon_Channel.RC.Write_Payload (LC).Base = System.Null_Address);
   Check (Lexicon_Channel.RC.Read_Payload (LS).Bytes = 0);
   IPCtest_Channel.RC.Attach_Client (IC, Client_Memory (Guard)'Address, Server_Memory (Guard)'Address);
   IPCtest_Channel.RC.Attach_Server (ISrv, Server_Memory (Guard)'Address, Client_Memory (Guard)'Address);
   Check (IPCtest_Channel.RC.Write_Payload (IC).Base = System.Null_Address);
   Check (IPCtest_Channel.RC.Read_Payload (ISrv).Bytes = 0);
   Ada.Text_IO.Put_Line ("Channel payload checks" & Checks'Image);
end Tests;
