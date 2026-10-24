------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The net-link headless test's guest (tests/net-link, docs/net-link.md).
--  While the host fixture pulls the virtual cable for two seconds:
--  - a TCP stream from the fixture (a repeating byte pattern) must survive
--    and keep transferring afterwards, every byte in order;
--  - a connected UDP channel that sends while the link is down must end
--    Unreachable (no usable route);
--  - once the link is back, a new UDP channel reaches the fixture's echo.
--  Prints one PASS line per check and TEST: PASS net-link when all pass.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Net_Channels;
with Startup;

procedure Main is
   package Channels renames CuBit.Net_Channels;
   package Layout renames Channels.Layout;

   Page_Bytes : constant := 4_096;
   Ring_Size  : constant := 16_384;
   Buffers    : constant := 2;
   Stream_Buffer : constant := 0;
   Echo_Buffer   : constant := 1;
   Stream_Bit    : constant := 0;
   Echo_Bit      : constant := 1;

   --  The fixture's stream: byte K of it is K mod Pattern_Period.
   Pattern_Period : constant := 251;
   --  Bytes before the fixture is told the stream is flowing, and bytes
   --  the stream must still carry once the link is back.
   Warm_Up_Bytes   : constant := 65_536;
   After_Up_Bytes  : constant := 262_144;
   Poll_MS         : constant := 100;
   Test_MS         : constant := 90_000;
   Echo_Ping       : constant String := "PING";
   Echo_Pong       : constant String := "PONG";

   Stream_Target : constant String := "@net:tcp:10.0.2.2:18460";
   Echo_Target   : constant String := "@net:udp:10.0.2.2:18461";

   Arena  : Channels.Arena;
   Stream, Echo : Channels.Stream;
   Token  : Unsigned_64 := 16#4E4C_0000_0000_0000#;
   Passed : Boolean := True;

   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1;
      return Token;
   end Next_Token;

   procedure Say (Text : String) is
   begin
      debugPrint ("net-link-check: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
      end if;
      Say (Name & (if Condition then " PASS" else " FAIL"));
   end Check;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   --  Open S on Target through Slot; True when open.
   function Open (S : in out Channels.Stream; Slot : CuBit.Handles.Endpoint;
                  Target : String) return Boolean
   is
      T : constant Unsigned_64 := Next_Token;
      Submitted, OK : Boolean;
      Reply : Message;
   begin
      Channels.Reset (S);
      Channels.Submit_Open (S, Slot, Target, T, Submitted);
      if not Submitted then
         return False;
      end if;
      Channels.Wait_For (T, Reply, OK);
      return OK and then Channels.Opened (S, Reply);
   end Open;

   --  The stream so far: bytes received, all in pattern.
   Received   : Unsigned_64 := 0;
   In_Order   : Boolean := True;

   --  Read what the stream has (waiting up to Poll_MS for some).
   procedure Pump_Stream is
      Ready : Boolean;
      Got   : Natural;
      Chunk : array (1 .. 4_096) of Unsigned_8;
   begin
      Channels.Await (Stream, Startup.Stream, Layout.Want_Readable,
                      Now + Poll_MS, Next_Token, Ready);
      loop
         Channels.Read (Stream, Chunk'Address, Chunk'Length, Got);
         exit when Got = 0;
         for I in 1 .. Got loop
            if Chunk (I) /= Unsigned_8 ((Received + Unsigned_64 (I - 1)) mod Pattern_Period) then
               In_Order := False;
            end if;
         end loop;
         Received := Received + Unsigned_64 (Got);
      end loop;
   end Pump_Stream;

   type Phase is (Before_Down, Down_Seen, Back_Up, Done);
   Current  : Phase := Before_Down;
   Up_Mark  : Unsigned_64 := 0;
   Started  : constant Unsigned_64 := Now;
   Last_Ping : Unsigned_64 := 0;
   Echo_Open : Boolean := False;
begin
   declare
      Raw : constant Unsigned_64 :=
        syscall (SYSCALL_SBRK, Unsigned_64 (Buffers * Channels.Buffer_Bytes (Ring_Size, Ring_Size)) + Page_Bytes);
      Aligned : constant Unsigned_64 := (Raw + Page_Bytes - 1) / Page_Bytes * Page_Bytes;
      OK : Boolean;
   begin
      if Raw = Unsigned_64'Last then
         Say ("no memory for the arena FAIL");
         return;
      end if;
      Channels.Create_Arena (Arena, Startup.Stream, To_Address (Integer_Address (Aligned)),
                             Ring_Size, Ring_Size, Buffers, OK);
      Check (OK, "arena lent");
      if not OK then
         return;
      end if;
   end;
   Channels.Prepare (Stream, Arena, Stream_Buffer, Stream_Bit, Startup.Stream);
   Channels.Prepare (Echo, Arena, Echo_Buffer, Echo_Bit, Startup.Echo);
   Check (Open (Stream, Startup.Stream, Stream_Target), "stream opens");
   Echo_Open := Open (Echo, Startup.Echo, Echo_Target);
   Check (Echo_Open, "echo channel opens");

   while Received < Warm_Up_Bytes and then Now - Started < Test_MS loop
      Pump_Stream;
   end loop;
   Say ("transferring");

   --  The fixture pulls the cable now. Ping the echo port every Poll_MS:
   --  while the link is down, a send has no usable route and the channel
   --  ends Unreachable; after that, open a new channel until one is
   --  answered.
   while Current /= Done and then Now - Started < Test_MS loop
      Pump_Stream;
      if Now - Last_Ping >= Poll_MS then
         Last_Ping := Now;
         if not Echo_Open then
            Echo_Open := Open (Echo, Startup.Echo, Echo_Target);
         end if;
         if Echo_Open then
            declare
               Sent, Truncated, Found : Boolean;
               Got : Natural;
               Reply : String (1 .. 8);
            begin
               loop
                  Channels.Receive_Datagram (Echo, Reply'Address, Reply'Length, Got, Truncated, Found);
                  exit when not Found;
                  if Current = Down_Seen and then Got = Echo_Pong'Length and then
                    Reply (1 .. Got) = Echo_Pong
                  then
                     Check (True, "udp works after the link is back");
                     Current := Back_Up;
                     Up_Mark := Received;
                  end if;
               end loop;
               if Channels.Status (Echo) = Layout.Status_Unreachable then
                  if Current = Before_Down then
                     Check (True, "udp send while down is unreachable");
                     Current := Down_Seen;
                  end if;
                  Channels.Close (Echo, Startup.Echo);
                  Echo_Open := False;
               else
                  Channels.Send_Datagram (Echo, Echo_Ping'Address, Echo_Ping'Length, Sent);
               end if;
            end;
         end if;
      end if;
      if Current = Back_Up and then Received - Up_Mark >= After_Up_Bytes then
         Current := Done;
      end if;
   end loop;

   Check (Current = Done and then In_Order and then not Channels.Final (Stream),
          "tcp stream survived the flap and kept transferring in order");
   Channels.Close (Stream, Startup.Stream);
   if Echo_Open then
      Channels.Close (Echo, Startup.Echo);
   end if;
   if Passed then
      debugPrint ("TEST: PASS net-link" & ASCII.LF);
   else
      debugPrint ("TEST: FAIL net-link" & ASCII.LF);
   end if;
end Main;
