------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The net-multi headless test's guest (tests/net-link, docs/net-link.md),
--  on a machine with two network interfaces: a static one (10.0.2.15) and
--  one netmgr configures by DHCP (10.0.3.x). It keeps a TCP stream from the
--  host fixture over the second interface, and sends a datagram every
--  100 ms to an address on neither network, which only a default route
--  reaches: while the fixture pulls one cable, the stream must keep flowing
--  and the datagrams must keep going out (by the other interface's default
--  route); the fixture checks which interface each one left by. The
--  datagram channel must never see Unreachable (some default route is always
--  usable). Ends when the fixture closes the stream.
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
   Pattern_Period : constant := 251;
   Warm_Up_Bytes  : constant := 65_536;
   Poll_MS        : constant := 100;
   Test_MS        : constant := 150_000;
   Probe          : constant String := "PROBE";
   Stream_Target  : constant String := "@net:tcp:10.0.3.2:18471";
   Probe_Target   : constant String := "@net:udp:192.0.2.1:9";

   Arena  : Channels.Arena;
   Stream, Probes : Channels.Stream;
   Token  : Unsigned_64 := 16#4E4D_0000_0000_0000#;
   Passed : Boolean := True;

   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1;
      return Token;
   end Next_Token;

   procedure Say (Text : String) is
   begin
      debugPrint ("net-multi-check: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
      end if;
      Say (Name & (if Condition then " PASS" else " FAIL"));
   end Check;

   function Now return Unsigned_64 is (syscall (SYSCALL_GETTIME));

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

   Received : Unsigned_64 := 0;
   In_Order : Boolean := True;

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

   Started    : constant Unsigned_64 := Now;
   Last_Probe : Unsigned_64 := 0;
   Running    : Boolean := False;
   Unreachable : Boolean := False;
   Opened      : Boolean := False;
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
   Channels.Prepare (Stream, Arena, 0, 0, Startup.Stream);
   Channels.Prepare (Probes, Arena, 1, 1, Startup.Probe);
   --  The second interface comes up by DHCP: retry until its route exists.
   loop
      Opened := Open (Stream, Startup.Stream, Stream_Target);
      exit when Opened or else Now - Started >= Test_MS;
      declare
         Ignore : constant Unsigned_64 := syscall (SYSCALL_SLEEP, Poll_MS);
      begin
         null;
      end;
   end loop;
   Check (Opened, "stream over the second interface opens");
   Check (Open (Probes, Startup.Probe, Probe_Target), "probe channel opens");

   while Opened and then not Channels.Final (Stream) and then Now - Started < Test_MS loop
      Pump_Stream;
      if not Running and then Received >= Warm_Up_Bytes then
         Running := True;
         Say ("running");
      end if;
      if Now - Last_Probe >= Poll_MS then
         Last_Probe := Now;
         if Channels.Status (Probes) = Layout.Status_Unreachable then
            Unreachable := True;
         else
            declare
               Sent : Boolean;
            begin
               Channels.Send_Datagram (Probes, Probe'Address, Probe'Length, Sent);
            end;
         end if;
      end if;
   end loop;

   Check (Running and then In_Order and then
          Channels.Status (Stream) = Layout.Status_Peer_Finished,
          "stream kept flowing in order until the fixture ended it");
   Check (not Unreachable, "a default route was always usable for the probes");
   Channels.Close (Stream, Startup.Stream);
   Channels.Close (Probes, Startup.Probe);
   if Passed then
      debugPrint ("TEST: PASS net-multi" & ASCII.LF);
   else
      debugPrint ("TEST: FAIL net-multi" & ASCII.LF);
   end if;
end Main;
