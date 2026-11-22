------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The outlet producer of tests/control-events: as the ipc-test service,
--  it produces records on its outlet 1 and answers readers' channel opens
--  (CuBit.Streams, CuBit.Outlet_Channels) until the test ends.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with CuBit.Protocols;
with CuBit.Streams;
with Startup;
with CuBit.Monotonic;

procedure Main is
   Outlet : constant CuBit.Streams.StreamId := 1;
   Outlet_Pages : constant := 1;
   Text : constant String := "from the producer";
   Poll_Microseconds : constant := 10_000;
   Ignore : Unsigned_64;
   Ignore_Handled : Boolean;
   Ignore_Written : Unsigned_32;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   if registerDriver (DRIVER_IPCTEST, Startup.Registration) = Unsigned_64'Last
   then
      debugPrint ("control-producer: registration FAIL" & ASCII.LF);
      return;
   end if;
   CuBit.Streams.streamCreateTyped
     (Outlet, Outlet_Pages, CuBit.Streams.TYPE_TEXT_LINE, CuBit.Protocols.TEXT_LINE_CONTRACT);
   debugPrint ("control-producer: ready" & ASCII.LF);
   loop
      --  One request per pass: a slow consumer, so a flood backs up (the
      --  isolation test needs one).
      Ignore_Handled := CuBit.Streams.streamHandleSubscription;
      --  Readers that let go free their slots (CuBit.Process_Events).
      CuBit.Process_Events.Poll;
      Ignore_Written := CuBit.Streams.streamWrite
        (Outlet, Text'Address, Text'Length, CuBit.Streams.TYPE_TEXT_LINE);
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
         CuBit.Monotonic.Microseconds
           + Poll_Microseconds);
   end loop;
end Main;
