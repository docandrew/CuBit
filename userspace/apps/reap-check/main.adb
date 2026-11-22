------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Dead-client reaping check (headless --test desktop-reap): a client that
--  holds a window but no surface ring and no buffer ends without saying
--  goodbye. Nothing it held tells Desktop it ended, and nothing else redraws
--  afterwards: Desktop must still remove the window ("desktop: surfaceN
--  ended: its process ended").
------------------------------------------------------------------------------
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Waits;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
with UI_Desktop_Service;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   Settle_Ms : constant := 4_000;
   function Send (Wire : Wire_Message) return Wire_Message is
      Msg : Message := From_Wire (Wire);
   begin
      Msg.tag := capCall (UI_Desktop_Service.Endpoint, Msg, Deadline_After (5_000));
      return To_Wire (Msg);
   end Send;
   Created : constant Creation_Result :=
     Decode_Creation_Result (Send (Encode_Create ((64, 48, Plain_Surface, Desktop_Sized))));
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   if Created.Status /= Success then
      debugPrint ("reap-check: FAIL no surface" & ASCII.LF);
      return;
   end if;
   --  Let Desktop's start settle first: the test is about a window that
   --  ends while nothing is composed.
   declare
      Ignore : constant Unsigned_64 := CuBit.Waits.Sleep_For (Settle_Ms);
   begin
      null;
   end;
   declare
      Text : constant String := Unsigned_64'Image (Unsigned_64 (Created.Surface));
   begin
      debugPrint ("reap-check: surface" & Text & " held; ending without goodbye" & ASCII.LF);
   end;
end Main;
