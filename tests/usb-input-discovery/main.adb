with Ada.Text_IO;
with Interfaces; use Interfaces;
with USB_Configurations; use USB_Configurations;
procedure Main is
   Keyboard : Bytes :=
     [9, 2, 25, 0, 1, 1, 0, 128, 50,
      9, 4, 4, 0, 1, 3, 1, 1, 0,
      7, 5, 16#81#, 3, 8, 0, 10];
   Hub : Bytes :=
     [9, 2, 25, 0, 1, 1, 0, 128, 50,
      9, 4, 0, 0, 1, 9, 0, 1, 0,
      7, 5, 16#81#, 3, 1, 0, 12];
   -- Non-boot HID precedes the actual boot keyboard. Endpoint/interface
   -- ownership must survive discovery, irrespective of descriptor order.
   Composite : Bytes :=
     [9, 2, 41, 0, 2, 1, 0, 128, 50,
      9, 4, 0, 0, 1, 3, 0, 0, 0,
      7, 5, 16#82#, 3, 16, 0, 10,
      9, 4, 4, 0, 1, 3, 1, 1, 0,
      7, 5, 16#81#, 3, 8, 0, 10];
   Value : Configuration;
   Result : Decode_Result;
begin
   Decode (Keyboard, Value, Result);
   pragma Assert (Result = Decoded and Value.Keyboard.Present);
   pragma Assert (Value.Keyboard.Number = 4 and Value.Keyboard.Input.Address = 129);
   pragma Assert (not Value.Mouse.Present and not Value.Hub.Present);
   Decode (Composite, Value, Result);
   pragma Assert (Result = Decoded and Value.Keyboard.Present);
   pragma Assert (Value.Keyboard.Number = 4 and Value.Keyboard.Input.Address = 129);
   for Protocol in Unsigned_8 range 0 .. 3 loop
      Hub (17) := Protocol;
      Decode (Hub, Value, Result);
      pragma Assert (Result = Decoded);
      pragma Assert (Value.Hub.Present = (Protocol <= 2));
      if Value.Hub.Present then
         pragma Assert (Value.Hub.Protocol = Protocol);
      end if;
   end loop;
   for Length in 0 .. Composite'Length - 1 loop
      Decode (Composite (1 .. Length), Value, Result);
      pragma Assert (Result = Malformed and not Value.Keyboard.Present);
   end loop;
   Keyboard (23) := 7; -- Too short for the eight-byte boot report.
   Decode (Keyboard, Value, Result);
   pragma Assert (Result = Decoded and not Value.Keyboard.Present);
   Keyboard (23) := 8;
   Keyboard (25) := 0;
   Decode (Keyboard, Value, Result);
   pragma Assert (Result = Decoded and not Value.Keyboard.Present);
   Composite (37) := 16#82#; -- Endpoint claimed by two active interfaces.
   Decode (Composite, Value, Result);
   pragma Assert (Result = Malformed and not Value.Keyboard.Present);
   Ada.Text_IO.Put_Line ("PASS: composite boot keyboard and USB2 hub discovery");
end Main;
