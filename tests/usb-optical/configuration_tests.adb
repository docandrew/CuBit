with Ada.Text_IO;
with Interfaces; use Interfaces;
with USB_Configurations; use USB_Configurations;

procedure Configuration_Tests is
   Mouse : Bytes (1 .. 25) :=
     [9, 2, 25, 0, 1, 1, 0, 128, 50,
      9, 4, 3, 0, 1, 3, 1, 2, 0,
      7, 5, 16#81#, 3, 4, 0, 10];
   Storage : Bytes (1 .. 32) :=
     [9, 2, 32, 0, 1, 1, 0, 128, 50,
      9, 4, 4, 0, 2, 8, 6, 16#50#, 0,
      7, 5, 16#82#, 2, 0, 2, 0,
      7, 5, 16#03#, 2, 0, 2, 0];
   Composite : Bytes (1 .. 48) := Mouse & Storage (10 .. 32);
   Value : Configuration;
   Result : Decode_Result;
begin
   Decode (Mouse, Value, Result);
   pragma Assert (Result = Decoded and Value.Mouse.Present);
   pragma Assert (Value.Mouse.Number = 3 and Value.Mouse.Input.Address = 129);
   pragma Assert (not Value.Storage.Present);
   Decode (Storage, Value, Result);
   pragma Assert (Result = Decoded and Value.Storage.Present);
   pragma Assert (Value.Storage.Number = 4 and
     Value.Storage.Input.Address = 130 and Value.Storage.Output.Address = 3);
   pragma Assert (Value.Storage.Input.Packet_Bytes = 512);

   Composite (3) := 48;
   Composite (5) := 2;
   Decode (Composite, Value, Result);
   pragma Assert (Result = Decoded and Value.Mouse.Present and Value.Storage.Present);
   pragma Assert (Value.Mouse.Number = 3 and Value.Storage.Number = 4);
   Composite (28) := 3; -- Duplicate default-alternate interface identity.
   Decode (Composite, Value, Result);
   pragma Assert (Result = Malformed and not Value.Mouse.Present);
   Composite (28) := 4;
   Composite (37) := 16#81#; -- Reuses the mouse's active endpoint.
   Decode (Composite, Value, Result);
   pragma Assert (Result = Malformed and not Value.Mouse.Present);
   Composite (37) := 16#82#;

   for Length in 0 .. 31 loop
      Decode (Storage (1 .. Length), Value, Result);
      pragma Assert (Result = Malformed);
      pragma Assert (not Value.Mouse.Present and not Value.Storage.Present);
   end loop;
   for Length in Unsigned_8 range 0 .. 255 loop
      Storage (19) := Length;
      Decode (Storage, Value, Result);
      --  Only the original exact seven-byte endpoint leads to this interface.
      if Length /= 7 then
         pragma Assert (not Value.Storage.Present);
      end if;
   end loop;
   Storage (19) := 7;
   Storage (28) := 16#83#; -- Two IN endpoints, never a bulk IN/OUT pair.
   Decode (Storage, Value, Result);
   pragma Assert (not Value.Storage.Present);
   Storage (28) := 3;
   Storage (13) := 1; -- Alternate setting one requires explicit selection.
   Decode (Storage, Value, Result);
   pragma Assert (not Value.Storage.Present);
   Storage (13) := 0;
   Storage (17) := 16#62#; -- UAS is not BOT.
   Decode (Storage, Value, Result);
   pragma Assert (not Value.Storage.Present);
   Storage (17) := 16#50#;
   for Address in Unsigned_8 loop
      Storage (21) := Address;
      Decode (Storage, Value, Result);
      pragma Assert (Value.Storage.Present = (Address in 129 .. 143));
   end loop;

   --  Endpoints from two incomplete interfaces must never be spliced together.
   declare
      Split : Bytes (1 .. 41) :=
        Storage (1 .. 25) & Storage (10 .. 18) & Storage (26 .. 32);
   begin
      Split (3) := 41;
      Split (5) := 2;
      Split (21) := 16#82#;
      Split (14) := 1;
      Split (28) := 5;
      Split (30) := 1;
      Decode (Split, Value, Result);
      pragma Assert (Result = Decoded and not Value.Storage.Present);
   end;
   --  An unrelated later interface cannot change the retained mouse number.
   Composite (31) := 255;
   Decode (Composite, Value, Result);
   pragma Assert (Result = Decoded and Value.Mouse.Number = 3);
   Mouse (25) := 0;
   Decode (Mouse, Value, Result);
   pragma Assert (not Value.Mouse.Present);
   Ada.Text_IO.Put_Line ("USB-CONFIGURATIONS: PASS bounded interface discovery");
end Configuration_Tests;
