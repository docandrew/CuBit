with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Records; use CuBit.Log_Records;
with CuBit.Text_To_Log; use CuBit.Text_To_Log;
procedure Main is
   Bytes, Bad : Wire_Buffer;
   Used : Wire_Count;
   Value, Again : Decoded;
   Full : constant String (1 .. Maximum_Text_Bytes) := [others => 'x'];
   Count : Natural := 0;
   Times : constant array (1 .. 3) of Timestamp :=
     [(Clock => Unspecified),
      (Monotonic_Milliseconds, Unsigned_64'Last, Unsigned_64'Last),
      (Unix_Milliseconds, Unsigned_64'Last)];
   Output : Step;
   Stream : Adapter (Warning);
   Now : constant Timestamp := (Monotonic_Milliseconds, 7, 123);
   procedure Push (Chunk : String) is
   begin
      for C of Chunk loop
         Feed (Stream, C, Now, Output);
         pragma Assert (Kind (Output) = Need_More);
      end loop;
   end Push;
   procedure Line (Expected : String) is
   begin
      Feed (Stream, ASCII.LF, Now, Output);
      pragma Assert (Kind (Output) = Record_Ready);
      pragma Assert (Text (CuBit.Text_To_Log.Value (Output)) = Expected);
      pragma Assert (Level (CuBit.Text_To_Log.Value (Output)) = Warning);
      pragma Assert (Reported_Time (CuBit.Text_To_Log.Value (Output)) = Now);
   end Line;
   function C (B : Natural) return Character is (Character'Val (B));
begin
   for Importance in Severity loop
      for Time of Times loop
         for Length in Text_Count loop
            Value := Make (Full (1 .. Length), Importance, Time);
            pragma Assert (Value.Success);
            Encode (Value.Value, Bytes, Used);
            pragma Assert (Used = Header_Bytes + Length);
            Again := Decode (Bytes, Used);
            pragma Assert (Again.Success and then Again.Value = Value.Value);
            pragma Assert
              (for all I in Used + 1 .. Wire_Count'Last => Bytes (I) = 0);
            Count := Count + 1;
         end loop;
      end loop;
   end loop;
   --  Independent known encoding, including little-endian time and domain.
   Value := Make ("A", Warning, (Monotonic_Milliseconds, 7, 16#0102#));
   pragma Assert (Value.Success);
   Encode (Value.Value, Bytes, Used);
   Bad := [others => 0];
   Bad (1 .. 10) := [16#43#, 16#4C#, 16#4F#, 16#47#, 1, 3, 1, 0, 1, 0];
   Bad (17) := 2; Bad (18) := 1; Bad (25) := 7; Bad (33) := 65;
   pragma Assert (Bytes = Bad and Used = 33);
   for Length in Wire_Count loop
      Again := Decode (Bytes, Length);
      pragma Assert (Again.Success = (Length = Used));
   end loop;
   for I in 1 .. 16 loop
      Bad := Bytes;
      Bad (I) := 255;
      Again := Decode (Bad, Used);
      pragma Assert (not Again.Success);
   end loop;
   Bad := Bytes; Bad (25 .. 32) := [others => 0];
   pragma Assert (not Decode (Bad, Used).Success);
   Bad := Bytes; Bad (7) := 0;
   pragma Assert (not Decode (Bad, Used).Success);
   Bad := Bytes; Bad (7) := 2;
   pragma Assert (not Decode (Bad, Used).Success);
   for B in 0 .. 255 loop
      Bad := Bytes; Bad (33) := Unsigned_8 (B);
      pragma Assert
        (Decode (Bad, Used).Success = (B = 9 or B in 32 .. 126));
   end loop;
   for First in 0 .. 255 loop
      for Second in 0 .. 255 loop
         pragma Assert
           (Valid_Text (C (First) & C (Second)) =
            (((First = 9 or First in 32 .. 126) and
              (Second = 9 or Second in 32 .. 126)) or
             (First in 16#C2# .. 16#DF# and
              Second in 16#80# .. 16#BF#)));
      end loop;
   end loop;
   pragma Assert (not Make (Full & "x").Success);
   --  UTF-8 shortest encodings, surrogate rejection, scalar upper bound.
   pragma Assert (Valid_Text (C (16#C2#) & C (16#80#)));
   pragma Assert (Valid_Text (C (16#E0#) & C (16#A0#) & C (16#80#)));
   pragma Assert (Valid_Text (C (16#F4#) & C (16#8F#) & C (16#BF#) & C (16#BF#)));
   pragma Assert (not Valid_Text (C (16#C0#) & C (16#80#)));
   pragma Assert (not Valid_Text (C (16#E0#) & C (16#9F#) & C (16#BF#)));
   pragma Assert (not Valid_Text (C (16#ED#) & C (16#A0#) & C (16#80#)));
   pragma Assert (not Valid_Text (C (16#F4#) & C (16#90#) & C (16#80#) & C (16#80#)));
   pragma Assert (not Valid_Text (C (16#E2#) & C (16#82#)));
   pragma Assert (not Valid_Text ("escape" & ASCII.ESC));
   Value := Make (C (16#E2#) & C (16#82#) & C (16#AC#));
   pragma Assert (Value.Success);
   Encode (Value.Value, Bytes, Used);
   Again := Decode (Bytes, Used);
   pragma Assert (Again.Success and then Again.Value = Value.Value);
   --  CRLF and UTF-8 split across independent incoming chunks.
   Push ("hel"); Push ("lo" & ASCII.CR); Line ("hello");
   Push ("" & C (16#E2#)); Push ("" & C (16#82#)); Push ("" & C (16#AC#));
   Line (C (16#E2#) & C (16#82#) & C (16#AC#));
   Line ("");
   Push (Full); Push ("" & ASCII.CR); Line (Full);
   Push (Full); Push ("overflow ignored");
   Feed (Stream, ASCII.LF, Now, Output);
   pragma Assert (Kind (Output) = Line_Dropped and then Reason (Output) = Oversized_Line);
   Push ("recovered"); Line ("recovered");
   Push ("bad" & ASCII.CR & "line");
   Feed (Stream, ASCII.LF, Now, Output);
   pragma Assert (Kind (Output) = Line_Dropped and then Reason (Output) = Invalid_UTF8_Or_Control);
   Push ("tail"); Finish (Stream, Now, Output);
   pragma Assert (Kind (Output) = Record_Ready and then Text (CuBit.Text_To_Log.Value (Output)) = "tail");
   Finish (Stream, Now, Output); pragma Assert (Kind (Output) = Need_More);
   Push ("" & ASCII.CR); Finish (Stream, Now, Output);
   pragma Assert (Kind (Output) = Line_Dropped);
   Push ("" & C (16#E2#)); Finish (Stream, Now, Output);
   pragma Assert (Kind (Output) = Line_Dropped);
   Push ("before lost bytes");
   Report_Gap (Stream, Output);
   pragma Assert (Kind (Output) = Line_Dropped and then Reason (Output) = Upstream_Gap);
   Push ("surviving suffix" & ASCII.CR & ASCII.LF);
   Push ("complete after gap"); Line ("complete after gap");
   Report_Gap (Stream, Output);
   Push ("unterminated suffix"); Finish (Stream, Now, Output);
   pragma Assert (Kind (Output) = Need_More);
   Push ("fresh stream"); Line ("fresh stream");
   Put_Line ("PASS:" & Count'Image & " log wire round trips + malformed wire/UTF-8/framing tests");
end Main;
