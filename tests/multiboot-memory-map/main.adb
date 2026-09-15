with Ada.Text_IO;
with Interfaces; use Interfaces;
with Multiboot_Memory_Map; use Multiboot_Memory_Map;

procedure Main is
   Checks : Natural := 0;
   Physical_Last : constant Unsigned_64 := 2 ** 48 - 1;
   Seed : Unsigned_32 := 16#B007CAFE#;
   Data : Bytes (7 .. 262) := [others => 0];
   Output : Entries (11 .. 18);
   Count, Following : Natural;
   Result : Status;
   Item : Decoded_Region;

   procedure Put_32 (Buffer : in out Bytes; Offset : Natural; Value : Unsigned_32) is
      Rest : Unsigned_32 := Value;
   begin
      for I in 0 .. 3 loop
         Buffer (Buffer'First + Offset + I) := Unsigned_8 (Rest mod 256);
         Rest := Rest / 256;
      end loop;
   end Put_32;

   procedure Put_64 (Buffer : in out Bytes; Offset : Natural; Value : Unsigned_64) is
      Rest : Unsigned_64 := Value;
   begin
      for I in 0 .. 7 loop
         Buffer (Buffer'First + Offset + I) := Unsigned_8 (Rest mod 256);
         Rest := Rest / 256;
      end loop;
   end Put_64;

   procedure Encode (Buffer : in out Bytes; Offset : Natural;
                     Size : Unsigned_32; Base, Length : Unsigned_64; Tag : Unsigned_32) is
   begin
      Put_32 (Buffer, Offset, Size);
      Put_64 (Buffer, Offset + 4, Base);
      Put_64 (Buffer, Offset + 12, Length);
      Put_32 (Buffer, Offset + 20, Tag);
   end Encode;

   procedure Expect (Length : Natural; Expected : Status; Entries_Expected : Natural := 0) is
   begin
      Parse (Data (Data'First .. Data'First + Length - 1), Physical_Last,
             Output, Count, Result);
      pragma Assert (Result = Expected);
      pragma Assert (Count = Entries_Expected);
      Checks := Checks + 1;
   end Expect;

   function Random return Unsigned_32 is
   begin
      Seed := Seed * 1_664_525 + 1_013_904_223;
      return Seed;
   end Random;

   procedure Fuzz_Oracle (Buffer : Bytes) is
      Pos : Natural := 0;
      Seen : Natural := 0;
      Expected_Status : Status := Empty_Map;
      Expected : Entries (Output'Range);
      function Number (Offset, Width : Natural) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         -- Opposite traversal from the production decoder, using radix math.
         for I in reverse 0 .. Width - 1 loop
            V := V * 256 + Unsigned_64 (Buffer (Buffer'First + Offset + I));
         end loop;
         return V;
      end Number;
   begin
      while Pos < Buffer'Length loop
         if Seen = Expected'Length then
            Expected_Status := Capacity_Exceeded;
            exit;
         elsif Buffer'Length - Pos < 4 then
            Expected_Status := Truncated_Header;
            exit;
         end if;
         declare
            Size : constant Unsigned_64 := Number (Pos, 4);
         begin
            if Size < 20 then
               Expected_Status := Short_Record;
               exit;
            elsif Size + 4 > Unsigned_64 (Buffer'Length - Pos) then
               Expected_Status := Truncated_Record;
               exit;
            end if;
            declare
               Base : constant Unsigned_64 := Number (Pos + 4, 8);
               Length : constant Unsigned_64 := Number (Pos + 12, 8);
               Tag : constant Unsigned_64 := Number (Pos + 20, 4);
               E : Decoded_Region;
            begin
               if Length /= 0 then
                  -- Wide modular addition with explicit wrap detection is
                  -- independent of the production subtraction-based check.
                  if Base > Physical_Last or else Base + (Length - 1) < Base
                    or else Base + (Length - 1) > Physical_Last
                  then
                     Expected_Status := Address_Out_Of_Range;
                     exit;
                  end if;
                  E := (First => Base, Last => Base + (Length - 1), Empty => False,
                    Kind => (case Tag is when 1 => Usable, when 3 => ACPI_Reclaim,
                      when 4 => ACPI_NVS, when 5 => Defective, when others => Reserved));
               end if;
               Expected (Expected'First + Seen) := E;
            end;
            Seen := Seen + 1;
            Pos := Pos + Natural (Size) + 4;
            Expected_Status := Success;
         end;
      end loop;
      Parse (Buffer, Physical_Last, Output, Count, Result);
      pragma Assert (Result = Expected_Status);
      if Result = Success then
         pragma Assert (Count = Seen);
         for I in Expected'First .. Expected'First + Seen - 1 loop
            pragma Assert (Output (I) = Expected (I));
         end loop;
      else
         pragma Assert (Count = 0);
      end if;
      Checks := Checks + 1;
   end Fuzz_Oracle;
begin
   Encode (Data, 0, 20, 16#1234_5678_9ABC#, 16#1001#, 1);
   Expect (24, Success, 1);
   pragma Assert (Output (11).First = 16#1234_5678_9ABC#);
   pragma Assert (Output (11).Last = 16#1234_5678_AABC#);
   for Length in 0 .. 23 loop
      Expect (Length, (if Length = 0 then Empty_Map elsif Length < 4 then Truncated_Header
                       else Truncated_Record));
   end loop;
   for Size in Unsigned_32 range 0 .. 19 loop
      Put_32 (Data, 0, Size);
      Expect (24, Short_Record);
   end loop;
   Put_32 (Data, 0, Unsigned_32'Last);
   Expect (24, Truncated_Record);

   -- Mixed 28-byte and 24-byte records: the old fixed-size count was wrong.
   Data := [others => 16#A5#];
   Encode (Data, 0, 24, 16#1000#, 16#2000#, 1);
   Encode (Data, 28, 20, 16#9000#, 1, 99);
   Expect (52, Success, 2);
   pragma Assert (Output (12).First = 16#9000# and Output (12).Kind = Reserved);
   for Length in 29 .. 51 loop
      Expect (Length, (if Length < 32 then Truncated_Header else Truncated_Record));
   end loop;
   for Tag in Unsigned_32 range 0 .. 8 loop
      Encode (Data, 0, 20, 16#1000#, 1, Tag);
      Fuzz_Oracle (Data (7 .. 30));
   end loop;
   Encode (Data, 0, 20, Physical_Last, 1, 1);
   Expect (24, Success, 1);
   Encode (Data, 0, 20, Physical_Last, 2, 1);
   Expect (24, Address_Out_Of_Range);
   Encode (Data, 0, 20, Unsigned_64'Last, 2, 1);
   Expect (24, Address_Out_Of_Range);
   Encode (Data, 0, 20, Unsigned_64'Last, 0, 1);
   Expect (24, Success, 1);
   pragma Assert (Output (11).Empty);
   Encode (Data, 0, 20, 0, Unsigned_64'Last, 1);
   Next_Entry (Data (7 .. 30), 0, Unsigned_64'Last, Following, Item, Result);
   pragma Assert (Result = Success and Following = 24 and Item.Last = Unsigned_64'Last - 1);

   -- Exact capacity, excess entries and an empty output array.
   for I in 0 .. 8 loop
      Encode (Data, I * 24, 20, Unsigned_64 (I * 4096), 4096, 1);
   end loop;
   Expect (8 * 24, Success, 8);
   Expect (9 * 24, Capacity_Exceeded);
   declare
      None : Entries (1 .. 0);
   begin
      Parse (Data (7 .. 30), Physical_Last, None, Count, Result);
      pragma Assert (Result = Capacity_Exceeded and Count = 0);
   end;
   -- A very high array lower bound must not overflow decoder index arithmetic.
   declare
      High_Buffer : Bytes (Buffer_Index'Last - 23 .. Buffer_Index'Last);
      High_Output : Entries (Positive'Last .. Positive'Last);
   begin
      Encode (High_Buffer, 0, 20, 16#1234#, 99, 3);
      Parse (High_Buffer, Physical_Last, High_Output, Count, Result);
      pragma Assert (Result = Success and Count = 1);
      pragma Assert (High_Output (Positive'Last).Kind = ACPI_Reclaim);
   end;

   for Trial in 1 .. 25_000 loop
      for B of Data loop
         B := Unsigned_8 (Shift_Right (Random, 24));
      end loop;
      Fuzz_Oracle (Data (7 .. 7 + Natural (Random mod 256) - 1));
      -- Structured random records exercise successful parsing and later damage.
      declare
         Pos : Natural := 0;
         Record_Count : constant Natural := 1 + Natural (Random mod 6);
      begin
         for I in 1 .. Record_Count loop
            declare
               Size : constant Unsigned_32 := 20 + Random mod 9;
               Base : constant Unsigned_64 := Unsigned_64 (Random) * 4096;
            begin
               Encode (Data, Pos, Size, Base, Unsigned_64 (Random), Random mod 8);
               Pos := Pos + Natural (Size) + 4;
            end;
         end loop;
         Fuzz_Oracle (Data (7 .. 7 + Pos - 1));
         Fuzz_Oracle (Data (7 .. 7 + Natural (Random mod Unsigned_32 (Pos)) - 1));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS boot-map decoder:" & Checks'Image & " cases; " &
     "extended/truncated records, byte oracle, overflow, capacity, high indices");
end Main;
