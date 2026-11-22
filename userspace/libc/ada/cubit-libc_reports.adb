------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Reports with SPARK_Mode is

   Lead : constant String := "cubit-libc: ";
   LF : constant Character := Character'Val (10);
   Digits_Bytes : constant := 20;     --  Unsigned_64'Last has 20 digits

   procedure Format
     (Prefix, What : String; Value : Integer_64;
      Result : out Line; Length : out Line_Length)
   is
      Kept : constant Natural := Natural'Min (What'Length, What_Bytes);
      Magnitude : Unsigned_64 :=
        (if Value < 0 then Unsigned_64'Mod (-(Value + 1)) + 1
         else Unsigned_64 (Value));
      Text : String (1 .. Digits_Bytes) := [others => '0'];
      First : Positive range 1 .. Digits_Bytes := Digits_Bytes;

      procedure Put (Item : String)
      with Pre => Length <= Line_Bytes and then Item'Length <= Line_Bytes - Length,
           Post => Length = Length'Old + Item'Length;
      procedure Put (Item : String) is
      begin
         Result (Length + 1 .. Length + Item'Length) := Item;
         Length := Length + Item'Length;
      end Put;
   begin
      Result := [others => ' '];
      Length := 0;
      --  Digits, least significant first, into the end of Text.
      loop
         Text (First) := Character'Val (Character'Pos ('0') + Natural (Magnitude mod 10));
         Magnitude := Magnitude / 10;
         exit when Magnitude = 0 or else First = 1;
         First := First - 1;
      end loop;
      Put (Lead);
      Put (Prefix);
      Put (" ");
      Put (What (What'First .. What'First + Kept - 1));
      Put (" ");
      if Value < 0 then
         Put ("-");
      end if;
      Put (Text (First .. Digits_Bytes));
      Put ([LF]);
   end Format;

   procedure First_Time
     (Seen : in out Seen_Table; What : String; Value : Integer_64;
      Is_New : out Boolean)
   is
      Kept : constant Natural := Natural'Min (What'Length, What_Bytes);
   begin
      for K in 1 .. Seen.Count loop
         if Seen.Entries (K).Value = Value
           and then Seen.Entries (K).Length = Kept
           and then Seen.Entries (K).What (1 .. Kept) =
                    What (What'First .. What'First + Kept - 1)
         then
            Is_New := False;
            return;
         end if;
      end loop;
      Is_New := True;
      if Seen.Count < Maximum_Seen then
         Seen.Count := Seen.Count + 1;
         Seen.Entries (Seen.Count).What (1 .. Kept) :=
           What (What'First .. What'First + Kept - 1);
         Seen.Entries (Seen.Count).Length := Kept;
         Seen.Entries (Seen.Count).Value := Value;
      end if;
   end First_Time;

end CuBit.Libc_Reports;
