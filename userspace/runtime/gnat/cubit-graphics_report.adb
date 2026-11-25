------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Graphics_Report with SPARK_Mode is

   --  Word 0: Kind (bits 0-1), Path (bits 2-3), Why (bits 4-7), Stale (bit
   --  8). Word 1: Detail + Detail_Offset (bits 0-31), the
   --  subject's length (bits 32-39), the device's (bits 40-47). Words 2-3:
   --  the subject's text, words 4-5 the device's, eight characters a word
   --  (first in the low byte). Word 6: Since. Word 7: Id.
   Path_Shift   : constant := 2 ** 2;
   Why_Shift    : constant := 2 ** 4;
   Stale_Bit    : constant := 2 ** 8;
   Header_Bits_Limit : constant := 2 ** 9;
   Detail_Offset : constant := 2 ** 31;
   Subject_Shift : constant := 2 ** 32;
   Device_Shift  : constant := 2 ** 40;
   Length_Mask   : constant := 2 ** 8 - 1;
   Detail_Mask   : constant := 2 ** 32 - 1;
   Header_Limit  : constant := 2 ** 48;
   Byte          : constant := 2 ** 8;
   Characters_Per_Word : constant := 8;

   function To_Name (Text : String) return Name is
      Result : Name := No_Name;
   begin
      for I in Text'Range loop
         exit when Result.Length = Name_Capacity;
         Result.Length := Result.Length + 1;
         Result.Text (Result.Length) := (if Text (I) in Name_Character then Text (I) else '?');
      end loop;
      return Result;
   end To_Name;

   --  Characters First .. First + 7 of Text as one word.
   function Text_Word (Item : Name; First : Name_Position) return Wire_Word
     with Pre => First + Characters_Per_Word - 1 <= Name_Capacity;

   function Text_Word (Item : Name; First : Name_Position) return Wire_Word is
      W : Word := 0;
   begin
      for I in reverse 0 .. Characters_Per_Word - 1 loop
         pragma Assert (Item.Text (First + I) in Name_Character);
         W := W * Byte + Word (Character'Pos (Item.Text (First + I)));
         pragma Loop_Invariant (W < 2 ** (8 * (Characters_Per_Word - I) - 1));
      end loop;
      return W;
   end Text_Word;

   function Character_Of (W : Word; Position : Natural) return Word is
     (Shift_Right (W, 8 * Position) and (Byte - 1))
     with Pre => Position < Characters_Per_Word;

   function Valid_Text (W : Word) return Boolean is
     (for all P in 0 .. Characters_Per_Word - 1 =>
        Character_Of (W, P) in Character'Pos (Name_Character'First) .. Character'Pos (Name_Character'Last));

   --  A name of Length characters from two text words.
   function Name_Of (Length : Word; Low, High : Word) return Name
     with Pre => Length <= Name_Capacity and then Valid_Text (Low) and then Valid_Text (High);

   function Name_Of (Length : Word; Low, High : Word) return Name is
      Result : Name := (Length => Name_Length (Length), Text => [others => ' ']);
   begin
      for P in 0 .. Characters_Per_Word - 1 loop
         Result.Text (1 + P) := Character'Val (Character_Of (Low, P));
         Result.Text (1 + Characters_Per_Word + P) := Character'Val (Character_Of (High, P));
      end loop;
      return Result;
   end Name_Of;

   function Encode (Item : Report_Entry) return Entry_Wire is
      Header : constant Word :=
        Word (Subject_Kind'Pos (Item.Kind)) + Word (Path_Kind'Pos (Item.Path)) * Path_Shift
        + Word (Reason'Pos (Item.Why)) * Why_Shift + (if Item.Stale then Stale_Bit else 0);
      Lengths : constant Word :=
        Word (Integer_64 (Item.Detail) + Detail_Offset) + Word (Item.Subject.Length) * Subject_Shift
        + Word (Item.Device.Length) * Device_Shift;
   begin
      return [0 => Header, 1 => Lengths,
              2 => Text_Word (Item.Subject, 1), 3 => Text_Word (Item.Subject, 9),
              4 => Text_Word (Item.Device, 1), 5 => Text_Word (Item.Device, 9),
              6 => Item.Since, 7 => Item.Id];
   end Encode;

   function Kind_Of (W : Word) return Word is (W and 3);
   function Path_Of (W : Word) return Word is (Shift_Right (W, 2) and 3);
   function Why_Of (W : Word) return Word is (Shift_Right (W, 4) and 15);

   function Valid (Wire : Entry_Wire) return Boolean is
     (Wire (0) < Header_Bits_Limit
      and then Kind_Of (Wire (0)) <= Subject_Kind'Pos (Subject_Kind'Last)
      and then Path_Of (Wire (0)) <= Path_Kind'Pos (Path_Kind'Last)
      and then Why_Of (Wire (0)) <= Reason'Pos (Reason'Last)
      and then (Path_Of (Wire (0)) = Path_Kind'Pos (Software)) = (Why_Of (Wire (0)) /= Reason'Pos (None))
      and then Wire (1) < Header_Limit
      and then (Wire (1) and Detail_Mask) in 1 .. 2 * Detail_Offset - 1
      and then (Shift_Right (Wire (1), 32) and Length_Mask) <= Name_Capacity
      and then Shift_Right (Wire (1), 40) <= Name_Capacity
      and then Valid_Text (Wire (2)) and then Valid_Text (Wire (3))
      and then Valid_Text (Wire (4)) and then Valid_Text (Wire (5))
      and then Wire (6) <= Maximum_Milliseconds);

   function Decode (Wire : Entry_Wire) return Report_Entry is
     ((Kind    => Subject_Kind'Val (Kind_Of (Wire (0))),
       Path    => Path_Kind'Val (Path_Of (Wire (0))),
       Why     => Reason'Val (Why_Of (Wire (0))),
       Stale   => (Wire (0) and Stale_Bit) /= 0,
       Id      => Subject_Id (Wire (7)),
       Detail  => Detail_Value (Integer_64 (Wire (1) and Detail_Mask) - Detail_Offset),
       Subject => Name_Of (Shift_Right (Wire (1), 32) and Length_Mask, Wire (2), Wire (3)),
       Device  => Name_Of (Shift_Right (Wire (1), 40), Wire (4), Wire (5)),
       Since   => Wire (6)));

end CuBit.Graphics_Report;
