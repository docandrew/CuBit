------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Start_Layout with SPARK_Mode is

   procedure Locate_Strings
     (Item : Block; First : out Starts; Count : out String_Count)
   is
      Position : Cursor := Header_Bytes + 1;
      Unused_Last : Natural;     --  the string's end: not needed here
      Next : Cursor;
      Wanted : constant String_Count := Strings_Declared (Item);
   begin
      First := [others => Block_Index'First];
      Count := 0;
      while Count < Wanted loop
         pragma Loop_Invariant (Count < Wanted);
         pragma Loop_Invariant
           (for all K in 1 .. Count => First (K) in Header_Bytes + 1 .. Strings_Last (Item));
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Strings_Last (Item) + 1);
         pragma Loop_Variant (Increases => Count);
         exit when Position > Strings_Last (Item);
         Next_String (Item, Position, Unused_Last, Next);
         Count := Count + 1;
         First (Count) := Position;
         Position := Next;
      end loop;
   end Locate_Strings;

end CuBit.Libc_Start_Layout;
