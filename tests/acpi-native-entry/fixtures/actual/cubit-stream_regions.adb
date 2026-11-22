------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Datagram_Rings;

package body CuBit.Stream_Regions is

   package DR renames CuBit.Datagram_Rings;
   use type Rings.Index;
   use type Rings.Count;
   use type SR.Publish_Result;
   use type SR.Read_Result;
   use type DR.Put_Result;

   --  A torn copy is retried this many times before the read gives up.
   Read_Attempts : constant := 4;
   Word_Bytes : constant := Rings.Index'Size / 8;

   function At_Byte (Base : Unsigned_64; Offset : Natural) return System.Address is
     (To_Address (Integer_Address (Base + Unsigned_64 (Offset))));

   function Word (Base : Unsigned_64; Offset : Natural) return Rings.Index;
   function Word (Base : Unsigned_64; Offset : Natural) return Rings.Index is
      Value : constant Rings.Index with Import, Volatile, Address => At_Byte (Base, Offset);
   begin
      return Value;
   end Word;

   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Rings.Index);
   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Rings.Index) is
      Target : Rings.Index with Import, Volatile, Address => At_Byte (Base, Offset);
   begin
      Target := Value;
   end Set_Word;

   --  Stores and loads stay in program order on x86-64; the compiler must
   --  keep them so too.
   procedure Fence;
   procedure Fence is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Fence;

   procedure Initialize (Base : Unsigned_64; Element : Unsigned_16) is
   begin
      for Offset in 0 .. SR.CONTROL_BYTES / Word_Bytes - 1 loop
         Set_Word (Base, Offset * Word_Bytes, 0);
      end loop;
      Set_Word (Base, SR.ELEMENT_OFFSET, Rings.Index (Element));
   end Initialize;

   function Writer_Of (Base : Unsigned_64; Pages : Natural) return Rings.Producer is
      Size : constant Rings.Ring_Size := SR.Ring_Bytes (Declared (Pages));
      Now : constant Rings.Index := Word (Base, SR.PRODUCED_OFFSET);
      Fill : constant Rings.Count := Rings.Distance (Word (Base, SR.OLDEST_OFFSET), Now);
   begin
      --  Indices that make no sense start the ring over from PRODUCED.
      return (Size => Size, Produced => Now,
              Fill => (if Fill <= Rings.Count (Size) then Natural (Fill) else 0));
   end Writer_Of;

   function Write
     (Base : Unsigned_64; Writer : in out Rings.Producer;
      Data : System.Address; Length : Natural) return Boolean
   is
      Ring : Rings.Bytes (0 .. Writer.Size - 1)
        with Import, Address => At_Byte (Base, SR.CONTROL_BYTES);
      Ignore_Evicted : Natural;
      Result : SR.Publish_Result;
      Put : DR.Put_Result;
   begin
      if not SR.Fits (Writer.Size, Length) then
         return False;
      end if;
      declare
         Source : constant Rings.Bytes (0 .. Length - 1) with Import, Address => Data;
      begin
         SR.Make_Room (Writer, Ring, Length, Ignore_Evicted, Result);
         if Result /= SR.Published then
            return False;
         end if;
         Set_Word (Base, SR.OLDEST_OFFSET, Rings.Consumed (Writer));
         Fence;
         DR.Put (Writer, Ring, Source, Put);
         if Put /= DR.Put then
            return False;
         end if;
         Fence;
         Set_Word (Base, SR.PRODUCED_OFFSET, Writer.Produced);
         return True;
      end;
   end Write;

   function Read
     (Base : Unsigned_64; Size : Rings.Ring_Size; Cursor : in out Rings.Index;
      Buffer : System.Address; Maximum : Natural) return Natural
   is
      Ring : constant Rings.Bytes (0 .. Size - 1)
        with Import, Address => At_Byte (Base, SR.CONTROL_BYTES);
      Into : Rings.Bytes (0 .. Maximum - 1) with Import, Address => Buffer;
      Length : Natural;
      Ignore_Truncated, Lost : Boolean;
      Result : SR.Read_Result;
   begin
      for Attempt in 1 .. Read_Attempts loop
         declare
            Start : constant Rings.Index := Cursor;
            Now : constant Rings.Index := Word (Base, SR.PRODUCED_OFFSET);
            Oldest : Rings.Index;
         begin
            Fence;
            Oldest := Word (Base, SR.OLDEST_OFFSET);
            SR.Read (Cursor, Size, Ring, Now, Oldest, Into, Length, Ignore_Truncated, Lost,
                     Result);
            if Result /= SR.Taken then
               if Result = SR.Malformed then
                  Cursor := Word (Base, SR.PRODUCED_OFFSET);   --  start over
               end if;
               return 0;
            end if;
            Fence;
            if SR.Intact ((if Lost then Oldest else Start),
                          Word (Base, SR.PRODUCED_OFFSET), Word (Base, SR.OLDEST_OFFSET))
            then
               return Length;
            end if;
            Cursor := Word (Base, SR.OLDEST_OFFSET);
         end;
      end loop;
      return 0;
   end Read;

   function Read_Owned
     (Base : Unsigned_64; Pages : Natural; Buffer : System.Address;
      Maximum : Natural) return Natural
   is
      Cursor : Rings.Index := Word (Base, SR.OWNER_CURSOR_OFFSET);
      Length : Natural;
   begin
      Length := Read (Base, SR.Ring_Bytes (Declared (Pages)), Cursor, Buffer, Maximum);
      Set_Word (Base, SR.OWNER_CURSOR_OFFSET, Cursor);
      return Length;
   end Read_Owned;

   function Produced (Base : Unsigned_64) return Rings.Index is
     (Word (Base, SR.PRODUCED_OFFSET));

   function Element (Base : Unsigned_64) return Unsigned_16 is
     (Unsigned_16 (Word (Base, SR.ELEMENT_OFFSET) and 16#FFFF#));

end CuBit.Stream_Regions;
