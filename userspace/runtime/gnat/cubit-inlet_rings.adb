------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Rings;
with CuBit.Stream_Rings;
with CuBit.Stream_Regions;

package body CuBit.Inlet_Rings is

   package Rings renames CuBit.Channel_Rings;
   package SR renames CuBit.Stream_Rings;
   use type Rings.Index, Rings.Count;

   function Word (Base : Unsigned_64; Offset : Natural) return Rings.Index;
   function Word (Base : Unsigned_64; Offset : Natural) return Rings.Index is
      Value : constant Rings.Index with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
   begin
      return Value;
   end Word;

   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Rings.Index);
   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Rings.Index) is
      Target : Rings.Index with Import, Volatile,
        Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
   begin
      Target := Value;
   end Set_Word;

   function Write (Base : Unsigned_64; Pages : Natural; Data : System.Address;
                   Length : Natural) return Put_Result
   is
      Size : constant Rings.Ring_Size := SR.Ring_Bytes (CuBit.Stream_Regions.Declared (Pages));
      Produced : constant Rings.Index := Word (Base, SR.PRODUCED_OFFSET);
      Unread : constant Rings.Count := Rings.Distance (Word (Base, SR.CONSUMER_OFFSET), Produced);
      --  OLDEST becomes the reader's cursor: what it has read may go.
      Writer : Rings.Producer;
   begin
      if Length = 0 or else Length > Maximum_Record or else not SR.Fits (Size, Length) then
         return Too_Large;
      elsif Unread > Rings.Count (Size) then
         return Full;                       --  a cursor that makes no sense
      end if;
      Writer := (Size => Size, Produced => Produced, Fill => Natural (Unread));
      if not SR.Room (Writer, SR.Record_Bytes (Length)) then
         return Full;
      end if;
      return (if CuBit.Stream_Regions.Write (Base, Writer, Data, Length) then Put else Too_Large);
   end Write;

   procedure Finish (Base : Unsigned_64) is
   begin
      Set_Word (Base, SR.ENDED_OFFSET, 1);
   end Finish;

   function Take (Base : Unsigned_64; Pages : Natural; Buffer : System.Address;
                  Maximum : Natural) return Natural
   is
      Size : constant Rings.Ring_Size := SR.Ring_Bytes (CuBit.Stream_Regions.Declared (Pages));
      Cursor : Rings.Index := Word (Base, SR.CONSUMER_OFFSET);
      Got : Natural;
   begin
      Got := CuBit.Stream_Regions.Read (Base, Size, Cursor, Buffer, Maximum);
      if Got > 0 then
         --  After the copy: the producer may reuse the record's bytes now.
         Set_Word (Base, SR.CONSUMER_OFFSET, Cursor);
      end if;
      return Got;
   end Take;

   function Ready (Base : Unsigned_64) return Boolean is
     (Word (Base, SR.CONSUMER_OFFSET) /= Word (Base, SR.PRODUCED_OFFSET));

   function Finished (Base : Unsigned_64) return Boolean is
     (Word (Base, SR.ENDED_OFFSET) /= 0 and then not Ready (Base));

end CuBit.Inlet_Rings;
