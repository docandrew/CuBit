------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Log;

package body CuBit.Refusal_Notes is

   subtype Key_Length is Natural range 0 .. Key_Bytes;
   type Note is record
      Length : Key_Length := 0;
      Text   : String (1 .. Key_Bytes) := [others => ' '];
   end record;
   type Note_Array is array (1 .. Capacity) of Note;

   Notes : Note_Array;
   Used  : Natural range 0 .. Capacity := 0;
   Full_Said : Boolean := False;
   Repeats : Unsigned_64 := 0;

   function Repeated return Unsigned_64 is (Repeats);

   procedure Count;
   procedure Count is
   begin
      if Repeats < Unsigned_64'Last then
         Repeats := Repeats + 1;
      end if;
   end Count;

   procedure Warn_Once (Service : String; Key : String; What : String) is
      Length : constant Key_Length := Natural'Min (Key'Length, Key_Bytes);
      Compared : constant String (1 .. Length) := Key (Key'First .. Key'First + Length - 1);
   begin
      for I in 1 .. Used loop
         if Notes (I).Length = Length and then Notes (I).Text (1 .. Length) = Compared then
            Count;
            return;
         end if;
      end loop;
      if Used = Capacity then
         Count;
         if not Full_Said then
            Full_Said := True;
            CuBit.Log.Warning (Service & ": Warning: refused more than" & Natural'Image (Capacity) &
                               " distinct requests; further refusals are counted only (CAP-003)");
         end if;
         return;
      end if;
      Used := Used + 1;
      Notes (Used).Length := Length;
      Notes (Used).Text (1 .. Length) := Compared;
      CuBit.Log.Warning (Service & ": Warning: refused " & What & " (CAP-003)");
   end Warn_Once;

end CuBit.Refusal_Notes;
