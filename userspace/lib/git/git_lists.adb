------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A list that grows on the heap (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Git_Lists is
   procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Array_Access);

   FIRST_ROOM : constant := 16;

   function Length (L : List) return Natural is (L.Used);
   function Get (L : List; Position : Positive) return Element is (L.Items (Position));
   function Last (L : List) return Element is (L.Items (L.Used));

   procedure Append (L : in out List; Value : Element) is
   begin
      if L.Items = null or else L.Used = L.Items'Length then
         declare
            Bigger : constant Array_Access :=
              new Element_Array (1 .. (if L.Items = null then FIRST_ROOM else Natural'Max (FIRST_ROOM, 2 * L.Items'Length)));
         begin
            if L.Used > 0 then
               Bigger (1 .. L.Used) := L.Items (1 .. L.Used);
            end if;
            Free (L.Items);
            L.Items := Bigger;
         end;
      end if;
      L.Used := L.Used + 1;
      L.Items (L.Used) := Value;
   end Append;

   procedure Replace (L : in out List; Position : Positive; Value : Element) is
   begin
      L.Items (Position) := Value;
   end Replace;

   procedure Delete_Last (L : in out List) is
   begin
      L.Used := L.Used - 1;
   end Delete_Last;

   procedure Clear (L : in out List) is
   begin
      Free (L.Items);
      L.Used := 0;
   end Clear;

   overriding procedure Adjust (L : in out List) is
   begin
      if L.Items /= null then
         L.Items := new Element_Array'(L.Items (1 .. L.Used));
      end if;
   end Adjust;

   overriding procedure Finalize (L : in out List) is
   begin
      Clear (L);
   end Finalize;
end Git_Lists;
