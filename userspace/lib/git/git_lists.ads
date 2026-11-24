------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A list that grows on the heap (CuAlloc natively) as elements are
--  appended, freed with its holder, for the clone writer: CuBit's user
--  runtime has no Ada.Containers; this is the little of Vectors git needs.
--  Indexed from 1. Assigning a list copies it.
------------------------------------------------------------------------------
pragma Ada_2022;
private with Ada.Finalization;
--  Loaded with the spec: GNAT 15 loads this runtime unit when it expands
--  a controlled object, and loading it in the middle of a client's body
--  (natively) drops the use-visibility that client's spec gave (its
--  operators on Interfaces.Unsigned_32 became invisible).
pragma Warnings (Off, "*internal GNAT unit*");
pragma Warnings (Off, "*is not referenced*");
with System.Finalization_Primitives;
pragma Warnings (On, "*is not referenced*");
pragma Warnings (On, "*internal GNAT unit*");

generic
   type Element is private;
package Git_Lists is
   type List is tagged private
     with Constant_Indexing => Get;

   function Length (L : List) return Natural;
   function Is_Empty (L : List) return Boolean is (Length (L) = 0);
   function Get (L : List; Position : Positive) return Element
   with Pre => Position <= Length (L);
   function Last (L : List) return Element
   with Pre => Length (L) > 0;
   procedure Append (L : in out List; Value : Element);
   procedure Replace (L : in out List; Position : Positive; Value : Element)
   with Pre => Position <= Length (L);
   procedure Delete_Last (L : in out List)
   with Pre => Length (L) > 0;
   procedure Clear (L : in out List);
private
   type Element_Array is array (Positive range <>) of Element;
   type Array_Access is access Element_Array;
   type List is new Ada.Finalization.Controlled with record
      Items : Array_Access;
      Used : Natural := 0;
   end record;
   overriding procedure Adjust (L : in out List);
   overriding procedure Finalize (L : in out List);
end Git_Lists;
