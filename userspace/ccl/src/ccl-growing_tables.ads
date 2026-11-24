------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  A table of elements in process memory (the heap, CuAlloc natively) that
--  grows from its input: storage starts small and doubles as elements are
--  appended, up to the instance's named Ceiling. Nothing is sized for the
--  ceiling. Appending at the ceiling, or when memory runs out, is refused
--  (Done is False) with the table unchanged, so a caller reports a typed
--  capacity result rather than failing.
--
--  A table behaves as a value: assigning one shares its storage (a count of
--  holders, kept atomically, so tables may be copied on several threads).
--  Appending through a holder whose last element is the storage's last
--  (the usual case: a registry copied, then extended) writes in place;
--  other holders still see only their own elements. Any other change
--  through a holder that shares the storage takes a private copy first
--  (copy on write). A copy is O(1); nothing else can observe the sharing.
--  Equality is content equality, which the prover may use as logical
--  equality: every function here depends only on Length and Get.
--
--  The postconditions are for the prover. They are not checked at run time
--  even under -gnata (each would compare every element and hold a copy
--  across the call, which forces the copy on write it describes); the
--  hosted tests check the same properties directly.
--
--  The visible part is SPARK; the heap is behind the private part. The body
--  is not proved (the trusted part): it is small and tested by
--  tests/ccl-growing-tables.
------------------------------------------------------------------------------
with Ada.Finalization;
--  Loaded with the spec: GNAT 15 loads this runtime unit when it expands
--  a controlled object, and loading it in the middle of a client's body
--  (natively) drops the use-visibility that client's spec gave (its
--  operators on Interfaces.Unsigned_32 became invisible).
pragma Warnings (Off, "*internal GNAT unit*");
pragma Warnings (Off, "*is not referenced*");
with System.Finalization_Primitives;
pragma Warnings (On, "*is not referenced*");
pragma Warnings (On, "*internal GNAT unit*");
private with Interfaces;

generic
   type Element is private;
   Ceiling : Positive;
package CCL.Growing_Tables with SPARK_Mode is
   pragma Assertion_Policy (Post => Ignore);
   subtype Count is Natural range 0 .. Ceiling;
   subtype Index is Positive range 1 .. Ceiling;

   --  Empty until something is appended.
   type Table is private with Default_Initial_Condition => Length (Table) = 0;

   function Length (Item : Table) return Count
     with Global => null;
   function Get (Item : Table; Position : Index) return Element
     with Global => null, Pre => Position <= Length (Item);

   --  The first element whose key (a part of it, such as a name) is
   --  Wanted, 0 when none is; keys are read in place, no element is copied.
   generic
      type Key is private;
      with function Key_Of (Item : Element) return Key;
      with function Same_Key (Left, Right : Key) return Boolean;
   function Search (Item : Table; Wanted : Key) return Count
     with Global => null,
       Post => Search'Result <= Length (Item) and then
               (for all P in 1 .. Search'Result - 1 =>
                  not Same_Key (Key_Of (Get (Item, P)), Wanted)) and then
               (if Search'Result = 0 then
                  (for all P in 1 .. Length (Item) => not Same_Key (Key_Of (Get (Item, P)), Wanted))
                else Same_Key (Key_Of (Get (Item, Search'Result)), Wanted));

   --  The same elements in the same order (not the same storage).
   function "=" (Left, Right : Table) return Boolean
     with Global => null;
   pragma Annotate (GNATprove, Logical_Equal, "=");

   --  Element after the last one. Refused at Ceiling, or when the storage
   --  cannot grow.
   procedure Append (Item : in out Table; Value : Element; Done : out Boolean)
     with Global => null, Always_Terminates,
       Post =>
         (if Done then
            Length (Item'Old) < Ceiling and then
            Length (Item) = Length (Item'Old) + 1 and then
            Get (Item, Length (Item)) = Value and then
            (for all P in 1 .. Length (Item'Old) => Get (Item, P) = Get (Item'Old, P))
          else Item = Item'Old);

   --  Element Position replaced by Value. Refused only when the storage is
   --  shared and a private copy cannot be made.
   procedure Replace
     (Item : in out Table; Position : Index; Value : Element; Done : out Boolean)
     with Global => null, Always_Terminates,
       Pre => Position <= Length (Item),
       Post =>
         (if Done then
            Length (Item) = Length (Item'Old) and then
            Get (Item, Position) = Value and then
            (for all P in 1 .. Length (Item) =>
               (if P /= Position then Get (Item, P) = Get (Item'Old, P)))
          else Item = Item'Old);
private
   pragma SPARK_Mode (Off);
   type Element_Array is array (Index range <>) of Element;
   type Block (Capacity : Index) is limited record
      --  Tables holding this block.
      Holders : aliased Interfaces.Unsigned_32 := 1;
      --  Items 1 .. Written are in use by some holder; a holder using all
      --  of them may append in place (claiming the next by compare and swap).
      Written : aliased Interfaces.Unsigned_32 := 0;
      Items : Element_Array (1 .. Capacity);
   end record;
   type Block_Access is access Block;
   type Table is new Ada.Finalization.Controlled with record
      Data : Block_Access := null;
      Used : Count := 0;
   end record;
   overriding procedure Adjust (Item : in out Table);
   overriding procedure Finalize (Item : in out Table);
end CCL.Growing_Tables;
