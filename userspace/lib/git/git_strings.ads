------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Text of any length for the clone writer (paths, ref names, details):
--  on the heap (CuAlloc natively), copied when assigned and freed with its
--  holder, so it behaves as a value. CuBit's user runtime has no
--  Ada.Strings.Unbounded; this is the little of it git needs.
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

package Git_Strings is
   type Text is private;
   Null_Text : constant Text;

   function To_Text (S : String) return Text;
   function To_String (T : Text) return String;
   function Length (T : Text) return Natural;
   procedure Append (T : in out Text; S : String);
   procedure Append (T : in out Text; Ch : Character);
   function "=" (Left, Right : Text) return Boolean;
   function "<" (Left, Right : Text) return Boolean;
private
   type String_Access is access String;
   type Text is new Ada.Finalization.Controlled with record
      Data : String_Access;
      Used : Natural := 0;
   end record;
   overriding procedure Adjust (T : in out Text);
   overriding procedure Finalize (T : in out Text);
   Null_Text : constant Text := (Ada.Finalization.Controlled with Data => null, Used => 0);
end Git_Strings;
