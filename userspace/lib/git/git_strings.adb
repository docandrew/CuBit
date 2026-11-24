------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Text of any length (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Git_Strings is
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   FIRST_ROOM : constant := 64;

   function To_Text (S : String) return Text is
      T : Text;
   begin
      Append (T, S);
      return T;
   end To_Text;

   function To_String (T : Text) return String is
     (if T.Data = null then "" else T.Data (1 .. T.Used));

   function Length (T : Text) return Natural is (T.Used);

   procedure Append (T : in out Text; S : String) is
   begin
      if S'Length = 0 then
         return;
      end if;
      if T.Data = null or else T.Data'Length - T.Used < S'Length then
         declare
            Bigger : constant String_Access :=
              new String (1 .. Natural'Max (FIRST_ROOM,
                                             Natural'Max (T.Used + S'Length,
                                                          2 * (if T.Data = null then 0 else T.Data'Length))));
         begin
            if T.Used > 0 then
               Bigger (1 .. T.Used) := T.Data (1 .. T.Used);
            end if;
            Free (T.Data);
            T.Data := Bigger;
         end;
      end if;
      T.Data (T.Used + 1 .. T.Used + S'Length) := S;
      T.Used := T.Used + S'Length;
   end Append;

   procedure Append (T : in out Text; Ch : Character) is
   begin
      Append (T, [1 => Ch]);
   end Append;

   function "=" (Left, Right : Text) return Boolean is (To_String (Left) = To_String (Right));
   function "<" (Left, Right : Text) return Boolean is (To_String (Left) < To_String (Right));

   overriding procedure Adjust (T : in out Text) is
   begin
      if T.Data /= null then
         T.Data := new String'(T.Data (1 .. T.Used));
      end if;
   end Adjust;

   overriding procedure Finalize (T : in out Text) is
   begin
      Free (T.Data);
      T.Used := 0;
   end Finalize;
end Git_Strings;
