------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Object names, commits and trees (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package body Git_Objects with SPARK_Mode is
   use type Interfaces.Unsigned_8;

   NUL : constant Byte := 0;
   SPACE : constant Byte := Character'Pos (' ');
   LF : constant Byte := 10;
   SLASH : constant Byte := Character'Pos ('/');
   MAXIMUM_MODE_DIGITS : constant := 6;

   function Decimal (N : Byte_Count) return String is
      Text : String (1 .. 19) := [others => '0'];
      V : Byte_Count := N;
      First : Positive := 19;
   begin
      loop
         pragma Loop_Invariant (First in 1 .. 19);
         pragma Loop_Variant (Decreases => First);
         Text (First) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
         V := V / 10;
         exit when V = 0 or else First = 1;
         First := First - 1;
      end loop;
      return Text (First .. 19);
   end Decimal;

   procedure Start (H : out Namer; Format : Object_Format; Kind : Whole_Kind; Size : Byte_Count) is
      Head : constant String := Type_Name (Kind) & " " & Decimal (Size);
      Bytes : Byte_Array (1 .. Head'Length + 1) := [others => 0];
   begin
      for K in Head'Range loop
         Bytes (K - Head'First + 1) := Character'Pos (Head (K));
      end loop;
      --  The header ends with NUL (the last byte is 0).
      Git_Hashes.Start (H.Hash, Format);
      Git_Hashes.Update (H.Hash, Bytes);
   end Start;

   procedure Update (H : in out Namer; Data : Byte_Array) is
   begin
      Git_Hashes.Update (H.Hash, Data);
   end Update;

   procedure Finish (H : in out Namer; Id : out Object_Id) is
   begin
      Git_Hashes.Finish (H.Hash, Id);
   end Finish;

   procedure Commit_Tree
     (Commit : Byte_Array; Format : Object_Format; Tree : out Object_Id; Found : out Boolean)
   is
      PREFIX : constant String := "tree ";
      Line_Bytes : constant Positive := PREFIX'Length + Hex_Digits (Format) + 1;
   begin
      Tree := NO_ID;
      Found := False;
      if Commit'Length < Line_Bytes then
         return;
      end if;
      declare
         Hex : String (1 .. Hex_Digits (Format));
      begin
         for K in 0 .. PREFIX'Length - 1 loop
            if Commit (Commit'First + K) /= Character'Pos (PREFIX (PREFIX'First + K)) then
               return;
            end if;
         end loop;
         for K in Hex'Range loop
            Hex (K) := Character'Val (Commit (Commit'First + PREFIX'Length + K - 1));
         end loop;
         if not Is_Hex_Id (Hex, Format) or else Commit (Commit'First + Line_Bytes - 1) /= LF then
            return;
         end if;
         Tree := To_Id (Hex, Format);
         Found := True;
      end;
   end Commit_Tree;

   function Name_Text (Tree : Byte_Array; E : Tree_Entry) return String is
      Text : String (1 .. E.Name_Last - E.Name_First + 1);
   begin
      for K in Text'Range loop
         Text (K) := Character'Val (Tree (E.Name_First + (K - 1)));
      end loop;
      return Text;
   end Name_Text;

   --  git's tree order (base_name_compare): bytes, a directory's name
   --  taken as ending in '/', any other as ending in NUL.
   function Before (Tree : Byte_Array; A, B : Tree_Entry) return Boolean
   with Pre => A.Name_First <= A.Name_Last and then A.Name_First in Tree'Range and then A.Name_Last in Tree'Range
                 and then B.Name_First <= B.Name_Last and then B.Name_First in Tree'Range
                 and then B.Name_Last in Tree'Range
   is
      A_Length : constant Positive := A.Name_Last - A.Name_First + 1;
      B_Length : constant Positive := B.Name_Last - B.Name_First + 1;
      Common : constant Positive := Positive'Min (A_Length, B_Length);
      CA, CB : Byte;
   begin
      for K in 0 .. Common - 1 loop
         CA := Tree (A.Name_First + K);
         CB := Tree (B.Name_First + K);
         if CA /= CB then
            return CA < CB;
         end if;
      end loop;
      CA := (if A_Length > Common then Tree (A.Name_First + Common) elsif A.Mode = Directory then SLASH else NUL);
      CB := (if B_Length > Common then Tree (B.Name_First + Common) elsif B.Mode = Directory then SLASH else NUL);
      return CA < CB;
   end Before;

   procedure Next_Entry
     (Tree : Byte_Array;
      Format : Object_Format;
      Pos : in out Positive;
      Has_Previous : Boolean;
      Previous : Tree_Entry;
      E : out Tree_Entry;
      Why : out Tree_Refusal)
   is
      Mode_First : constant Positive := Pos;
      I : Positive := Pos;
      function Mode_Is (S : String) return Boolean is
        (I - Mode_First = S'Length
         and then (for all K in 0 .. S'Length - 1 => Tree (Mode_First + K) = Character'Pos (S (S'First + K))))
      with Pre => I in Mode_First .. Tree'Last and then Mode_First in Tree'Range;
   begin
      E := (others => <>);
      Why := No_Refusal;
      --  "<octal mode> "
      while I <= Tree'Last and then Tree (I) /= SPACE and then I - Mode_First < MAXIMUM_MODE_DIGITS loop
         pragma Loop_Invariant (I in Mode_First .. Tree'Last);
         I := I + 1;
      end loop;
      if I > Tree'Last or else Tree (I) /= SPACE then
         Why := (if I > Tree'Last then Truncated_Entry else Bad_Mode);
         return;
      end if;
      if Mode_Is ("100644") then
         E.Mode := Regular_File;
      elsif Mode_Is ("100755") then
         E.Mode := Executable_File;
      elsif Mode_Is ("120000") then
         E.Mode := Symbolic_Link;
      elsif Mode_Is ("40000") then
         E.Mode := Directory;
      elsif Mode_Is ("160000") then
         E.Mode := Gitlink;
      else
         Why := Bad_Mode;
         return;
      end if;
      --  "<name>" NUL
      if I = Tree'Last then
         Why := Truncated_Entry;
         return;
      end if;
      E.Name_First := I + 1;
      I := I + 1;
      while I <= Tree'Last and then Tree (I) /= NUL loop
         pragma Loop_Invariant (I in E.Name_First .. Tree'Last);
         I := I + 1;
      end loop;
      if I > Tree'Last or else Tree'Last - I < Id_Bytes (Format) then
         Why := Truncated_Entry;
         return;
      elsif I = E.Name_First or else I - E.Name_First > MAXIMUM_NAME_BYTES then
         Why := Unsafe_Name;
         return;
      end if;
      E.Name_Last := I - 1;
      for K in 1 .. Id_Bytes (Format) loop
         E.Id (K) := Tree (I + K);
      end loop;
      if not Safe_Name (Name_Text (Tree, E)) then
         Why := Unsafe_Name;
         return;
      end if;
      if Has_Previous and then not Before (Tree, Previous, E) then
         Why := Out_Of_Order;
         return;
      end if;
      Pos := I + Id_Bytes (Format) + 1;
   end Next_Entry;
end Git_Objects;
