------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Debug.Rust_Names with SPARK_Mode is

   --  Generic arguments that are the defaults nobody writes.
   Global_Allocator : constant String := ", alloc::alloc::Global";
   Random_State : constant String := ", std::hash::random::RandomState";

   function Path_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ':');

   procedure Simplify (Raw : String; Text : out String; Length : out Natural) is
      I : Integer := Raw'First;
      Run_Start : Positive := 1;   --  where the current path run begins in Text
   begin
      Text := [others => ' '];
      Length := 0;
      while I <= Raw'Last loop
         pragma Loop_Invariant (Length <= Name_Ceiling and then Run_Start <= Length + 1
                                and then I >= Raw'First and then I <= Raw'Last);
         pragma Loop_Variant (Increases => I);
         if Raw'Last - I + 1 >= Global_Allocator'Length
           and then Raw (I .. I + Global_Allocator'Length - 1) = Global_Allocator
         then
            I := I + Global_Allocator'Length;
         elsif Raw'Last - I + 1 >= Random_State'Length
           and then Raw (I .. I + Random_State'Length - 1) = Random_State
         then
            I := I + Random_State'Length;
         elsif I < Raw'Last and then Raw (I) = ':' and then Raw (I + 1) = ':' then
            --  A qualifier ends: drop it.
            Length := Run_Start - 1;
            I := I + 2;
         else
            if not Path_Character (Raw (I)) then
               Run_Start := Length + 2;
            end if;
            if Length < Name_Ceiling then
               Length := Length + 1;
               Text (Length) := Raw (I);
            else
               exit;
            end if;
            if not Path_Character (Raw (I)) then
               Run_Start := Length + 1;
            end if;
            I := I + 1;
         end if;
      end loop;
   end Simplify;

   function Simplified (Raw : String) return String is
      Text : String (1 .. Name_Ceiling) := [others => ' '];
      Length : Natural;
   begin
      Simplify (Raw, Text, Length);
      return Text (1 .. Length);
   end Simplified;

end CuBit.Debug.Rust_Names;
