------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Modifier_Reveal with SPARK_Mode is

   procedure Key_Event
     (S : in out State; Item : Keys.Key; Down : Boolean; Modifiers : Keys.Modifier_Set;
      Now_Ms : Interfaces.Unsigned_64)
   is
      Now_Held : Keys.Modifier_Set := Modifiers;
      Was_Revealed : constant Boolean := Is_Revealed (S, Now_Ms);
   begin
      if Keys.Is_Modifier_Key (Item) then
         Now_Held (Keys.Modifier_Of (Item)) := Down;
      elsif Down and then not Keys.Is_Empty (S.Held_Set) then
         --  A chord: nothing shows until every modifier is released.
         S.Used := True;
      end if;
      if Keys.Is_Empty (Now_Held) then
         S := (Held_Set => Keys.No_Modifiers, Used => False, Changed_At => Now_Ms);
      elsif Now_Held /= S.Held_Set then
         --  A revealed layer switches at once (Shift added to Ctrl); a new
         --  hold waits for the delay.
         if not Was_Revealed then
            S.Changed_At := Now_Ms;
         end if;
         S.Held_Set := Now_Held;
      end if;
   end Key_Event;

   procedure Note_Use (S : in out State) is
   begin
      if not Keys.Is_Empty (S.Held_Set) then
         S.Used := True;
      end if;
   end Note_Use;

   procedure Release_All (S : out State) is
   begin
      S := (Held_Set => Keys.No_Modifiers, Used => False, Changed_At => 0);
   end Release_All;
end CuBit.UI.Modifier_Reveal;
