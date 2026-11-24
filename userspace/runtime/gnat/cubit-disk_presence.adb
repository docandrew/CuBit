------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Disk_Presence with SPARK_Mode is

   procedure Decode
     (Length : Natural; Item_Words : Words; Item : out Report; Valid : out Boolean)
   is
      State : constant Unsigned_64 := Item_Words (State_Word);
   begin
      Item := (others => <>);
      Valid := False;
      if Length /= Report_Words or else Item_Words (Unit_Word) not in 1 .. Unsigned_64 (Units.Maximum_Unit) then
         return;
      end if;
      if State = Unsigned_64 (Presence'Enum_Rep (Present)) then
         Item := (Unit => Units.Unit (Item_Words (Unit_Word) and Units.Low_Half), State => Present,
                  Write_Protected => False);
      elsif State = Unsigned_64 (Presence'Enum_Rep (Present)) + Write_Protected_Bit then
         Item := (Unit => Units.Unit (Item_Words (Unit_Word) and Units.Low_Half), State => Present,
                  Write_Protected => True);
      elsif State = Unsigned_64 (Presence'Enum_Rep (Gone)) then
         Item := (Unit => Units.Unit (Item_Words (Unit_Word) and Units.Low_Half), State => Gone,
                  Write_Protected => False);
      else
         return;
      end if;
      Valid := True;
   end Decode;

end CuBit.Disk_Presence;
