-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
pragma Ada_2022;
procedure Owned_Release_Walk (Base, Bytes : Unsigned_64; Touched, Done : out Boolean) is
   package Policy renames Owned_Reservation_Policy;
   Page : constant Unsigned_64 := Policy.Page_Bytes;
   Limit : constant Unsigned_64 := Base + Bytes;
   Cursor : Unsigned_64 := Base;
   Found : Boolean;
   Region_Base, Region_Bytes, Low, High : Unsigned_64;
   Cut : Policy.Run_Split;
begin
   Touched := False;
   Done := True;
   loop
      Next_Region (Cursor, Limit, Found, Region_Base, Region_Bytes);
      exit when not Found;
      -- A region Next_Region gave: page-aligned, a valid size, overlapping
      -- [Cursor, Limit). Anything else is a broken table: refused.
      if not Policy.Valid_Region (Region_Base, Region_Bytes)
        or else Region_Base >= Limit or else Region_Base + Region_Bytes <= Cursor
      then
         Done := False;
         return;
      end if;
      Touched := True;
      Low := Unsigned_64'Max (Cursor, Region_Base);
      High := Unsigned_64'Min (Limit, Region_Base + Region_Bytes);
      Cut := Policy.Split (Region_Base, Region_Bytes / Page, Low, High - Low);
      Apply (Region_Base, Region_Bytes, Cut, Done);
      exit when not Done;
      Cursor := Region_Base + (Cut.Lower + Cut.Dropped) * Page;
      exit when Cursor >= Limit;
   end loop;
end Owned_Release_Walk;
