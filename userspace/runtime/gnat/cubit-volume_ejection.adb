------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Volume_Ejection with SPARK_Mode is

   procedure Decode (Value : Unsigned_64; Item : out Answer; Valid : out Boolean) is
      Code : constant Unsigned_64 := Value and Low_Half;
      Count : constant Unsigned_64 := Shift_Right (Value, Count_Shift);
      Result : Outcome;
   begin
      Item := (others => <>);
      Valid := False;
      if Code = Code_Of (Safe_To_Remove) then
         Result := Safe_To_Remove;
      elsif Code = Code_Of (Open_Handles) then
         Result := Open_Handles;
      elsif Code = Code_Of (Not_Removable) then
         Result := Not_Removable;
      elsif Code = Code_Of (Not_Attached) then
         Result := Not_Attached;
      elsif Code = Code_Of (Flush_Failed) then
         Result := Flush_Failed;
      elsif Code = Code_Of (Access_Denied) then
         Result := Access_Denied;
      else
         return;
      end if;
      if (Result = Open_Handles) /= (Count /= 0) then
         return;
      end if;
      Item := (Result => Result, Open => Open_Count (Count));
      Valid := True;
   end Decode;

   procedure Decode_Policy
     (Code : Unsigned_32; Item : out Open_Handle_Policy; Valid : out Boolean) is
   begin
      Item := Refuse_If_Open;
      Valid := True;
      if Code = Policy_Code (Refuse_If_Open) then
         Item := Refuse_If_Open;
      elsif Code = Policy_Code (Close_Open_Handles) then
         Item := Close_Open_Handles;
      else
         Valid := False;
      end if;
   end Decode_Policy;

end CuBit.Volume_Ejection;
