-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
pragma Ada_2022;
package body Image_Share_Policy with SPARK_Mode => On is

   procedure Total_Pages (K : Key; Pages : out Page_Count; Fits : out Boolean) is
      Sum : Unsigned_64 := 0;
      One : Unsigned_64;
   begin
      Pages := 0;
      Fits := True;
      for I in 1 .. K.Count loop
         pragma Loop_Invariant (Sum <= Page_Count'Last);
         pragma Loop_Invariant (Fits);
         pragma Loop_Invariant (if I > 1 then Sum > 0);
         One := Pages_Of (K.Segments (I));
         if One > Page_Count'Last - Sum then
            Fits := False;
            return;
         end if;
         Sum := Sum + One;
         pragma Assert (One > 0);
      end loop;
      Pages := Sum;
   end Total_Pages;

   procedure Join (Users : in out User_Count; Joined : out Boolean) is
   begin
      Joined := Users < User_Count'Last;
      if Joined then
         Users := Users + 1;
      end if;
   end Join;

   procedure Leave (Users : in out User_Count; Last : out Boolean) is
   begin
      Users := Users - 1;
      Last := Users = 0;
   end Leave;

end Image_Share_Policy;
