------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Capture_Policy with SPARK_Mode is

   procedure Step (S : in out State; E : Event; D : out Decision) is
   begin
      D := None;
      case E.Kind is
         when Announce =>
            if E.Holder = No_Session or else E.New_Pairing then
               S.Allowed := False;
            end if;
            S.Holder := E.Holder;
         when Set_Allowed =>
            S.Allowed := E.Allowed and then S.Holder /= No_Session;
         when Request =>
            if S.Holder = No_Session then
               D := Refuse_No_Session;
            elsif not S.Allowed then
               D := Refuse_Off;
            elsif not Rate_Allows (S, E.Now) then
               D := Refuse_Rate;
            else
               D := Capture;
               S.Captured := True;
               S.Last_Ms := E.Now;
               S.Marked := True;
               S.Mark_End := Mark_End_After (E.Now);
            end if;
         when Tick =>
            if S.Marked and then E.Now >= S.Mark_End then
               S.Marked := False;
            end if;
      end case;
   end Step;
end Desktop_Capture_Policy;
