------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Key_Repeat with SPARK_Mode is

   procedure Press
     (S : in out State; K : Key; Now : Time; Config : Settings; Where : Route;
      Outcome : out Press_Outcome)
   is
   begin
      Outcome := New_Press;
      if Class_Of (K) = Ignored then
         return;
      elsif S.Held (K.Code, K.Extended) then
         Outcome := Held_Repeat;
         return;
      end if;
      S.Held (K.Code, K.Extended) := True;
      if Class_Of (K) = Repeating and then Config.Enabled and then Repeatable (Where.Where) then
         S := (Held     => S.Held,
               Active   => True,
               Current  => K,
               Target   => Where,
               Rate     => Config.Rate,
               Due      => Now + Config.Initial_Delay,
               Fraction => 0);
      else
         S := (S with delta Active => False, Due => NEVER);
      end if;
   end Press;

   procedure Release (S : in out State; K : Key) is
   begin
      if Class_Of (K) = Ignored then
         return;
      end if;
      S.Held (K.Code, K.Extended) := False;
      if S.Active and then (S.Current = K or else Class_Of (K) = Modifier) then
         S := (S with delta Active => False, Due => NEVER);
      end if;
   end Release;

   procedure Cancel (S : in out State) is
   begin
      S := (S with delta Active => False, Due => NEVER);
   end Cancel;

   procedure Reset (S : out State) is
   begin
      S := (Held => [others => [others => False]], Active => False, Current => (others => <>),
            Target => (others => <>), Rate => DI.DEFAULT_RATE_HZ, Due => NEVER, Fraction => 0);
   end Reset;

   procedure Tick
     (S : in out State; Now : Time; Where : Route; Fire : out Boolean; K : out Key)
   is
      Next : Milliseconds;
      Next_Fraction : Carry;
   begin
      Fire := False;
      K := S.Current;
      if not S.Active or else Now < S.Due then
         return;
      elsif Where /= S.Target then
         S := (S with delta Active => False, Due => NEVER);
         return;
      end if;
      pragma Assert (Period (S.Rate, S.Fraction) >= MILLISECONDS_PER_SECOND / DI.MAXIMUM_RATE_HZ);
      pragma Assert (Period (S.Rate, S.Fraction) <= MILLISECONDS_PER_SECOND / DI.MINIMUM_RATE_HZ + 1);
      Next := S.Due + Period (S.Rate, S.Fraction);
      Next_Fraction := Next_Carry (S.Rate, S.Fraction);
      if Next <= Now then
         --  Late (a stall): one repeat now, the next a full period later.
         Next := Now + MILLISECONDS_PER_SECOND / S.Rate;
         Next_Fraction := 0;
      end if;
      S := (S with delta Due => Next, Fraction => Next_Fraction);
      Fire := True;
   end Tick;
end Key_Repeat;
