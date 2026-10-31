pragma Ada_2022;
package body GPIO_Loop_Time with SPARK_Mode is
   function After (Now, Window : Tick) return Finite_Time is
   begin
      if Now >= Finite_Time'Last or else Window > Finite_Time'Last - Now
      then return Finite_Time'Last; end if;
      return Now + Window;
   end After;
end GPIO_Loop_Time;
