pragma Ada_2022;
package body GPIO_Startup_Readiness with SPARK_Mode is
   procedure Decide (S : in out State; Item : Gates; Selected : out Choice) is
   begin
      Selected := No_Report; if S.Sent then return; end if;
      S.Sent := True;
      Selected := (if Ready (Item) then Report_Ready else Report_Not_Present);
   end Decide;
end GPIO_Startup_Readiness;
