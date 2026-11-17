pragma Ada_2022;
package body ACPI_Query_Retry with SPARK_Mode is
   function Due(Item:State;Now:Tick)return Boolean is
     (Healthy(Item) and then Scheduled(Item) and then Now>=Deadline(Item));
   procedure Observe(Item:in out State;Now:Tick;Available:Boolean;Accepted:out Boolean) is
   begin
      Accepted:=Healthy(Item) and then Available and then (not Item.Has_Clock or else Now>=Item.Last);
      if Accepted then Item.Has_Clock:=True;Item.Last:=Now;
      else Item.Failed:=True;end if;
   end Observe;
   procedure Schedule(Item:in out State;Delay_Ticks:Interval) is
   begin
      if Item.Failed or else not Item.Has_Clock or else Item.Last>No_Deadline-Delay_Ticks-1 then
         Item.Failed:=True;return;
      end if;
      Item.Next:=Item.Last+Delay_Ticks;Item.Has_Deadline:=True;
   end Schedule;
end ACPI_Query_Retry;
