pragma Ada_2022;
with Interfaces;
package ACPI_Query_Retry with Pure,SPARK_Mode is
   type Tick is new Interfaces.Unsigned_64;
   No_Deadline:constant Tick:=Tick'Last;
   subtype Interval is Tick range 1..Tick'Last-1;
   type State is private;
   function Healthy(Item:State)return Boolean with Global=>null;
   function Seen(Item:State)return Boolean with Global=>null;
   function Last_Observed(Item:State)return Tick with Global=>null;
   function Scheduled(Item:State)return Boolean with Global=>null;
   function Deadline(Item:State)return Tick with Global=>null;
   function Due(Item:State;Now:Tick)return Boolean with Global=>null,
     Post=>Due'Result=(Healthy(Item) and then Scheduled(Item) and then Now>=Deadline(Item));
   procedure Observe(Item:in out State;Now:Tick;Available:Boolean;Accepted:out Boolean)
     with Always_Terminates,Global=>null,
       Post=>Accepted=(Healthy(Item'Old) and then Available and then
         (not Seen(Item'Old) or else Now>=Last_Observed(Item'Old)))
         and then Healthy(Item)=Accepted
         and then (if Accepted then Seen(Item) and then Last_Observed(Item)=Now
                   else Seen(Item)=Seen(Item'Old) and then Last_Observed(Item)=Last_Observed(Item'Old));
   -- All ticks use the same owner-supplied millisecond epoch. Neither this
   -- policy nor a successful observation proves physical clock accuracy.
   procedure Schedule(Item:in out State;Delay_Ticks:Interval)
     with Always_Terminates,Global=>null,
       Post=>Seen(Item)=Seen(Item'Old) and then Last_Observed(Item)=Last_Observed(Item'Old)
         and then Healthy(Item)=(Healthy(Item'Old) and then Seen(Item'Old)
           and then Last_Observed(Item'Old)<=No_Deadline-Delay_Ticks-1)
         and then (if Healthy(Item) then Scheduled(Item)
           and then Deadline(Item)=Last_Observed(Item)+Delay_Ticks
           and then Deadline(Item)>Last_Observed(Item) and then Deadline(Item)<No_Deadline
           else Deadline(Item)=No_Deadline);
private
   type State is record
      Failed,Has_Clock,Has_Deadline:Boolean:=False;
      Last,Next:Tick:=0;
   end record;
   function Healthy(Item:State)return Boolean is(not Item.Failed);
   function Seen(Item:State)return Boolean is(Item.Has_Clock);
   function Last_Observed(Item:State)return Tick is(Item.Last);
   function Scheduled(Item:State)return Boolean is(Item.Has_Deadline and then not Item.Failed);
   function Deadline(Item:State)return Tick is(if Scheduled(Item) then Item.Next else No_Deadline);
end ACPI_Query_Retry;
