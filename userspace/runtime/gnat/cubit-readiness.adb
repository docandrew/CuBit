pragma Ada_2022;
with System.Storage_Elements;
with Interfaces;

package body CuBit.Readiness is
   use type Interfaces.Unsigned_64;
   use type RP.Count;

   Page : RP.Readiness_Page
     with Import, Volatile, Async_Writers,
          Address => System.Storage_Elements.To_Address (RP.Page_Address);

   function Available return Boolean is (Page.Version = RP.Layout_Version);

   function Produced (Class : Queue_Class) return Count is
      Value : constant Count := Page.Produced (Class);
   begin
      return (if Available then Value else Count'Last);
   end Produced;

   function Pending (T : Tracker; Class : Queue_Class) return Boolean is
     (not Available or else RP.Behind (Page.Produced (Class), T.Consumed (Class)));

   procedure Took (T : in out Tracker; Class : Counted_Class) is
   begin
      if Available then
         T.Consumed (Class) := RP.Next (T.Consumed (Class));
      end if;
   end Took;

   procedure Caught_Up (T : in out Tracker; Class : Queue_Class; Seen : Count) is
   begin
      if Available and then Seen > T.Consumed (Class) and then Seen /= Count'Last then
         T.Consumed (Class) := Seen;
      end if;
   end Caught_Up;

   function Wake_Generation return Count is
     (if Available then Page.Wake_Generation else 0);

end CuBit.Readiness;
