pragma Ada_2022;
with System.Storage_Elements;
with System.Machine_Code;
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

   --  Entries waits took (Note_Taken), for every tracker; added atomically
   --  (any thread may wait).
   Taken_By_Waits : RP.Produced_Counts := [others => 0] with Volatile;

   function Taken (Class : Queue_Class) return Count is (Taken_By_Waits (Class));

   function Pending (T : Tracker; Class : Queue_Class) return Boolean is
     (not Available or else
      RP.Behind (Page.Produced (Class), T.Consumed (Class) + Taken (Class)));

   procedure Took (T : in out Tracker; Class : Queue_Class) is
   begin
      if Available then
         T.Consumed (Class) := RP.Next (T.Consumed (Class));
      end if;
   end Took;

   procedure Note_Taken (Class : Queue_Class) is
      One : Count := 1;
   begin
      System.Machine_Code.Asm
        ("lock xaddq %0, %1",
         Outputs => (Count'Asm_Output ("+r", One),
                     Count'Asm_Output ("+m", Taken_By_Waits (Class))),
         Clobber => "memory, cc", Volatile => True);
   end Note_Taken;

   procedure Caught_Up (T : in out Tracker; Class : Queue_Class; Seen : Count) is
      By_Waits : constant Count := Taken (Class);
   begin
      if Available and then Seen /= Count'Last and then Seen >= By_Waits
        and then Seen - By_Waits > T.Consumed (Class)
      then
         T.Consumed (Class) := Seen - By_Waits;
      end if;
   end Caught_Up;

   function Wake_Generation return Count is
     (if Available then Page.Wake_Generation else 0);

end CuBit.Readiness;
