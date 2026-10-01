with Interfaces;
package Compositor_Elapsed with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype Tick is Interfaces.Unsigned_64;
   Unavailable : constant Tick := Tick'Last;
   type Sample (Valid : Boolean := False) is record
      case Valid is
         when True => Microseconds : Tick;
         when False => null;
      end case;
   end record;
   function Measure (First, Last : Tick) return Sample is
     (if First /= Unavailable and then Last /= Unavailable and then Last >= First
      then (True, Last - First) else (Valid => False))
     with Post => Measure'Result.Valid =
       (First /= Unavailable and Last /= Unavailable and Last >= First) and then
       (if Measure'Result.Valid then Measure'Result.Microseconds = Last - First);
end Compositor_Elapsed;
