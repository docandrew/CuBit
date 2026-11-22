with Compositor_Damage;
--  A drag's outline is not the current surface. Retiring either requires
--  preserving its own footprint, even when several pointer reports coalesce.
package Compositor_Transition with Pure, SPARK_Mode is
   package D renames Compositor_Damage;
   function Cover (Old_Surface, New_Surface, Presented : D.Box;
                   Has_Presented : Boolean) return D.Box is
     (if Has_Presented then
        D.Envelope (D.Envelope (Old_Surface, New_Surface), Presented)
      else D.Envelope (Old_Surface, New_Surface))
     with Post => D.Contains (Cover'Result, Old_Surface) and
       D.Contains (Cover'Result, New_Surface) and
       (if Has_Presented then D.Contains (Cover'Result, Presented)) and
       (if D.Valid (Old_Surface) or D.Valid (New_Surface)
        then D.Valid (Cover'Result));
end Compositor_Transition;
