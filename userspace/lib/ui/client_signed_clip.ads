with Interfaces.C;
-- Signed foreign coordinates are translated in widened arithmetic before
-- clipping. The exported function is pure; it reads no foreign memory.
package Client_Signed_Clip with SPARK_Mode, Pure is
   subtype Coordinate is Interfaces.C.int;
   use type Interfaces.C.int;
   function Edge (Value, Offset, Limit : Coordinate) return Coordinate
     with Export, Convention => C, External_Name => "cubit_ui_clip_edge",
       Global => null,
       Post => Edge'Result >= 0 and then
         Edge'Result <= Coordinate'Max (0, Limit) and then
         Long_Long_Integer (Edge'Result) =
           Long_Long_Integer'Max (0, Long_Long_Integer'Min
             (Long_Long_Integer (Limit),
              Long_Long_Integer (Value) - Long_Long_Integer (Offset)));
end Client_Signed_Clip;
