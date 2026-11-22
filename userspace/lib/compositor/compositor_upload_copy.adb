with Interfaces.C;
with System.Storage_Elements;
package body Compositor_Upload_Copy with SPARK_Mode => Off is
   package U renames Compositor_Upload;
   use System.Storage_Elements;
   use type System.Address, U.Pixel_Format;
   function Memcpy (Target, Source : System.Address; Bytes : Interfaces.C.size_t)
     return System.Address with Import, Convention => C, External_Name => "memcpy";
   procedure Copy (Source, Target : System.Address;
      Source_Bytes, Source_Pitch : Natural; Width, Height : U.Edge;
      Kind : U.Pixel_Format; Plan : U.Plan; Complete : out Boolean)
   is
      S : constant Integer_Address := To_Integer (Source);
      T : constant Integer_Address := To_Integer (Target);
      Pixel_Bytes : constant Positive := U.Pixel_Bytes (Kind);
      Area : constant U.Rectangle := U.Area (Plan);
      Row_Bytes : constant Natural := Area.Width * Pixel_Bytes;
      Pitch : constant Natural := (if U.Row_Length (Plan) = 0 then Area.Width
                                  else U.Row_Length (Plan)) * Pixel_Bytes;
      Ignored : System.Address;
   begin
      Complete := False;
      if not U.Valid (Plan) or else Width /= U.Image_Width (Plan) or else
         Height /= U.Image_Height (Plan) or else Kind /= U.Format (Plan) or else
         Source_Pitch < Width * Pixel_Bytes or else
         Long_Long_Integer (Source_Pitch) * Long_Long_Integer (Height) > Long_Long_Integer (Source_Bytes) or else
         Source = System.Null_Address or else Target = System.Null_Address or else
         S > Integer_Address'Last - Integer_Address (Source_Bytes) or else
         T > Integer_Address'Last - Integer_Address (U.Capacity (Plan))
      then return; end if;
      if S < T + Integer_Address (U.Capacity (Plan)) and then
         T < S + Integer_Address (Source_Bytes) then return; end if;
      for Row in 0 .. Area.Height - 1 loop
         Ignored := Memcpy
           (Target + Storage_Offset (U.Buffer_Offset (Plan) + Row * Pitch),
            Source + Storage_Offset ((Area.Y + Row) * Source_Pitch + Area.X * Pixel_Bytes),
            Interfaces.C.size_t (Row_Bytes));
      end loop;
      Complete := True;
   end Copy;
end Compositor_Upload_Copy;
