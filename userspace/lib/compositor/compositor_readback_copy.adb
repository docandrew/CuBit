with Interfaces.C;
with System.Storage_Elements;
package body Compositor_Readback_Copy with SPARK_Mode => Off is
   package R renames Compositor_Row_Copy;
   use System.Storage_Elements;
   use type System.Address;
   function Memcpy (Target, Source : System.Address; Bytes : Interfaces.C.size_t)
     return System.Address with Import, Convention => C, External_Name => "memcpy";
   procedure Copy
     (Source, Target : System.Address;
      Width, Height, First_Row : R.G.Pixel_Edge;
      Source_Bytes, Target_Bytes, Target_Pitch, Byte_Budget : Natural;
      Copied_Rows : out Natural)
   is
      Plan : constant R.Readback_Batch := R.Readback_Plan
        (Width, Height, First_Row, Source_Bytes, Target_Bytes, Target_Pitch,
         Natural'Min (Byte_Budget, Maximum_Batch_Bytes));
      S : constant Integer_Address := To_Integer (Source);
      T : constant Integer_Address := To_Integer (Target);
      Ignored : System.Address;
   begin
      Copied_Rows := 0;
      if Plan.Rows = 0 or else Source = System.Null_Address or else
         Target = System.Null_Address or else
         S > Integer_Address'Last - Integer_Address (Source_Bytes) or else
         T > Integer_Address'Last - Integer_Address (Target_Bytes)
      then return; end if;
      if S < T + Integer_Address (Target_Bytes) and then
         T < S + Integer_Address (Source_Bytes) then return; end if;
      for Row in 0 .. Plan.Rows - 1 loop
         Ignored := Memcpy
           (Target + Storage_Offset (Plan.Target_Offset + Row * Target_Pitch),
            Source + Storage_Offset (Plan.Source_Offset + Row * Plan.Row_Bytes),
            Interfaces.C.size_t (Plan.Row_Bytes));
      end loop;
      Copied_Rows := Plan.Rows;
   end Copy;
end Compositor_Readback_Copy;
