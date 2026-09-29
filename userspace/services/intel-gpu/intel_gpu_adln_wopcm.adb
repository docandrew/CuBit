with Intel_GPU_Firmware;
package body Intel_GPU_ADLN_WOPCM with SPARK_Mode is
   function Select_Layout
     (Size_First, Base_First, Size_Second, Base_Second : Unsigned_32;
      Upload_Bytes : Unsigned_64) return Layout
   is
      Capacity : constant Unsigned_64 := 2 * 1024 * 1024;
      Base, Bytes : Unsigned_64;
      Locked : Boolean;
   begin
      if Size_First = Unsigned_32'Last or else Base_First = Unsigned_32'Last or else
        Size_First /= Size_Second or else Base_First /= Base_Second or else
        Upload_Bytes = 0 or else (Base_First and 2) /= 0 or else
        (Size_First and 1) /= (Base_First and 1)
      then return (others => <>); end if;
      Locked := (Size_First and 1) /= 0;
      if Locked then
         Base := Unsigned_64 (Base_First and 16#FFFFC000#);
         Bytes := Unsigned_64 (Size_First and 16#FFFFF000#);
      else
         Base := 16 * 1024;
         Bytes := Capacity - 36 * 1024 - Base;
      end if;
      if not Intel_GPU_Firmware.Fits_ADLN_WOPCM
        (Capacity, Base, Bytes, Upload_Bytes, 0)
      then return (others => <>); end if;
      return (True, Locked, Capacity, Base, Bytes, Bytes);
   end Select_Layout;
end Intel_GPU_ADLN_WOPCM;
