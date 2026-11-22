pragma Ada_2022;
package body User_Access_Ranges with SPARK_Mode is
   procedure Prove_Outside_Aperture
     (Address, Bytes, Aperture_First, Aperture_End, Byte : Unsigned_64) is
      pragma Unreferenced (Address, Bytes, Aperture_First, Aperture_End, Byte);
   begin
      null;
   end Prove_Outside_Aperture;
end User_Access_Ranges;
