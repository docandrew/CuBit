pragma Ada_2022;

package body Debug_Admission with SPARK_Mode is
   procedure Prove_In_Slice (S : Session_Index; View, Pages, Byte : Unsigned_64) is
      pragma Unreferenced (View, Pages, Byte);
   begin
      null;
   end Prove_In_Slice;
end Debug_Admission;
