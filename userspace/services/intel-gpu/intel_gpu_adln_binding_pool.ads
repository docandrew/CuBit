with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Binding_Pool with SPARK_Mode is
   -- Intel TGL Vol2a18-19. Disabling selects Surface State Base Address,
   -- even when resource streamer is already disabled.
   type B1 is mod 2 ** 1 with Size => 1;
   type B4 is mod 2 ** 4 with Size => 4;
   type B6 is mod 2 ** 6 with Size => 6;
   type B12 is mod 2 ** 12 with Size => 12;
   type B20 is mod 2 ** 20 with Size => 20;
   type B52 is mod 2 ** 52 with Size => 52;
   type Address_Control is record
      Reserved_0 : B1 := 0;
      MOCS_Index : B6 := 0;
      Reserved_7 : B4 := 0;
      Enable_Pool : B1 := 0;
      Address_Pages : B52 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Address_Control use record
      Reserved_0 at 0 range 0 .. 0;
      MOCS_Index at 0 range 1 .. 6;
      Reserved_7 at 0 range 7 .. 10;
      Enable_Pool at 0 range 11 .. 11;
      Address_Pages at 0 range 12 .. 63;
   end record;
   function Encode (V : Address_Control) return Unsigned_64 is
     (Unsigned_64 (V.Reserved_0) or
      Shift_Left (Unsigned_64 (V.MOCS_Index), 1) or
      Shift_Left (Unsigned_64 (V.Reserved_7), 7) or
      Shift_Left (Unsigned_64 (V.Enable_Pool), 11) or
      Shift_Left (Unsigned_64 (V.Address_Pages), 12));
   type Size_Control is record
      Reserved_0 : B12 := 0;
      Page_Count : B20 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Size_Control use record
      Reserved_0 at 0 range 0 .. 11;
      Page_Count at 0 range 12 .. 31;
   end record;
   function Encode (V : Size_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or Shift_Left (Unsigned_32 (V.Page_Count), 12));
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Caller supplies an already installed encoded MOCS policy.
   -- This validates encoding only, not ownership of the MOCS register bank.
   function Disable (MOCS : Unsigned_32) return Image
     with Post =>
       Disable'Result.Valid = (MOCS in 2 .. 126 and MOCS mod 2 = 0)
       and then (if not Disable'Result.Valid then
                   Disable'Result.Data = Words'(others => 0));
end Intel_GPU_ADLN_Binding_Pool;
