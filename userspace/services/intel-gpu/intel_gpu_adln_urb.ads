with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_URB with SPARK_Mode is
   -- TGL Vol2a pp135-142; Vol2d pp125-129. One-slice fixed VS-only probe.
   type Stage_Control is record
      Entries : B16 := 0;
      Rows_Minus_One : B9 := 0;
      Start_8KiB : B7 := 4;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stage_Control use record
      Entries at 0 range 0 .. 15;
      Rows_Minus_One at 0 range 16 .. 24;
      Start_8KiB at 0 range 25 .. 31;
   end record;
   function Encode (V : Stage_Control) return Unsigned_32 is
     (Unsigned_32 (V.Entries) or Shift_Left (Unsigned_32 (V.Rows_Minus_One), 16) or
      Shift_Left (Unsigned_32 (V.Start_8KiB), 25));
   type Words is array (Natural range 0 .. 7) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      VS_Entries : Natural := 0;
      Data : Words := [others => 0];
   end record;
   -- Caller supplies CURRENT usable URB capacity from owned L3 configuration,
   -- after hardware reservations, not a PCI-default guess. POSH must be off;
   -- HS/DS/GS stages disabled separately. Reserve first32KiB for constants.
   function Build (Usable_KiB : Natural) return Image with
     Post => (if Build'Result.Valid then
       Usable_KiB in 40 .. 512 and then
       Build'Result.VS_Entries in 64 .. 3576 and then
       Build'Result.VS_Entries mod 8 = 0 and then
       32768 + Build'Result.VS_Entries * 64 <= Usable_KiB * 1024);
end Intel_GPU_ADLN_URB;
