with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_State_Base with SPARK_Mode is
   -- TGL PRM Vol2a pp1256-1265, STATE_BASE_ADDRESS. Fixed private probe.
   -- Encoding only: caller must flush/invalidate and reissue state pointers
   -- in the documented order, with the resource streamer disabled.
   type B11 is mod 2 ** 11 with Size => 11;
   type B20 is mod 2 ** 20 with Size => 20;
   type B52 is mod 2 ** 52 with Size => 52;
   type Base_Control is record
      Modify_Enable : B1 := 1;
      Reserved_Low : B3 := 0;
      MOCS : B7 := 0;
      Reserved_11 : B1 := 0;
      Address_Pages : B52 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Base_Control use record
      Modify_Enable at 0 range 0 .. 0;
      Reserved_Low at 0 range 1 .. 3;
      MOCS at 0 range 4 .. 10;
      Reserved_11 at 0 range 11 .. 11;
      Address_Pages at 0 range 12 .. 63;
   end record;
   type Stateless_Control is record
      Reserved_Low : B16 := 0;
      MOCS : B7 := 0;
      Reserved_High : B9 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stateless_Control use record
      Reserved_Low at 0 range 0 .. 15;
      MOCS at 0 range 16 .. 22;
      Reserved_High at 0 range 23 .. 31;
   end record;
   type Page_Bound is record
      Modify_Enable : B1 := 1;
      Reserved : B11 := 0;
      Pages : B20 := 0; -- Count of 4KiB pages, NOT size-minus-one.
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Page_Bound use record
      Modify_Enable at 0 range 0 .. 0;
      Reserved at 0 range 1 .. 11;
      Pages at 0 range 12 .. 31;
   end record;
   type Bindless_Surface_Bound is record
      Reserved : B12 := 0;
      Entries_Minus_One : B20 := 0; -- 64-byte entries; zero means ONE.
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Bindless_Surface_Bound use record
      Reserved at 0 range 0 .. 11;
      Entries_Minus_One at 0 range 12 .. 31;
   end record;
   type Bindless_Sampler_Bound is record
      Reserved : B12 := 0;
      Pages : B20 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Bindless_Sampler_Bound use record
      Reserved at 0 range 0 .. 11;
      Pages at 0 range 12 .. 31;
   end record;
   function Encode (V : Base_Control) return Unsigned_64 is
     (Unsigned_64 (V.Modify_Enable) or Shift_Left (Unsigned_64 (V.Reserved_Low), 1) or
      Shift_Left (Unsigned_64 (V.MOCS), 4) or Shift_Left (Unsigned_64 (V.Reserved_11), 11) or
      Shift_Left (Unsigned_64 (V.Address_Pages), 12));
   function Encode (V : Stateless_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_Low) or Shift_Left (Unsigned_32 (V.MOCS), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 23));
   function Encode (V : Page_Bound) return Unsigned_32 is
     (Unsigned_32 (V.Modify_Enable) or Shift_Left (Unsigned_32 (V.Reserved), 1) or
      Shift_Left (Unsigned_32 (V.Pages), 12));
   function Encode (V : Bindless_Surface_Bound) return Unsigned_32 is
     (Unsigned_32 (V.Reserved) or Shift_Left (Unsigned_32 (V.Entries_Minus_One), 12));
   function Encode (V : Bindless_Sampler_Bound) return Unsigned_32 is
     (Unsigned_32 (V.Reserved) or Shift_Left (Unsigned_32 (V.Pages), 12));
   type Words is array (Natural range 0 .. 21) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- MOCS must also be installed/admitted by the hardware policy owner.
   function Build (MOCS : Unsigned_32) return Image;
end Intel_GPU_ADLN_State_Base;
