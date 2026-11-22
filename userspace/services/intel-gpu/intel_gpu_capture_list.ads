with Interfaces; use Interfaces;
package Intel_GPU_Capture_List with SPARK_Mode is
   -- ADL-N capture ABI, distinct from ADS engine-save register sets.
   -- Linux v6.16 guc_capture_list_init and __fill_ext_reg. No MMIO or
   -- publication: callers must provide an owned, mapped page even for empty.
   subtype Steering_ID is Natural range 0 .. 15;
   type Descriptor is record
      Offset : Unsigned_32 := 0;
      Group_ID, Instance : Steering_ID := 0;
   end record;
   type Descriptors is array (Natural range <>) of Descriptor;
   Max_Descriptors : constant := 255;
   type Page is array (Natural range 0 .. 4095) of Unsigned_8;
   type Image is record
      Valid : Boolean := False;
      Bytes : Page := [others => 0];
   end record;
   -- Current platform lists use mask=0. Do not accept arbitrary firmware
   -- flag bits, nor implicitly add NEEDS_STEERING as save lists do.
   function Encode (Items : Descriptors) return Image;
end Intel_GPU_Capture_List;
