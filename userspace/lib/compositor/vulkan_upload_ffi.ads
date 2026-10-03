with System; with Interfaces;
-- Foreign device/dispatch validity, mapping and completion are trusted. Private
-- request memory is exclusive and outlives the owner; no external pointer import.
package Vulkan_Upload_FFI with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   procedure Prepare (Request : System.Address; Capacity : U32;
      Bytes : out U64; Types : out U32; Result : out U32) with Global => null;
   procedure Bind (Request : System.Address; Bytes : U64; Memory_Type : U32;
      Mapping : out System.Address; Result : out U32) with Global => null;
   procedure Release (Request : System.Address; Result : out U32) with Global => null;
end Vulkan_Upload_FFI;
