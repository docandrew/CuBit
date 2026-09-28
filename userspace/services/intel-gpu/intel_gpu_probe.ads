with Interfaces;

--  Pure admission helpers, not a device driver or permission to touch MMIO.
package Intel_GPU_Probe with SPARK_Mode is
   use Interfaces;
   type Platform is (Unrecognized, Kaby_Lake_ULT_GT2, Alder_Lake_N);

   function Identify
     (Vendor, Device : Unsigned_16; Base_Class : Unsigned_8)
      return Platform
   with Global => null;

   type BAR_Status is
     (Memory_32, Memory_64, Unassigned, IO_Space,
      Unsupported_Encoding, Missing_Upper_Word);
   type BAR_Result is record
      Status       : BAR_Status := Unassigned;
      Base         : Unsigned_64 := 0;
      Prefetchable : Boolean := False;
      Words        : Positive range 1 .. 2 := 1;
   end record;

   --  Decode a memory BAR without destructive size probing. High is only
   --  meaningful when a 64-bit BAR has a following configuration DWORD.
   --  No size, mapping, authority, or device support can be inferred here.
   function Decode_BAR
     (Low, High : Unsigned_32; Has_Upper_Word : Boolean) return BAR_Result
   with Global => null,
     Post =>
       (if Decode_BAR'Result.Status in Memory_32 | Memory_64 then
           Decode_BAR'Result.Base /= 0
           and then Decode_BAR'Result.Base mod 16 = 0
        else Decode_BAR'Result.Base = 0);

   --  A caller supplies an already-authorized mapping, NOT an untrusted BAR.
   --  This checks address arithmetic only. Register semantics, power state,
   --  access authority and lifetime must be checked by the hardware adapter.
   function Contains_Register
     (Mapping_Base, Mapping_Bytes, Offset : Unsigned_64) return Boolean
   with Global => null;

   function Register_Address
     (Mapping_Base, Mapping_Bytes, Offset : Unsigned_64) return Unsigned_64
   with Global => null,
     Pre => Contains_Register (Mapping_Base, Mapping_Bytes, Offset),
     Post => Register_Address'Result >= Mapping_Base
       and then Register_Address'Result mod 4 = 0
       and then Register_Address'Result <= Unsigned_64'Last - 3
       and then Register_Address'Result - Mapping_Base <= Mapping_Bytes - 4;
end Intel_GPU_Probe;
