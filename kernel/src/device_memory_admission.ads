with Interfaces; use Interfaces;
package Device_Memory_Admission with SPARK_Mode is
   type Access_Mode is (Read_Write, Read_Only);
   for Access_Mode use (Read_Write => 0, Read_Only => 1);
   function Covers
     (Resource_Base, Resource_Bytes, Base, Bytes : Unsigned_64)
      return Boolean
   with Global => null,
     Post => (if Covers'Result then
       Bytes > 0 and then Resource_Bytes > 0
       and then Base >= Resource_Base
       and then Bytes - 1 <= Unsigned_64'Last - Base
       and then Resource_Bytes - 1 <= Unsigned_64'Last - Resource_Base
       and then Base - Resource_Base <= Resource_Bytes - Bytes);
   function Allows
     (Can_Read, Can_Write : Boolean; Requested : Access_Mode) return Boolean
   is (Can_Read and then (Requested = Read_Only or else Can_Write))
   with Global => null;
end Device_Memory_Admission;
