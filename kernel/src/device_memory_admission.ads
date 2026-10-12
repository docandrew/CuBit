with Interfaces; use Interfaces;
package Device_Memory_Admission with SPARK_Mode, Pure is
   type Access_Mode is (Read_Write, Read_Only);
   for Access_Mode use (Read_Write => 0, Read_Only => 1);
   -- [Base, Base + Bytes) lies within [Resource_Base, Resource_Base +
   -- Resource_Bytes); both ranges nonempty and nonwrapping. Stated by
   -- first and last byte so containment composes by transitivity.
   function Covers
     (Resource_Base, Resource_Bytes, Base, Bytes : Unsigned_64)
      return Boolean
   is (Resource_Bytes > 0 and then Bytes > 0
       and then Resource_Bytes - 1 <= Unsigned_64'Last - Resource_Base
       and then Bytes - 1 <= Unsigned_64'Last - Base
       and then Base >= Resource_Base
       and then Base + (Bytes - 1) <= Resource_Base + (Resource_Bytes - 1))
   with Global => null;
   function Allows
     (Can_Read, Can_Write : Boolean; Requested : Access_Mode) return Boolean
   is (Can_Read and then (Requested = Read_Only or else Can_Write))
   with Global => null;
end Device_Memory_Admission;
