package body Device_Memory_Admission with SPARK_Mode is
   function Covers
     (Resource_Base, Resource_Bytes, Base, Bytes : Unsigned_64)
      return Boolean is
   begin
      return Resource_Bytes > 0 and then Bytes > 0
        and then Resource_Bytes - 1 <= Unsigned_64'Last - Resource_Base
        and then Bytes - 1 <= Unsigned_64'Last - Base
        and then Base >= Resource_Base
        and then Bytes <= Resource_Bytes
        and then Base - Resource_Base <= Resource_Bytes - Bytes;
   end Covers;
end Device_Memory_Admission;
