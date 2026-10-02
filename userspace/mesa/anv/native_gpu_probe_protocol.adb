package body Native_GPU_Probe_Protocol with SPARK_Mode is
   function Valid_Desktop_Request (Value : Words) return Boolean is
     (Value = [1, 0, 0, 0]);
   function Valid_Desktop_Reply (Value : Words) return Boolean is
     (Value (0) <= 2 and then Value (1) = 1 and then
      Value (2) = 0 and then Value (3) = 0);
   function Request (Action : Operation; Reference : Unsigned_64 := 0)
     return Words is
     ([1, (if Action = Read_Target then 0 else 1), Reference, 0]);

   function Valid_Request (Value : Words) return Boolean is
     (Value (0) = 1 and then Value (3) = 0 and then
      ((Value (1) = 0 and then Value (2) = 0) or else
       (Value (1) = 1 and then Value (2) /= 0)));

   function Reply (Code : Status; Reference : Unsigned_64 := 0) return Words is
     ([Unsigned_64 (Code'Enum_Rep), 1,
       (if Code = Success then Reference else 0),
       (if Code = Success and then Reference /= 0 then Pixel_Bytes else 0)]);

   function Valid_Reply (Action : Operation; Value : Words) return Boolean is
     (Value (1) = 1 and then Value (0) <= 4 and then
      (if Value (0) /= 0 then
         Value (2) = 0 and then Value (3) = 0 and then
           (Value (0) /= 4 or else Action = Retire_Target)
       elsif Action = Read_Target then
         Value (2) /= 0 and then Value (3) = Pixel_Bytes
       else Value (2) = 0 and then Value (3) = 0));
end Native_GPU_Probe_Protocol;
