package body Intel_GPU_Display_Claim with SPARK_Mode is
   function Owner (Object : Claim) return Unsigned_64 is (Object.Holder);
   procedure Take
     (Object : in out Claim; Designated, Caller : Unsigned_64;
      Badge_Valid, Device_Valid : Boolean; Allowed : out Boolean)
   is
   begin
      Allowed := Object.Holder = 0 and Designated /= 0 and
        Caller = Designated and Badge_Valid and Device_Valid;
      if Allowed then Object.Holder := Caller; end if;
   end Take;
end Intel_GPU_Display_Claim;
