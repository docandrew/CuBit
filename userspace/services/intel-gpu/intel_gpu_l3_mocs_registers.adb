package body Intel_GPU_L3_MOCS_Registers with SPARK_Mode is
   function Matches (Raw, Expected : Interfaces.Unsigned_32) return Boolean is
      R : constant Pair_Register := Decode (Raw);
      E : constant Pair_Register := Decode (Expected);
   begin
      return R.Lower_Skip_Enable = E.Lower_Skip_Enable and then
        R.Lower_Skip_Control = E.Lower_Skip_Control and then
        R.Lower_Cache = E.Lower_Cache and then
        R.Upper_Skip_Enable = E.Upper_Skip_Enable and then
        R.Upper_Skip_Control = E.Upper_Skip_Control and then
        R.Upper_Cache = E.Upper_Cache and then
        R.Lower_Reserved_6 = 0 and then R.Lower_Reserved_8 = 0 and then
        R.Upper_Reserved_6 = 0 and then R.Upper_Reserved_8 = 0;
   end Matches;
end Intel_GPU_L3_MOCS_Registers;
