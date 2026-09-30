package body Intel_GPU_MOCS_Control_Registers with SPARK_Mode is
   function Matches (Raw, Expected : Interfaces.Unsigned_32) return Boolean is
      R : constant Control_Register := Decode (Raw);
      E : constant Control_Register := Decode (Expected);
   begin
      -- Policy fields must agree individually; the existing reserved-zero
      -- admission policy is retained, not relaxed using an older-generation
      -- inference. Every bit is represented, including unknown observations.
      return R.Cacheability = E.Cacheability and then
        R.Target_Cache = E.Target_Cache and then
        R.LRU_Management = E.LRU_Management and then
        R.Do_Not_Allocate_On_Miss = E.Do_Not_Allocate_On_Miss and then
        R.Reverse_Skip_Caching = E.Reverse_Skip_Caching and then
        R.Skip_Caching_Control = E.Skip_Caching_Control and then
        R.Page_Fault_Mode = E.Page_Fault_Mode and then
        R.Snoop_Control = E.Snoop_Control and then
        R.Class_Of_Service = E.Class_Of_Service and then
        R.Self_Snoop = E.Self_Snoop and then R.Reserved_High = 0;
   end Matches;
end Intel_GPU_MOCS_Control_Registers;
