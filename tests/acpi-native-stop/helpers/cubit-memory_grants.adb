package body CuBit.Memory_Grants is
   function Retirement_Confirmed (Reference : Grant_Reference) return Boolean is
   begin Queries := Queries + 1; Last_Query := Reference; return Model_Retired; end Retirement_Confirmed;
end CuBit.Memory_Grants;
