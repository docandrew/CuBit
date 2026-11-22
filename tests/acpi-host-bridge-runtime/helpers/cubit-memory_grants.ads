with CuBit.Grant_References;
package CuBit.Memory_Grants is
   subtype Grant_Reference is CuBit.Grant_References.Reference;
   Model_Retired : Boolean := False;
   Queries : Natural := 0;
   Last_Query : Grant_Reference;
   function Retirement_Confirmed (Reference : Grant_Reference) return Boolean;
end CuBit.Memory_Grants;
