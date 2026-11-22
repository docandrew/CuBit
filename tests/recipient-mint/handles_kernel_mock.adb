--  Hosted body of CuBit.Handles.Kernel: the system call is scripted by
--  Kernel_Mock. Only Call is replaced; the word conversions are the real
--  spec's.
with System.Storage_Elements;
with Kernel_Mock;
package body CuBit.Handles.Kernel is
   function At_Address (Word : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Word)));

   procedure Take_Derivation (Word : Unsigned_64) is
      Source : ABI.Derivation with Import, Address => At_Address (Word);
   begin
      Kernel_Mock.Last_Derivation := Source;
   end Take_Derivation;

   function Call
     (Number : Unsigned_64;
      A0, A1, A2, A3, A4, A5, A6 : Unsigned_64 := 0) return Unsigned_64 is
   begin
      Kernel_Mock.Calls := Kernel_Mock.Calls + 1;
      Kernel_Mock.Last_Number := Number;
      Kernel_Mock.Last_Arguments := [A0, A1, A2, A3, A4, A5, A6];
      if Number = Inspect then
         declare
            Output : ABI.Inspection with Import, Address => At_Address (A1);
         begin
            if Kernel_Mock.Live then
               Output := Kernel_Mock.Inspection;
            end if;
         end;
         return Kernel_Mock.Inspect_Result;
      elsif Number = Install_Capability then
         Take_Derivation (A1);
      elsif Number in Reply_With_Capability | Reply_Saved_With_Capability then
         Take_Derivation (A2);
      elsif Number = Derive_Capability then
         Take_Derivation (A0);
      end if;
      return Kernel_Mock.Answer;
   end Call;
end CuBit.Handles.Kernel;
