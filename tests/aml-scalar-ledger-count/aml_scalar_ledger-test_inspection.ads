generic
package AML_Scalar_Ledger.Test_Inspection with Pure is
   function Scanned_Objects (Store : State) return Natural;
   function Issued (Store : State) return Natural;
   procedure Set_Cached_Count (Store : in out State; Count : Natural);
end AML_Scalar_Ledger.Test_Inspection;
