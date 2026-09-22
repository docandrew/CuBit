with Memory_Grants.Loans;
with CuBit.Presentation_State;
package Loan_Proof with SPARK_Mode is
   package Production is new Memory_Grants.Loans;
   package Bounded is new Memory_Grants.Loans (Maximum_Sequence => 3);
   package Presentation is new CuBit.Presentation_State;
end Loan_Proof;
