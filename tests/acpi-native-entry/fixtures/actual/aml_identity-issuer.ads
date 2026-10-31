package AML_Identity.Issuer with SPARK_Mode,
  Abstract_State => Issuance, Initializes => Issuance is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- One library-level issuer, not an instantiable generic: issuing the same
   -- token from separate instances would defeat cross-arena rejection.
   function Issued_Count return Natural with Global => (Input => Issuance);
   function Is_Issued (Token : Identity) return Boolean
     with Global => (Input => Issuance),
     Post => Is_Issued'Result = (Token /= No_Identity and then Ordinal (Token) <= Issued_Count);
   procedure Issue (Token : out Identity; Success : out Boolean)
     with Global => (In_Out => Issuance),
     Post => (if Issued_Count'Old < Natural'Last then
       Success and then Token /= No_Identity
       and then Issued_Count = Issued_Count'Old + 1
       and then Ordinal (Token) = Issued_Count
       else not Success and then Token = No_Identity
         and then Issued_Count = Issued_Count'Old);
end AML_Identity.Issuer;
