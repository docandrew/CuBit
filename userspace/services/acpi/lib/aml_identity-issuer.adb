package body AML_Identity.Issuer with SPARK_Mode,
  Refined_State => (Issuance => Last_Issued) is
   Last_Issued : Natural := 0;
   function Issued_Count return Natural is (Last_Issued);
   function Is_Issued (Token : Identity) return Boolean is
     (Token /= No_Identity and then Natural (Token) <= Last_Issued);
   procedure Issue (Token : out Identity; Success : out Boolean) is
   begin
      if Last_Issued = Natural'Last then
         Token := No_Identity; Success := False;
      else
         Last_Issued := Last_Issued + 1;
         Token := Identity (Last_Issued); Success := True;
      end if;
   end Issue;
end AML_Identity.Issuer;
