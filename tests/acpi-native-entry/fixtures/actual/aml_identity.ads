package AML_Identity with SPARK_Mode, Pure is
   type Identity is private;
   No_Identity : constant Identity;
   function Ordinal (Token : Identity) return Natural with Ghost;
private
   type Identity is new Natural;
   No_Identity : constant Identity := 0;
   function Ordinal (Token : Identity) return Natural is (Natural (Token));
end AML_Identity;
