package body CuBit.Launch_Policy with SPARK_Mode => On is

   function Allows
     (Approval : Network_Approval; Requested : Network_Authority.Scope)
      return Boolean
   is
     (case Approval is
         when No_Network => False,
         when Declared_Network => Network_Authority.Valid (Requested));
end CuBit.Launch_Policy;
