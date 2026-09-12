with Interfaces;
with CuBit.Network_Authority;

--  Bootstrap policy for the shipped development image, not signer validation.
--  The exact root executable name is trusted installation policy; replace
--  this rule with approved installation identities when those are available.
package CuBit.Launch_Policy with SPARK_Mode => On is
   type Network_Approval is
     (No_Network, Declared_Network, Browser_Outbound);

   function Desktop_Approval
     (Name : String; Sender, Desktop_PID : Interfaces.Unsigned_64)
      return Network_Approval;

   function Allows
     (Approval : Network_Approval; Requested : Network_Authority.Scope)
      return Boolean;
end CuBit.Launch_Policy;
