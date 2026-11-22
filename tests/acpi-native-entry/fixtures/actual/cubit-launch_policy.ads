with CuBit.Network_Authority;
with CuBit.Process_IDs;

--  Bootstrap policy for the shipped development image, not signer validation.
--  The exact root executable name is trusted installation policy; replace
--  this rule with approved installation identities when those are available.
package CuBit.Launch_Policy with SPARK_Mode => On is
   type Network_Approval is
     (No_Network, Declared_Network, Browser_Outbound);

   function Desktop_Approval
     (Name : String; Sender, Desktop_PID : CuBit.Process_IDs.Process_ID)
      return Network_Approval;

   function Allows
     (Approval : Network_Approval; Requested : Network_Authority.Scope)
      return Boolean;

   --  A render session for an installed GPU application Desktop starts
   --  (its Apps menu). Same transitional rule as Desktop_Approval: the exact
   --  root executable name, from the registered Desktop only. procmgr still
   --  asks devmgr to admit the session; a refusal (no Intel GPU) starts the
   --  program without one when it declared render optional.
   function Desktop_Render_Approval
     (Name : String; Sender, Desktop_PID : CuBit.Process_IDs.Process_ID)
      return Boolean;
end CuBit.Launch_Policy;
