--  Pure decision core. Inputs are trusted, already scope-specific evidence;
--  this package neither authenticates evidence nor installs capabilities.
package CuBit.Authority_Policy with Pure, SPARK_Mode is
   type Decision is
     (Approved, Not_Requested, Installation_Denied, Session_Denied,
      Issuer_Denied);
   function Evaluate
     (Requested, Installation_Approved, Session_Approved,
      Issuer_Allowed : Boolean)
      return Decision is
     (if not Requested then Not_Requested
      elsif not Installation_Approved then Installation_Denied
      elsif not Session_Approved then Session_Denied
      elsif not Issuer_Allowed then Issuer_Denied
      else Approved)
     with Post =>
       (Evaluate'Result = Approved) =
         (Requested and Installation_Approved and
          Session_Approved and Issuer_Allowed);

   type Bootstrap_Authority is
     (Log_Publication, Log_Observation, Master_Audio);
   --  Transitional development-image approval: entry in the trusted startup
   --  plan approves declared privileged requests. OP_SPAWN cannot select it.
   --  Publication preserves the existing manifest-request policy.
   function Bootstrap_Approves
     (Authority : Bootstrap_Authority; Trusted_Startup : Boolean)
      return Boolean is
     (Authority = Log_Publication or else Trusted_Startup);
end CuBit.Authority_Policy;
