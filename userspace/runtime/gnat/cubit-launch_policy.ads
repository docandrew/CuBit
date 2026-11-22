with CuBit.Network_Authority;

--  A startup plan entry's network approval: (network approve-declared)
--  approves the scopes its manifest declares. A program another program
--  launches gets network only within a scope its launcher holds (procmgr's
--  Launch_Attenuation); no program name carries an approval (CAP-011).
package CuBit.Launch_Policy with SPARK_Mode => On is
   type Network_Approval is (No_Network, Declared_Network);

   function Allows
     (Approval : Network_Approval; Requested : Network_Authority.Scope)
      return Boolean;
end CuBit.Launch_Policy;
