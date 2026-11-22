with CCL.Secondary_Stacks;

--  GNATprove analyzes generic bodies through concrete instantiations.  Keep a
--  representative maximum-sized CCL region in the proof suite so the generic
--  does not silently receive only flow analysis.
package Secondary_Stacks_Proof with
   SPARK_Mode => On
is
   package Subject is new CCL.Secondary_Stacks
     (Capacity => 65_536, Max_Values => 256);
end Secondary_Stacks_Proof;
