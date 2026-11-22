with CuBit.Presentation_State;
package Presentation_Proof with SPARK_Mode is
   --  Prove real instances: GNATprove deliberately skips generic templates.
   package Production is new CuBit.Presentation_State;
   package Exhaustion_Fixture is new CuBit.Presentation_State (3);
end Presentation_Proof;
