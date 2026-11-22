-- Hosted boundary model. Actual generated Decode is unchanged; Value/Require
-- are not invoked by these tests. Native semantics uses the real runtime body.
package body CuBit.Launch_Parameters is
   procedure Get (Expected : CuBit.Parameter_Signatures.Identity;
      Value : out String; Length : out Natural; Result : out Outcome) is
      pragma Unreferenced (Expected);
   begin Value := [others => ' ']; Length := 0; Result := No_Launch_Block; end Get;
   procedure Refuse (Program : String; Why : Outcome) is
      pragma Unreferenced (Program, Why);
   begin raise Program_Error with "native launch block unavailable in hosted test"; end Refuse;
end CuBit.Launch_Parameters;
