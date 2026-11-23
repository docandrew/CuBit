--  The Linux preview has no power.svc (a stand-in, not CuBit): power.* is
--  not published here.
package body CCL_Power is
   function Available return Boolean is (False);

   procedure Read
     (Sources : out CuBit.Power_Sources.Source_Array; Count : out CuBit.Power_Sources.Source_Count;
      Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Sources := [others => (others => <>)];
      Count := 0;
      Success := False;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "power: the Linux preview has no power.svc");
   end Read;
end CCL_Power;
