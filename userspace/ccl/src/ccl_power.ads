with CuBit.Failures;
with CuBit.Power_Sources;

--  power.svc's power sources for power.* (CCL.Interfaces.Power), one body
--  per platform: native/ reads power.svc's snapshot through the program's
--  power-sources endpoint (CuBit.Power_Sources: Read_Now, then every page
--  of every source against one generation); the Linux preview's host/ has
--  none (Available is False there, so the interface is not published), a
--  stand-in, not CuBit.
package CCL_Power is
   function Available return Boolean;
   procedure Read
     (Sources : out CuBit.Power_Sources.Source_Array; Count : out CuBit.Power_Sources.Source_Count;
      Success : out Boolean; Why : out CuBit.Failures.Failure);
end CCL_Power;
