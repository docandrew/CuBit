with CCL.Interfaces.Machine;
with CuBit.Failures;

--  power.svc for machine.* (CCL.Interfaces.Machine), one body per
--  platform: native/ asks power.svc through the console's power-console
--  badge (CuBit.Power_Requests); the Linux preview's host/ has none
--  (Available is False there, so the interface is not published), a
--  stand-in, not CuBit.
package CCL_Machine is
   function Available return Boolean;
   procedure Request
     (Op : CCL.Interfaces.Machine.Operation; Result : out CCL.Interfaces.Machine.Answer;
      Success : out Boolean; Why : out CuBit.Failures.Failure);
end CCL_Machine;
