--  The Linux preview cannot turn a CuBit machine off: a Linux-hosted
--  stand-in, not CuBit. machine.* is not published there.
package body CCL_Machine is
   function Available return Boolean is (False);
   procedure Request
     (Op : CCL.Interfaces.Machine.Operation; Result : out CCL.Interfaces.Machine.Answer;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      pragma Unreferenced (Op);
   begin
      Result := CCL.Interfaces.Machine.Unavailable;
      Success := False;
      Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "machine: the Linux preview has no power.svc");
   end Request;
end CCL_Machine;
