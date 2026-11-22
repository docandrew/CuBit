--  Hosted stub: the real body reads the clock over IPC. The pure cores under
--  test use only the spec's types and expression functions.
package body CuBit.Clocks is
   procedure Read (Service : CuBit.Handles.Endpoint; Value : out Snapshot; Success : out Boolean) is
      pragma Unreferenced (Service);
   begin
      Value := (others => <>);
      Success := False;
   end Read;
end CuBit.Clocks;
