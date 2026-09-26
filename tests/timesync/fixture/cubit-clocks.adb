--  Hosted stub: the real body reads the clock over IPC. The pure cores under
--  test use only the spec's types and expression functions.
package body CuBit.Clocks is
   procedure Read (Value : out Snapshot; Success : out Boolean) is
   begin
      Value := (others => <>);
      Success := False;
   end Read;
end CuBit.Clocks;
