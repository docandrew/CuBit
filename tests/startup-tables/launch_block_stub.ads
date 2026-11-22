--  Hosted stand-in for the runtime's CuBit.Launch_Block: the tests load
--  startup tables themselves (CuBit.Startup_State.Load).
with CuBit.Startup_State;
package CuBit.Launch_Block is
   function Outcome return CuBit.Startup_State.Load_Result is
     (CuBit.Startup_State.Malformed);
   function Table_Present return Boolean is (False);
end CuBit.Launch_Block;
