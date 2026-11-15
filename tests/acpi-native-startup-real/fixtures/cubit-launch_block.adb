-- Hosted launch boundary: capabilities/parameters are supplied by named test
-- models. No guest launch block is mapped or read during elaboration.
package body CuBit.Launch_Block is
 function Outcome return CuBit.Startup_State.Load_Result is (CuBit.Startup_State.Malformed);
 function Table_Present return Boolean is (False);
end CuBit.Launch_Block;
