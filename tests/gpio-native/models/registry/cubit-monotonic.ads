------------------------------------------------------------------------------
--  Hosted fixture: the part of CuBit.Monotonic the driver code under test
--  reads (userspace/runtime/gnat/cubit-monotonic.ads).
------------------------------------------------------------------------------
with Interfaces;
package CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64;
end CuBit.Monotonic;
