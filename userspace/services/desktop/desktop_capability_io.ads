with Interfaces;
-- Raw kernel capability inspection only. Slot interpretation is SPARK policy
-- in the startup caller; successful inspection is not GPU release authority.
package Desktop_Capability_IO with SPARK_Mode is
   type Words is array (0 .. 5) of Interfaces.Unsigned_64;
   procedure Inspect (Slot : Interfaces.Unsigned_64;
      Data : out Words; Succeeded : out Boolean) with Global => null;
end Desktop_Capability_IO;
