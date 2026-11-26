pragma Ada_2022;
with Interfaces;
-- Test-only, finite simulated SystemMemory. No addresses are dereferenced.
package Hosted_Byte_Memory with SPARK_Mode, Pure is
   subtype Number is Interfaces.Unsigned_64;
   subtype Octet is Interfaces.Unsigned_8;
   use type Number;
   Last_Address : constant Number := 65_535;
   type State is private;
   function Empty return State;
   function Can_Access (Address : Number; Width : Natural) return Boolean is
     (Width in 1 .. 8 and then Address <= Last_Address
      and then Number (Width - 1) <= Last_Address - Address);
   procedure Read
     (Memory : State; Address : Number; Width : Natural;
      Value : out Number; Accepted : out Boolean)
     with Global => null,
       Post => Accepted = Can_Access (Address, Width)
         and then (if not Accepted then Value = 0);
   procedure Write
     (Memory : in out State; Address : Number; Width : Natural;
      Value : Number; Accepted : out Boolean)
     with Global => null,
       Post => Accepted = Can_Access (Address, Width)
         and then (if not Accepted then Memory = Memory'Old);
private
   subtype Cell_Index is Natural range 0 .. 65_535;
   type Cells is array (Cell_Index) of Octet;
   type State is record
      Data : Cells := [others => 0];
   end record;
   function Empty return State is ((Data => [others => 0]));
end Hosted_Byte_Memory;
