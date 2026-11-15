package body CuBit.Monotonic is
   function Read return Reading is ((Available => True, Microseconds => Fixture_Us));
   function Microseconds return Interfaces.Unsigned_64 is (Fixture_Us);
   function Milliseconds return Interfaces.Unsigned_64 is (Fixture_Ms);
end CuBit.Monotonic;
