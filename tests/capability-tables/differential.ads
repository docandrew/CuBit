-- Randomized differential test: drive a table instance with a fixed-seed
-- stream of operations and compare every result and every slot with a
-- simple, independent reference model after each operation.
with Interfaces; use Interfaces;
with Capability_Tables;

generic
   with package Tab is new Capability_Tables (<>);
   with function Make (Id : Unsigned_64) return Tab.Element;
   with function Id_Of (E : Tab.Element) return Unsigned_64;
procedure Differential
  (Seed       : Unsigned_64;
   Operations : Natural;
   Owner      : Unsigned_64;
   Name       : String);
