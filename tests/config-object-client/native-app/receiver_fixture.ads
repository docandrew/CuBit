with Interfaces;
with CCL.Objects;

package Receiver_Fixture is
   -- Dedicated single-run host fixture: acquire existing nested collection at
   -- revision 1, read First, write Second, read back revision 2, close. Native
   -- object imports with separate resource receivers; no client-side codec.
   procedure Run
     (Contract : CCL.Objects.Binding; First, Second : CCL.Objects.Image;
      Token : in out Interfaces.Unsigned_64; Good : out Boolean);
end Receiver_Fixture;
