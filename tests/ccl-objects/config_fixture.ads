with CCL.Objects;
with Config_Objects;

-- Hosted-only successful backend replies for the original value bridge tests.
-- Fault/lifetime cases are tested separately; this is not a storage backend.
package Config_Fixture is
   procedure Load_Empty (Object : in out Config_Objects.State);
   procedure Commit
     (Object : in out Config_Objects.State; Value : CCL.Objects.Image;
      Expected : Config_Objects.Number; Result : out Config_Objects.Outcome);
end Config_Fixture;
