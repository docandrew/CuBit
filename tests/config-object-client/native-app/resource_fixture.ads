with Interfaces;

package Resource_Fixture is
   -- Once per test process, after org.cubit.publication contains Integer 42.
   -- Uses actual Config IPC, no database/filesystem authority or client codec.
   procedure Run (Token : in out Interfaces.Unsigned_64; Good : out Boolean);
end Resource_Fixture;
