with Build_Store;
with Storehouse;
with Storehouse.Profiles;

--  Garbage collection of the Storehouse (docs/ccl-build.md, "Generations
--  and garbage collection"): delete every entry no root reaches.
--
--  Roots: every retained generation of every profile (its outputs and the
--  parameters that made it) and each existing checkout's last build of each
--  component (every step's outputs and the pinned trees it used). Builds in
--  flight need no roots: collection runs only with the store opened for
--  Collection, which waits until no other process uses the store and keeps
--  every other process out until it is done (Build_Store), so nothing a
--  build or switch is about to name can be deleted under it.
--
--  Kept always: action records (small; a step whose outputs were collected
--  runs again and its record checks that it gives the same outputs), Nix
--  store trees recorded in place (Nix owns them; their records are a few
--  bytes), and anything a root names whose record cannot be read: a damaged
--  root or generation stops the collection instead of losing what it keeps.
package Build_Collection is
   use Storehouse;

   type Summary is record
      Kept, Deleted : Natural := 0;           --  entries
      Freed : Build_Store.Byte_Count := 0;    --  bytes in deleted entries
      Generations_Deleted : Natural := 0;
      Dead_Roots : Natural := 0;              --  checkouts' roots whose checkout is gone
      Damaged_Removed : Natural := 0;         --  entries removed from damaged/
   end record;

   Collection_Error : exception;

   --  With Dry_Run nothing changes and Report names what would be deleted.
   --  Keep_Generations: 0 keeps every generation; N keeps the newest N of
   --  each profile and its current one.
   generic
      with procedure Report (Kind : Entry_Kind; Identity : Digest; Bytes : Build_Store.Byte_Count);
   procedure Collect
     (Store : Build_Store.Store; Dry_Run : Boolean; Keep_Generations : Storehouse.Profiles.Generation_Count;
      Result : out Summary);

end Build_Collection;
