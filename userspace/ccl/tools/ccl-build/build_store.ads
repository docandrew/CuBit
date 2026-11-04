with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Interfaces;
with Build_POSIX;
with Storehouse;

--  The Storehouse on a Linux host (docs/ccl-build.md, "The Storehouse"):
--
--     <root>/files/<identity>        immutable file, mode 0444
--     <root>/executables/<identity>  immutable executable file, mode 0555
--     <root>/trees/<identity>/       immutable tree, read-only
--     <root>/imports/<identity>      a Nix store tree recorded in place
--     <root>/actions/<identity>      step identity -> its outputs
--     <root>/roots/checkouts/<key>/  the last build of each component in
--                                    one checkout (<key>/checkout names it)
--     <root>/roots/<name>            a root written by an older ccl-build
--     <root>/profiles/<profile>/     generations and the current pointer
--     <root>/damaged/                entries found not to match their name
--     <root>/locks/                  advisory lock files
--     <root>/work/<process>-<n>      staging, owned by a live process
--     <root>/work/step-<shape>       a step's directory while it runs
--
--  Crash consistency: an entry, action record, generation or pointer
--  appears only by rename, after its content is complete, read-only and
--  synced, and the directory is synced after the rename; a crash leaves
--  garbage in work/ but never a partial entry. Several outputs of a step
--  become visible together: the action record naming them is renamed into
--  place last, so a step is either answered with all its outputs or run.
--
--  Integrity: the first time a process finds an entry present (Contains),
--  it re-hashes it. An entry that does not match its name is moved to
--  damaged/ and reported, and the caller sees it absent, so the step that
--  made it runs again or the source is stored again: a damaged entry is
--  never used. Nix store trees recorded in place are hashed when recorded
--  and by Verify, not on use (read-only, owned by Nix).
--
--  Concurrency: every process using a store holds the store's collection
--  lock shared for as long as it runs; the garbage collector holds it
--  exclusively. A step runs only under its step lock, so two builds of the
--  same step at once run it once: the second waits and is answered from
--  the action record. Locks are flock locks, released by the kernel when
--  the holder exits, so a dead builder never leaves a claim to break.
package Build_Store is
   use Storehouse;

   Store_Error : exception;

   type Store is tagged private;

   --  How a process uses the store: any number of Shared_Use processes
   --  (builds, switches, reads) run together; Collection (garbage
   --  collection) runs alone. Open waits, saying so on standard error,
   --  until its role is granted.
   type Store_Role is (Shared_Use, Collection);

   function Open (Given_Root : String; Role : Store_Role) return Store;
   function Root (S : Store) return String;

   --  Modification time of every entry file: the epoch's first second, as
   --  in Nix, so tools that record source times (GNAT .ali files) see the
   --  same value for the same content.
   Entry_Time_Seconds : constant := 1;

   function Path_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return String;
   --  Present and intact (see "Integrity" above): a damaged entry is moved
   --  to damaged/ and reported absent.
   function Contains (S : Store; Kind : Entry_Kind; Identity : Digest) return Boolean;

   --  Identities of host files and trees (Tree: the canonical listing).
   --  A tree's links must be relative unless Absolute_Links (Nix store
   --  trees link to other store paths).
   function Hash_File (Name : String) return Digest;
   function Hash_Tree (Name : String; Absolute_Links : Boolean := False) return Digest;

   --  Snapshot a host file or tree into the store. The bytes are hashed,
   --  copied only if the entry is absent, and the copy is hashed again: a
   --  source that changes meanwhile is refused, never stored under the
   --  wrong name.
   procedure Put_File (S : Store; Source : String; Kind : Entry_Kind; Identity : out Digest)
   with Pre => Kind /= Tree;
   procedure Put_Tree (S : Store; Source : String; Identity : out Digest);

   --  A tree in the Nix store, recorded in place by its identity instead of
   --  copied: Nix store paths are immutable, and a copy would break their
   --  absolute references to other store paths (the compiler's configured
   --  assembler, header links). Contains and Path_Of then answer for it as
   --  for a tree entry, and Verify re-hashes it.
   Nix_Store : constant String := "/nix/store/";
   procedure Record_Nix_Tree (S : Store; Source : String; Identity : out Digest);
   --  Whether a tree entry is a Nix store tree recorded in place.
   function Is_Imported (S : Store; Identity : Digest) return Boolean;

   --  Action records: the canonical encoding of a step's outputs, stored
   --  under the step's identity once its outputs are entries. A second,
   --  different record for one step means the step is not reproducible;
   --  the first is kept and Conflicting is set.
   procedure Put_Action (S : Store; Step : Digest; Data : Byte_Array; Conflicting : out Boolean);
   function Has_Action (S : Store; Step : Digest) return Boolean;
   function Get_Action (S : Store; Step : Digest) return Byte_Array;
   --  Moves a record that cannot be decoded to damaged/, so the step runs
   --  again and records it anew.
   procedure Quarantine_Action (S : Store; Step : Digest);

   --  Roots: the last build of each component, per checkout. Name is the
   --  record's name in the checkout's root directory, Checkout the source
   --  tree's absolute path; a checkout's roots die with its directory.
   procedure Put_Root (S : Store; Checkout, Name : String; Data : Byte_Array);

   --  Small records named by relative paths under the store's root
   --  (generations, profile pointers; Storehouse.Paths.Is_Relative).
   --  Publish_New places a read-only record unless the name exists
   --  (Placed False); Publish_Replace replaces it with one rename.
   procedure Publish_New (S : Store; Name : String; Data : Byte_Array; Placed : out Boolean);
   procedure Publish_Replace (S : Store; Name : String; Data : Byte_Array);
   function Read_Record (S : Store; Name : String) return Byte_Array;
   function Record_Exists (S : Store; Name : String) return Boolean;
   procedure Delete_Record (S : Store; Name : String);

   package Name_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   --  The names in a directory under the root, sorted by bytes ("" if
   --  absent: none).
   function Names (S : Store; Directory : String) return Name_Vectors.Vector;

   --  A named advisory lock under locks/, held until Unlock (or exit).
   type Named_Lock is limited private;
   procedure Lock (S : Store; Name : String; L : in out Named_Lock; Waiting_Message : String);
   procedure Unlock (L : in out Named_Lock);

   --  A fresh directory under work/, owned by this process.
   function New_Work (S : Store) return String;

   --  The directory a step runs in: work/step-<shape> (Build_Steps.Shape),
   --  the same for every run of the step in this store, so that tools that
   --  record the paths they were given (GNAT .ali files record
   --  -ffile-prefix-map) give the same bytes each time. Claiming it takes
   --  the step's lock, waiting (and saying so, naming Label) while another
   --  process runs the step; the caller then looks for its action record
   --  again before running it.
   function Claim_Step (S : Store; Shape : Digest; Label : String) return String;
   --  Removes the step directory (unless Keep) and releases the claim.
   procedure Release_Step (S : Store; Shape : Digest; Keep : Boolean := False);
   --  Deletes a directory tree, including read-only directories in it.
   procedure Remove (Name : String);

   --  Re-hash every entry. Report is called for each entry whose content
   --  no longer matches its name; Checked counts the entries examined.
   generic
      with procedure Report (Kind : Entry_Kind; Name : String);
   procedure Verify (S : Store; Checked : out Natural);

   --  Garbage collection's view (Build_Collection): every entry, and its
   --  deletion. Discard first renames the entry out of its directory, so
   --  it disappears whole, then deletes it.
   generic
      with procedure Visit (Kind : Entry_Kind; Identity : Digest);
   procedure For_Each_Entry (S : Store);
   subtype Byte_Count is Interfaces.Unsigned_64;
   function Size_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return Byte_Count;
   procedure Discard (S : Store; Kind : Entry_Kind; Identity : Digest)
   with Pre => not Is_Imported (S, Identity) or else Kind /= Tree;
   --  Every root record, Live False for one of a checkout that no longer
   --  exists. Name is the record's path under roots/.
   generic
      with procedure Visit (Name : String; Data : Byte_Array; Live : Boolean);
   procedure For_Each_Root (S : Store);
   --  Live processes other than this one with work in the store, as
   --  " <process>" items ("" if none). With the store opened for
   --  Collection, these can only be older ccl-builds, which take no
   --  collection lock: collecting under them could delete what they use.
   function Other_Users (S : Store) return String;
   --  Deletes the roots of checkouts that no longer exist.
   procedure Remove_Dead_Roots (S : Store);
   --  Deletes what damaged/ holds; Removed counts the entries.
   procedure Empty_Damaged (S : Store; Removed : out Natural);

private
   type Store is tagged record
      Root : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Named_Lock is limited record
      Held : Build_POSIX.Lock := Build_POSIX.No_Lock;
   end record;
end Build_Store;
