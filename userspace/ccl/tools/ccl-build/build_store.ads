with Ada.Strings.Unbounded;
with Storehouse;

--  The Storehouse on a Linux host (docs/ccl-build.md, "The Storehouse"):
--
--     <root>/files/<identity>        immutable file, mode 0444
--     <root>/executables/<identity>  immutable executable file, mode 0555
--     <root>/trees/<identity>/       immutable tree, read-only
--     <root>/imports/<identity>      a Nix store tree recorded in place
--     <root>/actions/<identity>      step identity -> its outputs
--     <root>/roots/<name>            the identities a root keeps
--     <root>/work/<process>-<n>      staging and step directories
--
--  An entry appears only by rename, after its content is complete, synced
--  and read-only, so a crash leaves garbage in work/ but never a partial
--  entry. Opening the store deletes the work of processes that are gone.
package Build_Store is
   use Storehouse;

   Store_Error : exception;

   type Store is tagged private;

   function Open (Root : String) return Store;
   function Root (S : Store) return String;

   --  Modification time of every entry file: the epoch's first second, as
   --  in Nix, so tools that record source times (GNAT .ali files) see the
   --  same value for the same content.
   Entry_Time_Seconds : constant := 1;

   function Path_Of (S : Store; Kind : Entry_Kind; Identity : Digest) return String;
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

   --  Action records: the canonical encoding of a step's outputs, stored
   --  under the step's identity once its outputs are entries. A second,
   --  different record for one step means the step is not reproducible;
   --  the first is kept and Conflicting is set.
   procedure Put_Action (S : Store; Step : Digest; Data : Byte_Array; Conflicting : out Boolean);
   function Has_Action (S : Store; Step : Digest) return Boolean;
   function Get_Action (S : Store; Step : Digest) return Byte_Array;

   --  Roots: the latest record of what a root keeps, replaced atomically.
   procedure Put_Root (S : Store; Name : String; Data : Byte_Array);

   --  A fresh directory under work/, owned by this process.
   function New_Work (S : Store) return String;

   --  The directory a step runs in: work/step-<shape> (Build_Steps.Shape),
   --  the same for every run of the step in this store, so that tools that
   --  record the paths they were given (GNAT .ali files record
   --  -ffile-prefix-map) give the same bytes each time. Claiming it records
   --  this process as its owner (work/step-<shape>.owner, placed by link so
   --  it is never partial); a step another live process is running is
   --  refused.
   function Claim_Step (S : Store; Shape : Digest) return String;
   --  Removes the step directory (unless Keep) and the claim.
   procedure Release_Step (S : Store; Shape : Digest; Keep : Boolean := False);
   --  Deletes a directory tree, including read-only directories in it.
   procedure Remove (Name : String);

   --  Re-hash every entry. Report is called for each entry whose content
   --  no longer matches its name; Checked counts the entries examined.
   generic
      with procedure Report (Kind : Entry_Kind; Name : String);
   procedure Verify (S : Store; Checked : out Natural);

private
   type Store is tagged record
      Root : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end Build_Store;
