pragma Ada_2022;

--  Applying the current system generation at boot (SYS-003): before the
--  startup plan runs, procmgr reads @system/storehouse/profiles/system/
--  current and the generation record it names, and starts each pinned
--  startup-plan entry from its pinned program (executables/<sha256>)
--  instead of the image's. This is how a pin whose restart contract is
--  (replace reboot), acpi.svc's, takes effect: activated "at next boot",
--  it is taken here. Pins of live-switchable entries are taken here too,
--  so a switch made live survives the reboot.
--
--  A pin is taken only when every check below holds; otherwise the entry
--  runs its image's program, procmgr logs why (to logstore, which keeps
--  the installed system's log on its disk) and counts the drift. The
--  manifest check is the live switch's (CuBit.Service_Replacement
--  Content_Admits): a pin never widens what an entry holds. No I/O here:
--  procmgr reads the files and reports the facts.
package Boot_Generations with SPARK_Mode, Pure is

   --  How the current pointer and the record it names read.
   type Generation_Reading is
     (No_Generation,     --  no current pointer: the image's programs run (generation 0)
      Read,              --  the record read and checked against its type
      Pointer_Invalid,   --  current is not a generation number in range
      Record_Missing,    --  current names a record that could not be read
      Record_Invalid);   --  the record does not check, or its number is not current's

   --  The pins are applied only from a record that read.
   function Applies (Reading : Generation_Reading) return Boolean is (Reading = Read);

   --  A generation is named but cannot be used: drift, logged as a warning.
   function Is_Drift (Reading : Generation_Reading) return Boolean is
     (Reading in Pointer_Invalid | Record_Missing | Record_Invalid);

   --  Gathered in this order; procmgr stops reading at the first False
   --  (later facts stay False).
   type Pin_Facts is record
      Entry_Known      : Boolean := False;   --  a startup-plan entry runs that executable
      First_For_Entry  : Boolean := False;   --  no earlier pin of this record names it
      Program_Readable : Boolean := False;   --  the pinned program read whole
      Content_Matches  : Boolean := False;   --  its bytes' SHA-256 is its storehouse name
      Image_Readable   : Boolean := False;   --  the entry's image program read whole
      Same_Manifest    : Boolean := False;   --  both declare the same manifest, byte for byte
   end record;

   type Pin_Verdict is
     (Applied,              --  the entry starts from the pinned program
      Unknown_Service,      --  no startup-plan entry runs that executable
      Duplicate_Pin,        --  the record pins that entry twice: neither is trusted
      Program_Unreadable,   --  the storehouse object is missing or unreadable
      Content_Mismatch,     --  the object's bytes are not its name (damaged)
      Image_Unreadable,     --  the image's program could not be read to compare
      Manifest_Changed);    --  the pinned program declares another manifest

   function Judge (F : Pin_Facts) return Pin_Verdict
   with Post =>
     (Judge'Result = Applied) =
       (F.Entry_Known and then F.First_For_Entry and then F.Program_Readable
        and then F.Content_Matches and then F.Image_Readable and then F.Same_Manifest)
     and then (if not F.Entry_Known then Judge'Result = Unknown_Service)
     and then (if F.Entry_Known and then not F.First_For_Entry then Judge'Result = Duplicate_Pin)
     and then (if F.Entry_Known and then F.First_For_Entry and then F.Program_Readable
                 and then not F.Content_Matches then Judge'Result = Content_Mismatch)
     and then (if F.Entry_Known and then F.First_For_Entry and then F.Program_Readable
                 and then F.Content_Matches and then F.Image_Readable
                 and then not F.Same_Manifest then Judge'Result = Manifest_Changed);

   --  Stable names for the log (native enumeration images are positions).
   function Reading_Name (Item : Generation_Reading) return String is
     (case Item is
        when No_Generation   => "no current generation",
        when Read            => "read",
        when Pointer_Invalid => "the current pointer is not a generation number",
        when Record_Missing  => "the generation record could not be read",
        when Record_Invalid  => "the generation record does not check against System_Generation");

   function Verdict_Name (Item : Pin_Verdict) return String is
     (case Item is
        when Applied            => "applied",
        when Unknown_Service    => "no startup-plan entry runs that service",
        when Duplicate_Pin      => "the record pins the service twice",
        when Program_Unreadable => "the pinned program is missing from the storehouse",
        when Content_Mismatch   => "the pinned program's bytes do not match its SHA-256 name",
        when Image_Unreadable   => "the image's program could not be read to compare manifests",
        when Manifest_Changed   => "the pinned program declares another manifest");

end Boot_Generations;
