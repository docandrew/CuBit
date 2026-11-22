with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Filesystems;
with CuBit.File_Access;
with CuBit.Filesystem_Places;
with Files_Places;
with Files_Status; use Files_Status;

package body Files_Places_Tests is
   package FS renames CuBit.Filesystems;
   package FA renames CuBit.File_Access;
   package FP renames CuBit.Filesystem_Places;

   Total_Checks, Total_Failures : Natural := 0;
   function Checks return Natural is (Total_Checks);
   function Failures return Natural is (Total_Failures);

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Total_Checks := Total_Checks + 1;
      if not Condition then
         Total_Failures := Total_Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   READ : constant Unsigned_8 := FA.Rights_To_Wire ([FA.Read_Objects => True, others => False]);
   ALL_RIGHTS : constant Unsigned_8 := FA.Rights_To_Wire (FA.All_Rights);

   --  A profile as the service encodes it.
   type Scope is record
      Prefix : String (1 .. 32) := [others => ' '];
      Length : Natural := 0;
      Rights : Unsigned_8 := READ;
   end record;
   type Scope_List is array (Positive range <>) of Scope;
   function S (Prefix : String; Rights : Unsigned_8 := READ) return Scope is
      Result : Scope := (Length => Prefix'Length, Rights => Rights, others => <>);
   begin
      Result.Prefix (1 .. Prefix'Length) := Prefix;
      return Result;
   end S;

   procedure Encode (Scopes : Scope_List; Wire : out FA.Wire_Bytes) is
   begin
      Wire := [others => 0];
      for N in Scopes'Range loop
         declare
            Base : constant Positive := (N - Scopes'First) * FA.Wire_Entry_Bytes + 1;
         begin
            Wire (Base + FP.Rights_At) := Scopes (N).Rights;
            Wire (Base + FP.Prefix_Length_At) := Unsigned_8 (Scopes (N).Length);
            for K in 1 .. Scopes (N).Length loop
               Wire (Base + FP.Prefix_At + K - 1) := Character'Pos (Scopes (N).Prefix (K));
            end loop;
         end;
      end loop;
   end Encode;

   --  Select_Places over Scopes and the present volumes' names, as the
   --  prefixes it writes, "|" after each.
   function Places_Of (Scopes : Scope_List; Present : FP.Root_Table; Room : Natural := FA.Maximum_Entries)
     return String
   is
      Wire : FA.Wire_Bytes (1 .. FA.Maximum_Entries * FA.Wire_Entry_Bytes);
      Output : FP.Place_Bytes (1 .. Room * FA.Wire_Entry_Bytes + 1);
      Written : Natural;
      Result : String (1 .. 1_024);
      Length : Natural := 0;
      Decoded : FA.Policy;
      OK : Boolean;
   begin
      Encode (Scopes, Wire);
      FP.Select_Places (Wire, Scopes'Length, Present, Output, Written);
      Check (Written <= Room, "only whole entries fit");
      if Written = Natural'Min (Room, FP.Count_Places (Wire, Scopes'Length, Present)) then
         null;
      else
         Check (False, "Count_Places is what Select_Places lists");
      end if;
      if Written > 0 then
         --  A client decodes the answer as File_Access entries.
         declare
            As_Wire : FA.Wire_Bytes (1 .. Written * FA.Wire_Entry_Bytes);
         begin
            for K in As_Wire'Range loop
               As_Wire (K) := Output (K);
            end loop;
            FA.Decode (As_Wire, Decoded, OK);
            Check (OK, "the places decode as File_Access entries");
         end;
      end if;
      for N in 1 .. Written loop
         declare
            Base : constant Positive := (N - 1) * FA.Wire_Entry_Bytes + 1;
            Count : constant Natural := Natural (Output (Base + FP.Prefix_Length_At));
         begin
            for K in 1 .. Count loop
               Length := Length + 1;
               Result (Length) := Character'Val (Output (Base + FP.Prefix_At + K - 1));
            end loop;
            Length := Length + 1;
            Result (Length) := '|';
         end;
      end loop;
      return Result (1 .. Length);
   end Places_Of;

   procedure Test_Selection is
      Both : constant FP.Root_Table := [FP.Make_Root ("nvme:0"), FP.Make_Root ("mem:0")];
      Memory_Only : constant FP.Root_Table := [1 => FP.Make_Root ("mem:0")];
      NVMe_Only : constant FP.Root_Table := [1 => FP.Make_Root ("nvme:0")];
      None : constant FP.Root_Table (1 .. 0) := [others => <>];
      Manifest : constant Scope_List := [S ("@nvme:0/"), S ("@mem:0/")];
   begin
      Check (FP.Make_Root ("mem:0").Text (1 .. FP.Make_Root ("mem:0").Length) = "@mem:0/", "a root is @name/");
      Check (Places_Of (Manifest, Both) = "@nvme:0/|@mem:0/|", "both present: both, in the manifest's order");
      Check (Places_Of (Manifest, Memory_Only) = "@mem:0/|",
             "absent volume not listed (live image, no NVMe driver): " & Places_Of (Manifest, Memory_Only));
      Check (Places_Of (Manifest, NVMe_Only) = "@nvme:0/|", "installed system without a live workspace");
      Check (Places_Of (Manifest, None) = "", "no volume present: no places");
      Check (Places_Of ([S ("@mem:0/work/"), S ("@mem:0"), S ("@mem:00/"), S ("@memo:0/")], Memory_Only)
             = "@mem:0/work/|@mem:0|", "a scope below a root, and the root without its slash; other names not");
      Check (Places_Of ([S ("apps/"), S ("@nvme:0/")], Both) = "@nvme:0/|", "unqualified scopes are no places");
      Check (Places_Of ([1 => S ("", ALL_RIGHTS)], Both) = "@nvme:0/|@mem:0/|",
             "the bootstrap wildcard lists every present root");
      Check (Places_Of (Manifest, Both, Room => 1) = "@nvme:0/|", "a range that holds fewer gets whole entries");
      declare
         Wire : FA.Wire_Bytes (1 .. FA.Maximum_Entries * FA.Wire_Entry_Bytes);
      begin
         Encode ([1 => S ("", ALL_RIGHTS)], Wire);
         Check (FP.Count_Places (Wire, 1, Both) = 2, "the count includes the wildcard's roots");
         Encode (Manifest, Wire);
         Check (FP.Count_Places (Wire, 2, Memory_Only) = 1, "the count leaves absent volumes out");
      end;
   end Test_Selection;

   procedure Test_Captions is
      use Files_Places;
   begin
      Check (Caption ("@nvme:0/") = "NVMe volume 0", "NVMe caption");
      Check (Caption ("@mem:0/") = "Live workspace", "live workspace caption");
      Check (Caption ("@mem:1/") = "Live workspace 1", "a second memory volume");
      Check (Caption ("@ata:0/") = "ATA disk 0", "ATA caption");
      Check (Caption ("@cd:0/") = "Optical disc", "optical caption");
      Check (Caption ("@boot/") = "Boot image", "boot archive caption");
      Check (Caption ("@nvme:0/home/doc/") = "@nvme:0/home/doc/", "a scope below a root keeps its path");
      Check (Caption ("@host:0/") = "@host:0/", "an unknown volume keeps its root");
      Check (Kind_Of ("@nvme:0/") = NVMe_Volume and then Kind_Of ("@mem:0/") = Live_Workspace
             and then Kind_Of ("apps/") = Other_Volume, "kinds by volume name");
   end Test_Captions;

   function Has_Digit (Text : String) return Boolean is (for some C of Text => C in '0' .. '9');

   procedure Test_Status is
      --  The label each kind comes from (Unexpected: none).
      CODES : constant array (Status_Kind range Succeeded .. Volume_Unavailable) of Unsigned_32 :=
        [Succeeded => FS.REPLY_OK, Refused => FS.REPLY_ERR, No_Space => FS.REPLY_NO_SPACE,
         Read_Only => FS.REPLY_READ_ONLY, Out_Of_Range => FS.REPLY_OUT_OF_RANGE,
         Layout_Unsupported => FS.REPLY_FILE_RANGE_UNSUPPORTED, Device_Error => FS.REPLY_IO_ERROR,
         Not_Granted => FS.REPLY_ACCESS_DENIED, Damaged_Filesystem => FS.REPLY_MALFORMED_FILESYSTEM,
         Wrong_Kind => FS.REPLY_WRONG_OBJECT_TYPE, Already_Exists => FS.REPLY_ALREADY_EXISTS,
         Not_Found => FS.REPLY_NOT_FOUND, Recovery_Required => FS.REPLY_RECOVERY_REQUIRED,
         Durability_Unsupported => FS.REPLY_DURABILITY_UNSUPPORTED,
         Unsupported_Object => FS.REPLY_UNSUPPORTED_OBJECT, In_Use => FS.REPLY_SHARING_VIOLATION,
         Not_Empty => FS.REPLY_NOT_EMPTY, Is_Folder => FS.REPLY_IS_DIRECTORY,
         Invalid_Move => FS.REPLY_INVALID_MOVE, Cross_Volume => FS.REPLY_CROSS_VOLUME,
         Cancelled => FS.REPLY_CANCELLED, Deadline_Passed => FS.REPLY_DEADLINE, Busy => FS.REPLY_BUSY,
         Volume_Unavailable => FS.REPLY_VOLUME_UNAVAILABLE];
   begin
      for K in Status_Kind loop
         declare
            Sentence : constant String := Listing_Text (K);
            Clause : constant String := Reason (K);
         begin
            Check (Sentence'Length > 1 and then Sentence (Sentence'Last) = '.'
                   and then Sentence (Sentence'First) in 'A' .. 'Z', Status_Kind'Image (K) & ": a sentence");
            Check (not Has_Digit (Sentence) and then not Has_Digit (Clause),
                   Status_Kind'Image (K) & ": no number reaches the person");
            Check (Clause'Length > 0 and then Clause (Clause'Last) /= '.', Status_Kind'Image (K) & ": a clause");
            if K /= Unexpected then
               Check (Kind_Of (CODES (K)) = K, Status_Kind'Image (K) & ": its label maps to it");
            end if;
         end;
      end loop;
      --  Every label the service sends has words of its own.
      for Code in FS.REPLY_OK .. FS.REPLY_VOLUME_UNAVAILABLE loop
         Check (Kind_Of (Code) /= Unexpected, "label" & Unsigned_32'Image (Code) & " is known");
      end loop;
      Check (Kind_Of (FS.REPLY_VOLUME_UNAVAILABLE + 1) = Unexpected and then Kind_Of (0) = Unexpected,
             "an unknown label is Unexpected");
      Check (Listing_Text (Kind_Of (FS.REPLY_VOLUME_UNAVAILABLE)) = "This volume is not available.",
             "an absent volume: " & Listing_Text (Kind_Of (FS.REPLY_VOLUME_UNAVAILABLE)));
      Check (Listing_Text (Kind_Of (FS.REPLY_ACCESS_DENIED)) = "Files is not granted this folder.", "denied");
      Check (Listing_Text (Kind_Of (FS.REPLY_NOT_FOUND)) = "This folder does not exist.", "gone");
      Check (Listing_Kind (FS.REPLY_NOT_FOUND, At_Root => True) = Volume_Unavailable
             and then Listing_Kind (FS.REPLY_NOT_FOUND, At_Root => False) = Not_Found,
             "a place's root that is not found is a volume that is not");
      Check ((for all K in Status_Kind range Read_Only .. Read_Only => Severity_Of (K) = Expected)
             and then Severity_Of (Volume_Unavailable) = Expected and then Severity_Of (Not_Found) = Expected
             and then Severity_Of (Not_Granted) = Expected and then Severity_Of (Cancelled) = Expected,
             "expected conditions are muted");
      Check (Severity_Of (Device_Error) = Problem and then Severity_Of (Damaged_Filesystem) = Problem
             and then Severity_Of (Unexpected) = Problem and then Severity_Of (Recovery_Required) = Problem,
             "genuine errors are red");
   end Test_Status;

   procedure Run is
   begin
      Test_Selection;
      Test_Captions;
      Test_Status;
   end Run;
end Files_Places_Tests;
