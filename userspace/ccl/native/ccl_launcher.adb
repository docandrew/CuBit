with CuBit.Child_Exits;
with CuBit.Handles.Services;
with CuBit.Launch_Arguments;
with CuBit.Launch_Failure_Text;
with CuBit.Launch_Authority;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Process_IDs;
with CuBit.Memory_Grants;
with CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Stream_Regions;
with CuBit.Streams;
with CCL_Places;

--  Programs on CuBit: the console's own launch table and each program's
--  description from procmgr (CuBit.Launching); a run lends a ring for each
--  outlet (launcher-owned outlet rings) and reads them in place, so no
--  line is lost to the child's exit.
package body CCL_Launcher is
   use Interfaces;
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package LAuth renames CuBit.Launch_Authority;
   use type CuBit.Launching.Launch_Result;
   use type PD.Connector_Direction;
   use type PD.Element_Kind;
   use type PD.Check_Result;
   use type CuBit.Child_Exits.Termination_Kind;
   use type LA.Launch_Failure;
   use type CCL_Places.Result_Kind;
   use type CCL_Places.Files.File_Kind;

   NEWLINE : constant Unsigned_8 := 10;

   --  procmgr: the program's "process-manager" startup entry. Without one
   --  every launch is refused (No_Process_Manager).
   Launcher : constant CuBit.Launching.Launcher :=
     CuBit.Launching.Using (CuBit.Handles.Services.Process_Manager.Optional_Named ("process-manager"));
   MAXIMUM_LINE : constant := 200;
   type Partial is record
      Text : String (1 .. MAXIMUM_LINE) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_LINE := 0;
   end record;
   type Base_Array is array (PD.Connector_Index) of Unsigned_64;
   type Page_Array is array (PD.Connector_Index) of Natural;
   type Reference_Array is array (PD.Connector_Index) of CuBit.Memory_Grants.Grant_Reference;
   type Partial_Array is array (PD.Connector_Index) of Partial;
   type Run_State is record
      Active : Boolean := False;
      Child : CuBit.Launching.Child;
      Connector_Total : PD.Connector_Count := 0;
      Bases : Base_Array := [others => 0];      --  0: no ring for the outlet
      Pages : Page_Array := [others => 0];
      References : Reference_Array := [others => (others => <>)];
      Lines : Partial_Array;
      Exited : Boolean := False;
      How : Ending;
   end record;
   Runs : array (Run_Index) of Run_State;

   --  Rings of released runs: their grants revoked, their pages reused for
   --  a later run once the kernel confirms the grant retired (no mapping
   --  of them remains).
   MAXIMUM_SPARE : constant := 64;
   type Spare is record
      Base : Unsigned_64 := 0;          --  0: empty
      Pages : Natural := 0;
      Reference : CuBit.Memory_Grants.Grant_Reference;
   end record;
   Spares : array (1 .. MAXIMUM_SPARE) of Spare;

   --  Pages for a ring of at least Pages, from the spares: 0 when none fit.
   function Take_Spare (Pages : Positive) return Unsigned_64 is
   begin
      for S of Spares loop
         if S.Base /= 0 and then S.Pages >= Pages
           and then CuBit.Memory_Grants.Retirement_Confirmed (S.Reference)
         then
            return Base : constant Unsigned_64 := S.Base do
               S := (others => <>);
            end return;
         end if;
      end loop;
      return 0;
   end Take_Spare;

   procedure Set_Why (Item : in out Program; Text : String) is
      Kept : constant Natural := Natural'Min (Text'Length, MAXIMUM_WHY);
   begin
      Item.Why := [others => ' '];
      Item.Why (1 .. Kept) := Text (Text'First .. Text'First + Kept - 1);
      Item.Why_Length := Kept;
   end Set_Why;

   --  Item for Name as procmgr describes it to this process: OP_DESCRIBE
   --  runs the launch's own admission (the table, and for a staged
   --  program a read scope over it and the Desktop's switch) before it
   --  reads anything.
   procedure Describe_Into (Name : String; Staged : Boolean; Item : out Program)
   with Pre => Name'Length in 1 .. MAXIMUM_NAME
   is
      Descriptor : PD.Bytes (1 .. PD.Maximum_Descriptor_Bytes);
      Described : PD.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Decoded : Boolean;
   begin
      Item := (others => <>);
      Item.Name (1 .. Name'Length) := Name;
      Item.Length := Name'Length;
      Item.Staged := Staged;
      CuBit.Launching.Describe (Launcher, Name, Descriptor, Described, Result, Failure);
      if Result /= CuBit.Launching.Launched then
         Item.State := Refused;
         Set_Why (Item,
           (if Result = CuBit.Launching.No_Process_Manager
            then "this process holds no process-manager grant"
            elsif Failure = LA.Not_Granted and then Staged
            then "procmgr does not admit this staged program: it needs a read scope over it " &
                 "and the Desktop's Allow app replacement switch on"
            elsif Failure = LA.Not_Granted
            then "procmgr does not admit it: the launch table does not name it"
            else CuBit.Launch_Failure_Text.Explanation (Failure)));
      elsif Described = 0 then
         Item.State := Not_Described;
         Set_Why (Item, "it declares no parameters, inlets or outlets (no program description)");
      else
         PD.Decode (Descriptor (1 .. Described), Item.Description, Decoded);
         if not Decoded then
            Item.State := Not_Described;
            Set_Why (Item, "its program description is malformed");
         end if;
      end if;
   end Describe_Into;

   --  The launch table procmgr holds for this process (Valid False: none).
   procedure Read_Table
     (Table : out LAuth.Table_Bytes; Length : out LAuth.Table_Length; Valid : out Boolean)
   with Pre => Table'First = 1 and then Table'Length = LAuth.Maximum_Table_Bytes
   is
      Answered : CuBit.Launching.Launch_Result;
   begin
      CuBit.Launching.Launch_Table (Launcher, Table, Length, Answered);
      Valid := Answered = CuBit.Launching.Launched and then Length > 0
        and then LAuth.Valid (Table (1 .. Length));
   end Read_Table;

   function Ends_With (Text, Suffix : String) return Boolean is
     (Text'Length >= Suffix'Length
      and then Text (Text'Last - Suffix'Length + 1 .. Text'Last) = Suffix);

   PROGRAM_SUFFIX : constant String := ".app";

   --  One directory listing at a time: too large for the stack.
   Listing : CCL_Places.Listing;

   procedure Programs (Items : out Program_Array; Count : out Program_Count; More : out Boolean) is
      Table : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes);
      Length : LAuth.Table_Length;
      Valid : Boolean;

      --  Directories still to walk, breadth first, with their depth below
      --  the staging directory.
      subtype Directory_Count is Natural range 0 .. MAXIMUM_STAGING_DIRECTORIES;
      type Pending is record
         Path : String (1 .. MAXIMUM_NAME) := [others => ' '];
         Length : Name_Length := 0;
         Depth : Natural range 0 .. MAXIMUM_STAGING_DEPTH := 0;
      end record;
      Queue : array (1 .. MAXIMUM_STAGING_DIRECTORIES) of Pending;
      Queued, Walked : Directory_Count := 0;

      procedure Add (Item : Program) is
      begin
         if Count = MAXIMUM_PROGRAMS then
            More := True;
         else
            Count := Count + 1;
            Items (Count) := Item;
         end if;
      end Add;

      procedure Push (Path : String; Depth : Natural) is
      begin
         if Path'Length > MAXIMUM_NAME or else Depth > MAXIMUM_STAGING_DEPTH
           or else Queued = MAXIMUM_STAGING_DIRECTORIES
         then
            More := True;
         else
            Queued := Queued + 1;
            Queue (Queued).Path (1 .. Path'Length) := Path;
            Queue (Queued).Length := Path'Length;
            Queue (Queued).Depth := Depth;
         end if;
      end Push;
   begin
      Items := [others => (others => <>)];
      Count := 0;
      More := False;
      Read_Table (Table, Length, Valid);
      if not Valid then
         return;
      end if;
      for Index in 1 .. LAuth.Maximum_Names loop
         declare
            First : Positive;
            Last : Natural;
            Found : Boolean;
         begin
            LAuth.Name_At (Table (1 .. Length), Index, First, Last, Found);
            exit when not Found;
            if Last - First + 1 in 1 .. MAXIMUM_NAME then
               declare
                  Name : String (1 .. Last - First + 1);
                  Item : Program;
               begin
                  for K in Name'Range loop
                     Name (K) := Character'Val (Table (First + K - 1));
                  end loop;
                  if Name (Name'Last) = LAuth.Staging_Mark then
                     --  A staging directory: walked below, its programs by
                     --  their full names.
                     if Name'Length > 1 then
                        Push (Name (Name'First .. Name'Last - 1), 0);
                     end if;
                  else
                     Describe_Into (Name, False, Item);
                     Add (Item);
                  end if;
               end;
            end if;
         end;
      end loop;
      while Walked < Queued loop
         Walked := Walked + 1;
         declare
            Directory : constant Pending := Queue (Walked);
            Path : constant String := Directory.Path (1 .. Directory.Length);
            Listed : CCL_Places.Listed_Count;
            Total : Natural;
            Result : CCL_Places.Result_Kind;
         begin
            CCL_Places.List (Path, Listing, Listed, Total, Result);
            --  Not_Found: nothing staged yet. Access_Denied: the manifest
            --  names the directory but holds no read scope over it, so
            --  procmgr would refuse its programs anyway.
            if Result = CCL_Places.Listed_All then
               More := More or else Total > Listed;
               for E in 1 .. Listed loop
                  declare
                     Child : CCL_Places.Listed renames Listing (E);
                     Child_Name : constant String := Child.Name (1 .. Child.Name_Length);
                     Full : constant String := Path & "/" & Child_Name;
                     Item : Program;
                  begin
                     if Child_Name'Length = 0 or else Child_Name = "." or else Child_Name = ".." then
                        null;
                     elsif Child.Kind = CCL_Places.Files.Directory then
                        if Directory.Depth < MAXIMUM_STAGING_DEPTH then
                           Push (Full, Directory.Depth + 1);
                        end if;
                     elsif Child.Kind = CCL_Places.Files.File
                       and then Ends_With (Child_Name, PROGRAM_SUFFIX)
                     then
                        if Full'Length <= MAXIMUM_NAME then
                           Describe_Into (Full, True, Item);
                           Add (Item);
                        else
                           More := True;
                        end if;
                     end if;
                  end;
               end loop;
            elsif Result /= CCL_Places.Not_Found then
               CuBit.Messages.debugPrint
                 ("ccl-launcher: staging directory " & Path & " not listed (" & Result'Image & ")" &
                  ASCII.LF);
            end if;
         end;
      end loop;
      if More then
         CuBit.Messages.debugPrint
           ("ccl-launcher: more programs than" & MAXIMUM_PROGRAMS'Image & ", or a staging directory " &
            "past the walk's ceilings (depth" & MAXIMUM_STAGING_DEPTH'Image & "," &
            MAXIMUM_STAGING_DIRECTORIES'Image & " directories): the rest are not offered" & ASCII.LF);
      end if;
   end Programs;

   procedure Check (Name : String; Item : out Program) is
      Table : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes);
      Length : LAuth.Table_Length;
      Valid : Boolean;
      Named, Staged : Boolean := False;
   begin
      Item := (others => <>);
      if Name'Length not in 1 .. MAXIMUM_NAME then
         Item.State := Refused;
         Set_Why (Item, "that is not a program name (1 to" & MAXIMUM_NAME'Image & " bytes)");
         return;
      end if;
      Read_Table (Table, Length, Valid);
      if Valid then
         Named := LAuth.Contains (Table (1 .. Length), Name);
         Staged := not Named and then LAuth.Admits_Staged (Table (1 .. Length), Name);
      end if;
      --  procmgr decides either way; only the sentence is this process's.
      Describe_Into (Name, Staged, Item);
      if Item.State = Refused and then not Named and then not Staged then
         Set_Why (Item,
           (if not Valid then "this process has no launch table"
            else "it is neither named by this process's launch table nor beneath one of its " &
                 "staging directories"));
      end if;
      if Item.State = Refused then
         CuBit.Messages.debugPrint
           ("ccl-launcher: " & Name & " refused: " & Item.Why (1 .. Item.Why_Length) & ASCII.LF);
      end if;
   end Check;

   procedure Say (Why : out String; Why_Length : out Natural; Text : String) is
      Kept : constant Natural := Natural'Min (Text'Length, Why'Length);
   begin
      Why := [others => ' '];
      Why (Why'First .. Why'First + Kept - 1) := Text (Text'First .. Text'First + Kept - 1);
      Why_Length := Kept;
   end Say;

   procedure Start
     (Name : String; Description : PD.Signature; V : PD.Values;
      Started_As : out Run; Result : out Start_Result;
      Why : out String; Why_Length : out Natural)
   is
      Block : LA.Builder;
      Grants : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Checked : PD.Check_Result;
      Rings : CuBit.Outlet_Rings.Table;
      Free : Natural := 0;
      Launched : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      State : Run_State;
   begin
      Started_As := (others => <>);
      Say (Why, Why_Length, "");
      for R in Runs'Range loop
         if not Runs (R).Active then
            Free := R;
            exit;
         end if;
      end loop;
      if Free = 0 then
         Result := Too_Many_Runs;
         Say (Why, Why_Length, "this console already follows" & MAXIMUM_RUNS'Image & " programs");
         return;
      end if;
      if Name'Length not in 1 .. LA.Maximum_Name_Bytes then
         Result := Refused;
         Say (Why, Why_Length, "no such program");
         return;
      end if;
      PD.Render (Description, V, Name, Block, Grants, Checked);
      if Checked /= PD.Matches then
         Result := Refused;
         Say (Why, Why_Length, "its parameters do not render: " & Checked'Image);
         return;
      end if;
      LG.Finish (Grants, Region, Region_Length);
      --  A ring for each outlet, in this console's memory.
      State.Connector_Total := Description.Connector_Total;
      for P in 0 .. Description.Connector_Total - 1 loop
         if Description.Connectors (P).Direction = PD.Outlet
           and then Rings.Count < CuBit.Outlet_Rings.Maximum_Entries
         then
            declare
               Base : Unsigned_64 := Take_Spare (Description.Connectors (P).Pages);
               Grant : Unsigned_64;
               Reference : CuBit.Memory_Grants.Grant_Reference;
               Lent : Boolean;
            begin
               CuBit.Launching.Lend_Ring
                 (Launcher, Description.Connectors (P).Pages,
                  (if Description.Connectors (P).Element = PD.Text_Lines
                   then CuBit.Streams.TYPE_TEXT_LINE else CuBit.Streams.TYPE_RAW_BYTES),
                  Base, Grant, Reference, Lent);
               if not Lent then
                  CuBit.Messages.debugPrint
                    ("ccl-console: no ring lent for a connector (base" & Base'Image & ")" & ASCII.LF);
               end if;
               if Lent then
                  State.Bases (P) := Base;
                  State.Pages (P) := Description.Connectors (P).Pages;
                  State.References (P) := Reference;
                  Rings.Count := Rings.Count + 1;
                  Rings.Entries (Rings.Count) := (Outlet => P, Grant => Grant);
               end if;
            end;
         end if;
      end loop;
      CuBit.Launching.Launch
        (Launcher, Name, Block.Data (1 .. Block.Used), Region (1 .. Region_Length),
         State.Child, Launched, Failure, Rings);
      if Launched /= CuBit.Launching.Launched then
         Result := (if Launched = CuBit.Launching.No_Process_Manager then Not_Available else Refused);
         Say (Why, Why_Length,
              (case Failure is
                  when LA.Not_Granted => "it needs authority this console does not hold " &
                    "(a file outside its places, or a request the console lacks)",
                  when LA.Arguments_Rejected => "procmgr refused its arguments or rings",
                  when others => CuBit.Launch_Failure_Text.Explanation (Failure)));
         return;
      end if;
      State.Active := True;
      Runs (Free) := State;
      Started_As := (Index => Free, Process => CuBit.Process_IDs.To_Word (State.Child.Process));
      Result := Started;
   end Start;

   procedure Poll (Item : Run; Ended : out Boolean; How : out Ending) is
      State : Run_State renames Runs (Item.Index);
      Buffer : array (1 .. 4096) of Unsigned_8;
      Has_Ended : Boolean := False;
      Report : CuBit.Child_Exits.Report;

      procedure Drain (P : PD.Connector_Index) is
         Read : Natural;
         L : Partial renames State.Lines (P);
      begin
         loop
            Read := CuBit.Stream_Regions.Read_Owned
              (State.Bases (P), State.Pages (P), Buffer'Address, Buffer'Length);
            exit when Read = 0;
            for K in 1 .. Read loop
               if Buffer (K) = NEWLINE then
                  Deliver (P, L.Text (1 .. L.Length));
                  L.Length := 0;
               elsif L.Length < MAXIMUM_LINE then
                  L.Length := L.Length + 1;
                  L.Text (L.Length) := Character'Val (Buffer (K));
               end if;
            end loop;
         end loop;
      end Drain;
   begin
      Ended := False;
      How := (others => <>);
      if not State.Active or else CuBit.Process_IDs.To_Word (State.Child.Process) /= Item.Process then
         Ended := True;
         return;
      end if;
      if not State.Exited then
         CuBit.Launching.Poll_Exit (State.Child, Has_Ended, Report);
         if Has_Ended then
            State.Exited := True;
            State.How := (if Report.Kind = CuBit.Child_Exits.Exited
                          then (Kind => Exited, Code => Exit_Code (Report.Code))
                          else (Kind => Stopped, Code => 0));
         end if;
      end if;
      --  The rings outlive the child: drain them once more after its exit.
      for P in 0 .. State.Connector_Total - 1 loop
         if State.Bases (P) /= 0 then
            Drain (P);
         end if;
      end loop;
      if State.Exited then
         for P in 0 .. State.Connector_Total - 1 loop
            if State.Lines (P).Length > 0 then
               Deliver (P, State.Lines (P).Text (1 .. State.Lines (P).Length));
               State.Lines (P).Length := 0;
            end if;
         end loop;
         Ended := True;
         How := State.How;
      end if;
   end Poll;

   procedure Release (Item : Run) is
      State : Run_State renames Runs (Item.Index);
      Revoked : Boolean;
   begin
      if CuBit.Process_IDs.To_Word (State.Child.Process) /= Item.Process then
         return;
      end if;
      --  Revoke each ring's grant and keep its pages for a later run. When
      --  the spare table is full the pages are not reused; the grant is
      --  still revoked.
      for P in 0 .. State.Connector_Total - 1 loop
         if State.Bases (P) /= 0 then
            CuBit.Memory_Grants.Revoke (State.References (P), Revoked);
            for S of Spares loop
               if S.Base = 0 then
                  S := (Base => State.Bases (P), Pages => State.Pages (P),
                        Reference => State.References (P));
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      State := (others => <>);
   end Release;
end CCL_Launcher;
