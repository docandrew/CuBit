with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Launch_Authority;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Memory_Grants;
with CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Stream_Regions;
with CuBit.Streams;

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

   NEWLINE : constant Unsigned_8 := 10;
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

   procedure Programs (Items : out Program_Array; Count : out Program_Count) is
      Table : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes);
      Length : LAuth.Table_Length;
      Answered : CuBit.Launching.Launch_Result;
   begin
      Items := [others => (others => <>)];
      Count := 0;
      CuBit.Launching.Launch_Table (Table, Length, Answered);
      if Answered /= CuBit.Launching.Launched or else Length = 0
        or else not LAuth.Valid (Table (1 .. Length))
      then
         return;
      end if;
      for Index in 1 .. LAuth.Maximum_Names loop
         exit when Count = MAXIMUM_PROGRAMS;
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
                  Descriptor : PD.Bytes (1 .. PD.Maximum_Descriptor_Bytes);
                  Described : PD.Descriptor_Length;
                  Result : CuBit.Launching.Launch_Result;
                  Failure : LA.Launch_Failure;
                  Decoded : Boolean;
                  Item : Program;
               begin
                  for K in Name'Range loop
                     Name (K) := Character'Val (Table (First + K - 1));
                  end loop;
                  CuBit.Launching.Describe (Name, Descriptor, Described, Result, Failure);
                  if Result = CuBit.Launching.Launched and then Described > 0 then
                     PD.Decode (Descriptor (1 .. Described), Item.Description, Decoded);
                     if Decoded then
                        Item.Name (1 .. Name'Length) := Name;
                        Item.Length := Name'Length;
                        Count := Count + 1;
                        Items (Count) := Item;
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Programs;

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
                 (Description.Connectors (P).Pages,
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
        (Name, Block.Data (1 .. Block.Used), Region (1 .. Region_Length),
         State.Child, Launched, Failure, Rings);
      if Launched /= CuBit.Launching.Launched then
         Result := (if Launched = CuBit.Launching.No_Process_Manager then Not_Available else Refused);
         Say (Why, Why_Length,
              (case Failure is
                  when LA.Not_Granted => "it needs authority this console does not hold " &
                    "(a file outside its places, or a request the console lacks)",
                  when LA.Spawn_Failed => "it could not be read or started",
                  when LA.Arguments_Rejected => "procmgr refused its arguments or rings",
                  when others => "procmgr refused the launch: " & Failure'Image));
         return;
      end if;
      State.Active := True;
      Runs (Free) := State;
      Started_As := (Index => Free, Process => State.Child.Process,
                     Generation => State.Child.Generation);
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
      if not State.Active or else State.Child.Process /= Item.Process
        or else State.Child.Generation /= Item.Generation
      then
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
      if State.Child.Process /= Item.Process or else State.Child.Generation /= Item.Generation then
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
