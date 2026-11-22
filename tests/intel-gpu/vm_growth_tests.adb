with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package Growth is new VM.Growth;
   use type Growth.Plan_Status;
   use type Growth.Inspection_Status;
   use type Growth.Requirements;
   use type Growth.Description_Status;
   use type Growth.Node_List;
   Source : VM.Image;
   DMA : VM.Backing_Pages;
   OK : Boolean;
   Starts : constant array (1 .. 7) of Unsigned_64 :=
     [8192, 2 ** 21 - 4096, 2 ** 21, 2 ** 30 - 4096,
      2 ** 39 - 4096, 2 ** 39, 2 ** 48 - 8192];
   Lengths : constant array (1 .. 7) of Positive := [1, 2, 511, 512, 513, 514, 4096];
   Levels : constant array (1 .. 3) of Natural := [21, 30, 39];
   Cases : Natural := 0;
begin
   for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
   VM.Initialize (Source, DMA, OK); pragma Assert (OK);
   VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
   VM.Seal (Source, OK); pragma Assert (OK);
   pragma Assert (Growth.Inspect (Source, 4096, 4096).Status = Growth.Occupied);
   pragma Assert (Growth.Inspect (Source, 0, 4096).Status = Growth.Invalid_Range);
   pragma Assert (Growth.Inspect (Source, 8192, 0).Status = Growth.Invalid_Range);
   pragma Assert (Growth.Inspect (Source, 8193, 4096).Status = Growth.Invalid_Range);
   declare
      Query : Growth.Inspection;
      Other : VM.Image;
      Other_DMA : VM.Backing_Pages := DMA;
      Turns : Natural := 0;
   begin
      Growth.Start_Inspection (Query, Source, 8192, 510 * 4096, OK);
      pragma Assert (OK);
      pragma Assert (Growth.Inspection_Result (Query, Source).Status = Growth.Invalid_Range);
      Growth.Step_Inspection (Query, Source);
      pragma Assert (Growth.Inspection_State (Query) = Growth.Scanning);
      -- An overlapping start cannot discard the active request.
      Growth.Start_Inspection (Query, Source, 4096, 4096, OK);
      pragma Assert (not OK);
      Turns := 1;
      while Growth.Inspection_State (Query) = Growth.Scanning loop
         Growth.Step_Inspection (Query, Source); Turns := Turns + 1;
         pragma Assert (Turns <= 20);
      end loop;
      pragma Assert (Turns >= 16);
      pragma Assert (Growth.Inspection_Result (Query, Source) =
                     Growth.Inspect (Source, 8192, 510 * 4096));
      for P in Other_DMA'Range loop Other_DMA (P) := Other_DMA (P) + 16#400000#; end loop;
      VM.Initialize (Other, Other_DMA, OK); pragma Assert (OK);
      VM.Map_Page (Other, 4096, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Other, OK); pragma Assert (OK);
      pragma Assert (Growth.Inspection_Result (Query, Other).Status = Growth.Invalid_Range);
      Growth.Start_Inspection (Query, Source, 8192, 510 * 4096, OK);
      pragma Assert (OK);
      Growth.Step_Inspection (Query, Other);
      pragma Assert (Growth.Inspection_State (Query) = Growth.Stale);
      Growth.Step_Inspection (Query, Source);
      pragma Assert (Growth.Inspection_State (Query) = Growth.Stale);
      Growth.Start_Inspection (Query, Source, 8192, 510 * 4096, OK);
      pragma Assert (OK);
      Growth.Cancel_Inspection (Query);
      Growth.Step_Inspection (Query, Source);
      pragma Assert (Growth.Inspection_State (Query) = Growth.Stale);
      pragma Assert (Growth.Inspection_Result (Query, Source).Status = Growth.Invalid_Range);
      Growth.Start_Inspection (Query, Source, 2 ** 39, 4096, OK);
      pragma Assert (OK);
      Growth.Step_Inspection (Query, Source);
      pragma Assert (Growth.Inspection_State (Query) = Growth.Complete);
      pragma Assert (Growth.Inspection_Result (Query, Source).Additional_Tables = 3);
   end;
   declare
      Query : Growth.Inspection;
      Mutable : VM.Image;
   begin
      VM.Initialize (Mutable, DMA, OK); pragma Assert (OK);
      Growth.Start_Inspection (Query, Mutable, 8192, 4096, OK);
      pragma Assert (not OK);
      VM.Map_Page (Mutable, 8192, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Growth.Start_Inspection (Query, Mutable, 12288, 4096, OK);
      pragma Assert (not OK);
      VM.Seal (Mutable, OK); pragma Assert (OK);
      Growth.Start_Inspection (Query, Mutable, 12288, 4096, OK);
      pragma Assert (OK);
   end;
   for First of Starts loop
      for Count of Lengths loop
         declare
            Bytes : constant Unsigned_64 := Unsigned_64 (Count) * 4096;
            Plan : constant Growth.Requirements := Growth.Inspect (Source, First, Bytes);
            Needed : Natural := 0;
            Mirror : VM.Image;
            Data : VM.Data_Pages (1 .. Count) := [others => 16#200000#];
            Nodes : Growth.Node_List (5 .. 36);
            Streamed : Growth.Node_List (5 .. 36) := [others => (others => <>)];
            Written : Natural;
            Planned : Boolean;
            Description : Growth.Description;
            Emissions : Natural := 0;
            function Authorized return Boolean is (True);
            procedure Emit (Ordinal : Positive; Item : Growth.Node; Accepted : out Boolean) is
            begin
               Streamed (Streamed'First + (Ordinal - 1)) := Item;
               Emissions := Emissions + 1;
               Accepted := True;
            end Emit;
            procedure Describe_Stream is new Growth.Describe_Into (Authorized, Emit);
            procedure Describe_Step is new Growth.Step_Description (Authorized, Emit);
         begin
            if Bytes > 2 ** 48 - First then
               pragma Assert (Plan.Status = Growth.Invalid_Range);
            else
               -- Independent interval oracle: baseline has exactly prefix0
               -- at each level. Count nonzero prefix intervals intersected.
               for Bits of Levels loop
                  declare
                     Low : constant Unsigned_64 := First / 2 ** Bits;
                     High : constant Unsigned_64 := (First + Bytes - 1) / 2 ** Bits;
                  begin
                     Needed := Needed + Natural (High - Low + 1) - (if Low = 0 then 1 else 0);
                  end;
               end loop;
               pragma Assert (Plan.Status = Growth.Ready and Plan.Additional_Tables = Needed);
               pragma Assert (Plan.Fits_Reserved = (Needed <= 4));
               Growth.Describe (Source, First, Bytes, Nodes, Written, Planned);
               pragma Assert (Planned and Written = Needed);
               Describe_Stream (Source, First, Bytes, Streamed'Length, Written, Planned);
               pragma Assert (Planned and Written = Needed and Streamed = Nodes);
               Streamed := [others => (others => <>)]; Emissions := 0;
               Growth.Start_Description (Description, Source, First, Bytes, Streamed'Length, Planned);
               pragma Assert (Planned);
               Growth.Start_Description (Description, Source, First, Bytes, 0, Planned);
               pragma Assert (not Planned); -- cannot replace an active plan
               while Growth.Description_State (Description) in Growth.Checking | Growth.Emitting loop
                  declare
                     Before : constant Natural := Emissions;
                     Checking : constant Boolean := Growth.Description_State (Description) = Growth.Checking;
                  begin
                     Describe_Step (Description, Source);
                     pragma Assert (Emissions - Before <= 32);
                     if Checking then pragma Assert (Emissions = Before); end if;
                  end;
               end loop;
               pragma Assert (Growth.Description_Valid (Description, Source));
               pragma Assert (Growth.Description_Count (Description, Source) = Needed);
               pragma Assert (Emissions = Needed and Streamed = Nodes);
               declare
                  type Entries is array (Table_Index) of Natural;
                  Tree : array (1 .. 36) of Entries := [others => [others => 0]];
                  Depths : array (1 .. 36) of Natural := [others => 0];
                  Parent, Cursor : Natural;
               begin
                  Tree (1) (0) := 2; Tree (2) (0) := 3; Tree (3) (0) := 4;
                  Depths (1 .. 4) := [3, 2, 1, 0];
                  for N in 1 .. Written loop
                     declare Item : Growth.Node renames Nodes (Nodes'First + N - 1); begin
                        pragma Assert ((Item.Existing_Parent /= 0) /= (Item.New_Parent /= 0));
                        pragma Assert (Item.Existing_Parent <= 4 and Item.New_Parent < N);
                        Parent := (if Item.Existing_Parent /= 0 then Item.Existing_Parent else 4 + Item.New_Parent);
                        pragma Assert (Tree (Parent) (Item.Index) = 0);
                        pragma Assert (Depths (Parent) = Natural (Item.Level) + 1);
                        Tree (Parent) (Item.Index) := 4 + N;
                        Depths (4 + N) := Natural (Item.Level);
                     end;
                  end loop;
                  -- Replay topology using IDs, not assumed contiguous DMA.
                  for Page in 0 .. Count - 1 loop
                     declare
                        W : constant Walk := Locate (First + Unsigned_64 (Page) * 4096);
                        Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
                     begin
                        Cursor := 1;
                        for Index of Route loop
                           Cursor := Tree (Cursor) (Index);
                           pragma Assert (Cursor /= 0);
                        end loop;
                        pragma Assert (Depths (Cursor) = 0);
                     end;
                  end loop;
               end;
               if Needed > 0 then
                  declare Small : Growth.Node_List (1 .. Needed - 1); begin
                     Growth.Describe (Source, First, Bytes, Small, Written, Planned);
                     pragma Assert (not Planned and Written = 0);
                     for Item of Small loop
                        pragma Assert (Item.Existing_Parent = 0 and Item.New_Parent = 0);
                     end loop;
                  end;
               end if;
               VM.Initialize (Mirror, DMA, OK); pragma Assert (OK);
               VM.Map_Page (Mirror, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
               VM.Map_Pages (Mirror, First, Data, Write_Back, Read_Write, OK);
               pragma Assert (OK = Plan.Fits_Reserved);
               pragma Assert (VM.Used (Mirror) = (if OK then 4 + Needed else 4));
            end if;
            pragma Assert (VM.Used (Source) = 4 and VM.Lookup (Source, 4096) = 16#100003#);
            Cases := Cases + 1;
         end;
      end loop;
   end loop;
   for Position in 0 .. 511 loop
      declare
         Occupied : VM.Image;
         First : constant Unsigned_64 := 2 ** 21;
         Target : constant Unsigned_64 := First + Unsigned_64 (Position) * 4096;
         Nodes : Growth.Node_List (1 .. 8);
         Written : Natural;
      begin
         VM.Initialize (Occupied, DMA, OK); pragma Assert (OK);
         VM.Map_Page (Occupied, Target, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Occupied, OK); pragma Assert (OK);
         -- Every possible occupied word in an otherwise empty leaf table.
         pragma Assert (Growth.Inspect (Occupied, First, 2 ** 21).Status = Growth.Occupied);
         -- Begin in a missing previous table; the final requested word is occupied.
         pragma Assert (Growth.Inspect (Occupied, First - 4096,
           Unsigned_64 (Position + 2) * 4096).Status = Growth.Occupied);
         if Position > 0 then
            pragma Assert (Growth.Inspect (Occupied, First,
              Unsigned_64 (Position) * 4096).Status = Growth.Ready);
         end if;
         if Position < 511 then
            pragma Assert (Growth.Inspect (Occupied, Target + 4096,
              Unsigned_64 (511 - Position) * 4096).Status = Growth.Ready);
         end if;
         Growth.Describe (Occupied, First, 2 ** 21, Nodes, Written, OK);
         pragma Assert (not OK and Written = 0);
         for Node of Nodes loop
            pragma Assert (Node.Existing_Parent = 0 and Node.New_Parent = 0);
         end loop;
      end;
   end loop;
   for Fault in 0 .. 3 loop
      declare
         State : Growth.Description;
         Held : Boolean := Fault /= 0;
         Calls : Natural := 0;
         function Authorized return Boolean is (Held);
         procedure Emit (Ordinal : Positive; Item : Growth.Node; Accepted : out Boolean) is
            pragma Unreferenced (Ordinal, Item);
         begin
            Calls := Calls + 1;
            Accepted := Fault /= 1;
            if Fault = 2 then Held := False; end if;
            if Fault = 3 then Growth.Cancel_Description (State); end if;
         end Emit;
         procedure Step is new Growth.Step_Description (Authorized, Emit);
      begin
         Growth.Start_Description (State, Source, 2 ** 39, 4096, 3, OK);
         pragma Assert (OK);
         while Growth.Description_State (State) in Growth.Checking | Growth.Emitting loop
            Step (State, Source);
         end loop;
         pragma Assert (Growth.Description_State (State) = Growth.Rejected);
         pragma Assert (not Growth.Description_Valid (State, Source));
         pragma Assert (Growth.Description_Count (State, Source) = 0);
         pragma Assert (Calls = (if Fault = 0 then 0 else 1));
         Held := True;
         Step (State, Source);
         pragma Assert (Calls = (if Fault = 0 then 0 else 1));
      end;
   end loop;
   declare
      package Large_VM is new Intel_GPU_VM_Image (128);
      package Large_Growth is new Large_VM.Growth;
      use type Large_Growth.Description_Status;
      Image : Large_VM.Image;
      Pages : Large_VM.Backing_Pages;
      State : Large_Growth.Description;
      Calls, Steps : Natural := 0;
      function Authorized return Boolean is (True);
      procedure Emit (Ordinal : Positive; Item : Large_Growth.Node; Accepted : out Boolean) is
         pragma Unreferenced (Item);
      begin
         Calls := Calls + 1; pragma Assert (Ordinal = Calls); Accepted := True;
      end Emit;
      procedure Step is new Large_Growth.Step_Description (Authorized, Emit);
   begin
      for P in Pages'Range loop Pages (P) := Unsigned_64 (P) * 4096; end loop;
      Large_VM.Initialize (Image, Pages, OK); pragma Assert (OK);
      Large_VM.Map_Page (Image, 4096, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      Large_VM.Seal (Image, OK); pragma Assert (OK);
      Large_Growth.Start_Description (State, Image, 2 ** 39, 64 * 2 ** 21, 66, OK);
      pragma Assert (OK);
      while Large_Growth.Description_State (State) in Large_Growth.Checking | Large_Growth.Emitting loop
         declare Before : constant Natural := Calls; begin
            Step (State, Image); Steps := Steps + 1;
            pragma Assert (Calls - Before <= 32 and Steps < 100);
         end;
      end loop;
      pragma Assert (Large_Growth.Description_Valid (State, Image));
      pragma Assert (Calls = 66 and Large_Growth.Description_Count (State, Image) = 66);
   end;
   Ada.Text_IO.Put_Line ("Growth topology PASS" & Natural'Image (Cases) & " ranges plus512 leaf conflict positions; stepped66-node bound and4 callback faults; interval oracle and offline mapper agree; no GPU publication");
end VM_Growth_Tests;
