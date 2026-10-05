with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package Growth is new VM.Growth;
   use type Growth.Plan_Status;
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
            function Authorized return Boolean is (True);
            procedure Emit (Ordinal : Positive; Item : Growth.Node; Accepted : out Boolean) is
            begin
               Streamed (Streamed'First + (Ordinal - 1)) := Item;
               Accepted := True;
            end Emit;
            procedure Describe_Stream is new Growth.Describe_Into (Authorized, Emit);
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
   Ada.Text_IO.Put_Line ("Growth topology PASS" & Natural'Image (Cases) & " ranges plus512 leaf conflict positions; interval oracle and offline mapper agree; no GPU publication");
end VM_Growth_Tests;
