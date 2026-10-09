with Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Directory;
with Extent_Directory_Fixture;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply; use Intel_GPU_Buffer_Reply;
procedure Buffer_Reply_Tests is
   Base : constant Unsigned_64 := 16#2000000#;
   Data : Words;
   Decoded : Backing;
   procedure Reject (W : Words) is
   begin
      pragma Assert (Classify (1, 1, 16#F000#, 4, 0, 0, W) = Invalid);
      pragma Assert (not Decode (1, 1, 16#F000#, 4, 0, 0, W).Ready);
   end Reject;
   type Values is array (Positive range <>) of Unsigned_64;
begin
   declare
      package L renames Intel_GPU_Buffer_Backing;
      Block : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
      function Valid (Bytes, DMA : Unsigned_64; CPU : Unsigned_64 := L.CPU_Base)
        return Boolean is (L.Heap_Geometry_Valid (Bytes, DMA, CPU));
   begin
      pragma Assert (Valid (L.Default_Heap.Byte_Quota, L.Default_Heap.DMA_Limit));
      pragma Assert (Valid (24 * 1024 ** 3, 2 ** 48));
      pragma Assert (Valid (2 ** 40, 2 ** 48));
      pragma Assert (Valid (2 ** 47 - L.CPU_Base, 2 ** 48));
      pragma Assert (not Valid (2 ** 47 - L.CPU_Base + Block, 2 ** 48));
      pragma Assert (not Valid (Unsigned_64'Last, 2 ** 48));
      pragma Assert (not Valid (0, 2 ** 48));
      pragma Assert (not Valid (Block + 1, 2 ** 48));
      pragma Assert (not Valid (Block, Block));
      pragma Assert (not Valid (Block, 2 ** 48 + 1));
      pragma Assert (not Valid (Block, 2 ** 48, 0));
      pragma Assert (not Valid (Block, 2 ** 48, L.CPU_Base + 1));
      pragma Assert (not Valid (Block, 2 ** 48, 2 ** 47));
      pragma Assert (not Valid (Block, 2 ** 48, Unsigned_64'Last));
   end;
   declare
      package L renames Intel_GPU_Buffer_Backing;
      Block : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
      W : L.Budget_Words := [0, 7, 0, 0];
      function Admit
        (Label : Unsigned_32 := L.Extent_Request_Label;
         Length : Unsigned_8 := 2; Flags : Unsigned_8 := 0; Reserved : Unsigned_16 := 0;
         Sender : Unsigned_64 := 7; Authority : Unsigned_64 := 16#4947#;
         Owner : Unsigned_64 := 7; Committed : Unsigned_64 := 600 * Block;
         Granted : Boolean := True) return Boolean is
        (L.Extent_Request_Authorized (Label, Length, Flags, Reserved, W,
           Sender, Authority, Owner, Committed, Granted));
   begin
      for Index in 0 .. 599 loop
         W (0) := Unsigned_64 (Index);
         pragma Assert (Admit);
         pragma Assert (L.CPU_Base + W (0) * Block < 2 ** 47);
      end loop;
      W (0) := 600; pragma Assert (not Admit);
      W (0) := Unsigned_64'Last; pragma Assert (not Admit);
      W (0) := 16;
      pragma Assert (not Admit (Committed => 16 * Block));
      pragma Assert (Admit (Committed => 17 * Block));
      pragma Assert (not Admit (Label => 0));
      pragma Assert (not Admit (Length => 3));
      pragma Assert (not Admit (Flags => 1));
      pragma Assert (not Admit (Reserved => 1));
      pragma Assert (not Admit (Sender => 8));
      pragma Assert (not Admit (Authority => 0));
      pragma Assert (not Admit (Owner => 0));
      pragma Assert (not Admit (Granted => False));
      pragma Assert (not Admit (Committed => 0));
      pragma Assert (not Admit (Committed => 17 * Block + 1));
      pragma Assert (not Admit (Committed => Unsigned_64'Last));
      pragma Assert (not Admit (Committed => 2 ** 47 - L.CPU_Base + Block));
      W (1) := 8; pragma Assert (not Admit); W (1) := 7;
      W (2) := 1; pragma Assert (not Admit); W (2) := 0;
      W (3) := 1; pragma Assert (not Admit);
   end;
   pragma Assert (Extent_View'Object_Size <= 64 * 8);
   pragma Assert (Backing'Object_Size <= 96 * 8);
   declare
      package D renames Intel_GPU_Extent_Directory;
      Block : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
      DMA : constant Unsigned_64 := 2 ** 40;
      Frame_4K : constant Unsigned_64 := 3840 * 2160 * 4;
      Owner : aliased D.Directory;
      type Metadata is array (1 .. 16384) of Unsigned_8 with Alignment => 4096;
      Storage : Metadata := [others => 0];
      Early : Extent_View;
      Whole, Frame, Tail : Backing;
      OK : Boolean;
   begin
      -- Model discontiguous physical pages; only metadata is real host RAM.
      D.Initialize (Owner, 2 ** 40, 2 ** 48, OK);
      pragma Assert (OK);
      for I in 0 .. 15 loop
         D.Append (Owner, DMA + Unsigned_64 (I) * 2 * Block, OK);
         pragma Assert (OK);
      end loop;
      Early := From_Extents (D.Borrow (Owner), 77, 0, 16 * Block);
      Whole := From_View (Early);
      pragma Assert (Valid (Whole) and then Whole.Bytes = 32 * 1024 ** 2);
      Frame := Slice (Whole, 0, Frame_4K);
      pragma Assert (Valid (Frame) and then Frame.Bytes = Frame_4K);
      pragma Assert (Page_Address (Frame, Frame_4K - 4096) =
        DMA + ((Frame_4K - 4096) / Block) * 2 * Block +
        (Frame_4K - 4096) mod Block);
      pragma Assert (not From_Linear (16#2000000#, Layout.CPU_Base,
        Frame_4K, 16#2000000#).Ready);
      D.Extend_Metadata (Owner, Unsigned_64 (To_Integer (Storage'Address)),
        16384, OK);
      pragma Assert (OK);
      for I in 16 .. 511 loop
         D.Append (Owner, DMA + Unsigned_64 (I) * 2 * Block, OK);
         pragma Assert (OK);
      end loop;
      pragma Assert (Valid (Frame) and Byte_Count (Early) = 16 * Block);
      pragma Assert (not Slice (Whole, 0, 2 ** 30).Ready);
      Whole := From_View (From_Extents (D.Borrow (Owner), 77, 0, 2 ** 30));
      pragma Assert (Valid (Whole) and then Same_Arena (Whole, Frame));
      for I in 0 .. 511 loop
         pragma Assert (Overlaps_DMA (Whole, DMA + Unsigned_64 (I) * 2 * Block, 4096));
         pragma Assert (not Overlaps_DMA (Whole,
           DMA + Unsigned_64 (I) * 2 * Block + Block, 4096));
         pragma Assert (Page_Address (Whole, Unsigned_64 (I) * Block) =
           DMA + Unsigned_64 (I) * 2 * Block);
         pragma Assert (Page_Address (Whole, Unsigned_64 (I + 1) * Block - 4096) =
           DMA + Unsigned_64 (I) * 2 * Block + Block - 4096);
      end loop;
      Tail := Slice (Whole, 2 ** 30 - 8192, 8192);
      pragma Assert (Valid (Tail) and then Tail.CPU_Address =
        Layout.CPU_Base + 2 ** 30 - 8192);
      pragma Assert (not Overlaps_DMA (Tail, DMA + 1022 * Block, Block - 8192));
      pragma Assert (Overlaps_DMA (Tail, DMA + 1022 * Block, Block - 8191));
      pragma Assert (Overlaps_DMA (Whole, DMA - 1, 2));
      pragma Assert (not Overlaps_DMA (Whole, DMA - 1, 1));
      pragma Assert (Page_Address (Whole, 2 ** 30) = 0);
      pragma Assert (not Slice (Whole, 2 ** 30 - 4096, 8192).Ready);
      pragma Assert (not Slice (Whole, 4096, Unsigned_64'Last - 4095).Ready);
      pragma Assert (not Valid (From_Extents (D.Borrow (Owner), 77,
        2 ** 47 - Layout.CPU_Base, 4096)));
      pragma Assert (not Valid (From_Extents (D.Borrow (Owner), 77,
        Unsigned_64'Last - 4095, 4096)));
      D.Quarantine (Owner);
      pragma Assert (not Valid (Whole) and not Valid (Frame) and not Valid (Tail));
      pragma Assert (Page_Address (Whole, 0) = 0);
      pragma Assert (not Slice (Whole, 0, 4096).Ready);
   end;
   declare
      package E renames Intel_GPU_Physical_Extents;
      Bases : E.Addresses;
      Map : Intel_GPU_Extent_Directory.Borrowed_View;
      Owner, Other : aliased Intel_GPU_Extent_Directory.Directory;
      OK : Boolean;
      View, Part : Extent_View;
   begin
      for I in E.Block_Index loop Bases (I) := 16#4000_0000# - Unsigned_64 (I) * 2 * E.Block_Bytes; end loop;
      Extent_Directory_Fixture.Initialize (Owner, Bases);
      Map := Intel_GPU_Extent_Directory.Borrow (Owner);
      View := From_Extents (Map, 7, 0, E.Capacity);
      pragma Assert (Valid (View) and CPU_Address (View) = Layout.CPU_Base);
      for I in 0 .. 8191 loop
         pragma Assert (Page_Address (View, Unsigned_64 (I) * 4096) =
           Bases (I / 512) + Unsigned_64 (I mod 512) * 4096);
      end loop;
      Part := Slice (View, E.Block_Bytes - 4096, 8192);
      pragma Assert (Valid (Part) and Same_Arena (View, Part));
      pragma Assert (Page_Address (Part, 0) = Bases (0) + E.Block_Bytes - 4096);
      pragma Assert (Page_Address (Part, 4096) = Bases (1));
      pragma Assert (Byte_Count (Part) = 8192 and Page_Address (Part, 8192) = 0);
      pragma Assert (Overlaps_DMA (Part, Bases (0) + E.Block_Bytes - 1, 1));
      pragma Assert (Overlaps_DMA (Part, Bases (1), 4096));
      pragma Assert (not Overlaps_DMA (Part, Bases (1) + 4096, 4096));
      pragma Assert (not Overlaps_DMA (Part, Bases (0), 4096));
      pragma Assert (not Overlaps_DMA (View, Bases (1) + E.Block_Bytes, E.Block_Bytes));
      pragma Assert (Overlaps_DMA (View, Unsigned_64'Last, 2));
      -- Independent interval oracle straddles physical block and logical slice
      -- edges, including the boundary between indexed and general queries.
      for I in E.Block_Index loop
         for Address of Values'[Bases (I) - 1, Bases (I), Bases (I) + 4095,
           Bases (I) + E.Block_Bytes - 1, Bases (I) + E.Block_Bytes,
           Bases (I) + E.Block_Bytes + 1]
         loop
            for Length of Values'[1, 2, 4096, E.Block_Bytes - 1,
              E.Block_Bytes, E.Block_Bytes + 1]
            loop
               declare
                  Whole_Hit : Boolean := False;
                  function Hit (Start, Count : Unsigned_64) return Boolean is
                    (Address < Start + Count and Start < Address + Length);
                  Part_Hit : constant Boolean :=
                    Hit (Bases (0) + E.Block_Bytes - 4096, 4096) or
                    Hit (Bases (1), 4096);
               begin
                  for J in E.Block_Index loop
                     Whole_Hit := Whole_Hit or Hit (Bases (J), E.Block_Bytes);
                  end loop;
                  pragma Assert (Overlaps_DMA (View, Address, Length) = Whole_Hit);
                  pragma Assert (Overlaps_DMA (Part, Address, Length) = Part_Hit);
               end;
            end loop;
         end loop;
      end loop;
      declare
         Buffer : constant Backing := From_View (Part);
      begin
         pragma Assert (Valid (Buffer) and Byte_Count (Part) = Buffer.Bytes);
         pragma Assert (Page_Address (Buffer, 0) = Bases (0) + E.Block_Bytes - 4096);
         pragma Assert (Page_Address (Buffer, 4096) = Bases (1));
         pragma Assert (Same_Arena (Buffer, Slice (Buffer, 4096, 4096)));
         pragma Assert (not Overlaps_DMA (Buffer, Bases (1) + 4096, 4096));
      end;
      pragma Assert (not Valid (Slice (View, Unsigned_64'Last, 4096)));
      pragma Assert (not Valid (From_Extents (Map, 0, 0, 4096)));
      pragma Assert (not Same_Arena (View, From_Extents (Map, 8, 0, 4096)));
      Bases (0) := 16#6000_0000#;
      Extent_Directory_Fixture.Initialize (Other, Bases);
      Map := Intel_GPU_Extent_Directory.Borrow (Other);
      pragma Assert (not Same_Arena (View, From_Extents (Map, 7, 0, 4096)));
      Intel_GPU_Extent_Directory.Quarantine (Owner);
      pragma Assert (not Valid (View) and Page_Address (View, 0) = 0);
   end;
   declare
      Parent : Backing := From_Linear (Base, Layout.CPU_Base, 256 * 4096, Base);
      Part : Backing;
   begin
      pragma Assert (Valid (Parent) and Same_Arena (Parent, Parent));
      pragma Assert (not Overlaps_DMA (Parent, Base - 4096, 4096));
      pragma Assert (Overlaps_DMA (Parent, Base - 4096, 4097));
      pragma Assert (Overlaps_DMA (Parent, Base + Parent.Bytes - 1, 1));
      pragma Assert (not Overlaps_DMA (Parent, Base + Parent.Bytes, 4096));
      pragma Assert (Overlaps_DMA (Parent, Unsigned_64'Last, 2));
      pragma Assert (Overlaps_DMA (Parent, Base, 0));
      for First in 0 .. 255 loop
         pragma Assert (Page_Address (Parent, Unsigned_64 (First) * 4096) =
           Base + Unsigned_64 (First) * 4096);
         for Count in 1 .. 256 loop
            Part := Slice (Parent, Unsigned_64 (First) * 4096,
                           Unsigned_64 (Count) * 4096);
            pragma Assert (Part.Ready = (First + Count <= 256));
            if Part.Ready then
               pragma Assert (Page_Address (Part, 0) = Base + Unsigned_64 (First) * 4096
                 and Part.CPU_Address = Layout.CPU_Base + Unsigned_64 (First) * 4096
                 and Part.Bytes = Unsigned_64 (Count) * 4096 and Same_Arena (Part, Parent));
            end if;
         end loop;
      end loop;
      for Bad of Values'[1, 4095, Unsigned_64'Last, Unsigned_64'Last - 4095] loop
         pragma Assert (Page_Address (Parent, Bad) = 0);
         pragma Assert (not Slice (Parent, Bad, 4096).Ready);
         pragma Assert (not Slice (Parent, 0, Bad).Ready);
      end loop;
      pragma Assert (not Slice (Parent, 0, 0).Ready);
      pragma Assert (not Slice (Parent, Parent.Bytes, 4096).Ready);
      Parent.CPU_Address := Parent.CPU_Address + 4096;
      pragma Assert (not Valid (Parent) and not Same_Arena (Parent, Parent));
      pragma Assert (Overlaps_DMA (Parent, Base + Parent.Bytes, 4096));
      pragma Assert (Page_Address (Parent, 0) = 0);
      pragma Assert (not Slice (Parent, 0, 4096).Ready);
      Parent := From_Linear (Base, Layout.CPU_Base - 4096, 4096, Base);
      pragma Assert (not Slice (Parent, 0, 4096).Ready);
      Parent := (Ready => False);
      pragma Assert (Page_Address (Parent, 0) = 0);
      pragma Assert (not Slice (Parent, 0, 4096).Ready);
   end;
   for Index in 1 .. Layout.Bootstrap_Slots loop
      for Pages in Layout.Page_Count loop
         declare
            Bytes : constant Unsigned_64 := Unsigned_64 (Pages) * 4096;
            Offset : constant Unsigned_64 := Layout.Capacity - Bytes;
         begin
            Data := [Base + Offset, Layout.CPU_Base + Offset, Bytes, Unsigned_64 (Index)];
            pragma Assert (Classify (Index, Pages, 16#F000#, 4, 0, 0, Data) = Granted);
            Decoded := Decode (Index, Pages, 16#F000#, 4, 0, 0, Data);
            pragma Assert (Valid (Decoded) and then
              Page_Address (Decoded, 0) = Base + Offset and then Decoded.Bytes = Bytes);
         end;
      end loop;
   end loop;
   Data := [Base, Layout.CPU_Base, 4096, 1];
   for N in Unsigned_8 loop
      pragma Assert ((Classify (1, 1, 16#F000#, N, 0, 0, Data) = Granted) = (N = 4));
      pragma Assert ((Classify (1, 1, 16#F000#, 4, N, 0, Data) = Granted) = (N = 0));
   end loop;
   for N in Unsigned_16 loop
      pragma Assert ((Classify (1, 1, 16#F000#, 4, 0, N, Data) = Granted) = (N = 0));
   end loop;
   for CPU of Values'[0, Layout.CPU_Base - 4096, Layout.CPU_Base + 1,
                     Layout.CPU_Base + Layout.Capacity, Unsigned_64'Last] loop
      Reject ([Base, CPU, 4096, 1]);
   end loop;
   for DMA of Values'[0, 1, 2 ** 32 - Layout.Capacity + 4096, Unsigned_64'Last] loop
      Reject ([DMA, Layout.CPU_Base, 4096, 1]);
   end loop;
   Reject ([4096, Layout.CPU_Base + 8192, 4096, 1]);
   Reject ([Base, Layout.CPU_Base, 8192, 1]);
   Reject ([Base, Layout.CPU_Base, 4096, 2]);
   for Label in Unsigned_32 range 16#F000# .. 16#F003# loop
      pragma Assert ((Classify (1, 1, Label, 4, 0, 0, Data) = Granted) = (Label = 16#F000#));
   end loop;
   pragma Assert (Classify (1, 1, 16#F002#, 0, 0, 0, [0, 0, 0, 0]) = Retry);
   pragma Assert (Classify (1, 1, 16#F001#, 0, 0, 0, [0, 0, 0, 0]) = Denied);
   for I in Data'Range loop
      declare W : Words := [others => 0]; begin
         W (I) := 1;
         pragma Assert (Classify (1, 1, 16#F002#, 0, 0, 0, W) = Invalid);
         pragma Assert (Classify (1, 1, 16#F001#, 0, 0, 0, W) = Invalid);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Buffer reply PASS: 65536 scalar boundaries; extent-backed 4K/1GiB geometry, stable growth, slices and quarantine (modeled DMA)");
end Buffer_Reply_Tests;
