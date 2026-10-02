with Ada.Text_IO;
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
      package E renames Intel_GPU_Physical_Extents;
      Bases : E.Addresses;
      Map : E.Map;
      OK : Boolean;
      View, Part : Extent_View;
   begin
      for I in E.Block_Index loop Bases (I) := 16#4000_0000# - Unsigned_64 (I) * 2 * E.Block_Bytes; end loop;
      E.Admit (Bases, Map, OK); pragma Assert (OK);
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
      E.Admit (Bases, Map, OK); pragma Assert (OK);
      pragma Assert (not Same_Arena (View, From_Extents (Map, 7, 0, 4096)));
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
   for Index in Layout.Slot loop
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
   Ada.Text_IO.Put_Line ("Buffer reply PASS: 65536 slot/size boundaries, envelopes, arena bounds and failure payloads (scalar codec only)");
end Buffer_Reply_Tests;
