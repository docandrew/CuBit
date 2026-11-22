with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_PPGTT_Scratch;
procedure VM_Insertion_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   procedure Stepped (Fault : Natural) is
      Source, Other : VM.Image;
      DMA : VM.Backing_Pages;
      Data : VM.Data_Pages (5 .. 7) := [16#200000#, 16#201000#, 16#202000#];
      Held : Boolean := True;
      Writes, Before : Natural := 0;
      Epoch : Unsigned_64;
      OK : Boolean;
      procedure Reenter;
      function Exclusive return Boolean is (Held);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         pragma Assert (Table_DMA = 16#4000# and Index = Table_Index (Writes + 1));
         pragma Assert (Expected = 0 and Replacement = Encode_Leaf
           (16#200000# + Unsigned_64 (Writes - 1) * 4096, Write_Back, Read_Write));
         pragma Assert (VM.Lookup (Source, 8192) = 0 and VM.Revision (Source) = Epoch);
         if Writes = 1 and Fault in 5 | 6 then Reenter; end if;
         if Writes = 3 and Fault = 7 then Held := False; end if;
         Success := not (Fault = 8 and Writes = 2);
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin Success := True; end;
      package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
      State : Insert.Controller;
      procedure Reenter is
         Accepted : Boolean;
      begin
         if Fault = 5 then Insert.Step (State, Source);
         else Insert.Commit (State, Source, True, Accepted); pragma Assert (not Accepted);
         end if;
      end Reenter;
   begin
      for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
      VM.Initialize (Source, DMA, OK); pragma Assert (OK);
      VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
      VM.Seal (Source, OK); pragma Assert (OK);
      for P in DMA'Range loop DMA (P) := Unsigned_64 (P + 16) * 4096; end loop;
      VM.Initialize (Other, DMA, OK); pragma Assert (OK);
      VM.Map_Page (Other, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
      VM.Seal (Other, OK); pragma Assert (OK);
      Epoch := VM.Revision (Source);
      Insert.Start (State, Source, Epoch, 8192, Data, Write_Back, Read_Write, OK);
      pragma Assert (OK and Writes = 0 and Insert.Publishing (State) and not Insert.Published (State));
      Data := [others => 16#BAD000#]; -- retained words must not borrow this array
      if Fault = 1 then Held := False; end if;
      if Fault = 4 then
         Insert.Commit (State, Source, True, OK); pragma Assert (not OK);
      end if;
      if Fault = 9 then
         Insert.Start (State, Source, Epoch, 8192, Data, Write_Back, Read_Write, OK);
         pragma Assert (not OK and Writes = 0);
      end if;
      while Insert.Publishing (State) loop
         Before := Writes;
         if Fault = 2 and Writes = 1 then Held := False; end if;
         if Fault = 3 and Writes = 1 then Insert.Step (State, Other);
         else Insert.Step (State, Source); end if;
         pragma Assert (Writes <= Before + 1 and Writes <= 3);
      end loop;
      pragma Assert (VM.Lookup (Source, 8192) = 0 and VM.Revision (Source) = Epoch);
      pragma Assert (Insert.Published (State) = (Fault in 0 | 9));
      Insert.Commit (State, Source, True, OK);
      pragma Assert (OK = (Fault in 0 | 9));
      if OK then
         pragma Assert (Writes = 3 and VM.Revision (Source) = Epoch + 1);
         pragma Assert (VM.Lookup (Source, 16384) = Encode_Leaf (16#202000#, Write_Back, Read_Write));
      else
         pragma Assert (Insert.Failed (State) and VM.Revision (Source) = Epoch);
         pragma Assert (Writes = (case Fault is
           when 1 | 4 => 0, when 2 | 3 | 5 | 6 => 1, when 8 => 2, when others => 3));
         Held := True; Before := Writes;
         Insert.Step (State, Source); pragma Assert (Writes = Before);
         Insert.Start (State, Source, Epoch, 8192, Data, Write_Back, Read_Write, OK);
         pragma Assert (not OK and Writes = Before);
      end if;
   end Stepped;
   procedure Run (Mode : Natural; With_Scratch : Boolean) is
      Object : VM.Image;
      DMA : VM.Backing_Pages;
      Hardware : array (Table_Index) of Unsigned_64 := [others => 0];
      Owner : Boolean := True;
      Writes, Flushes : Natural := 0;
      OK : Boolean;
      Epoch : Unsigned_64;
      Scratch : constant Intel_GPU_PPGTT_Scratch.Backing_Pages :=
        (if With_Scratch then [16#90000#, 16#91000#, 16#92000#, 16#93000#]
         else [others => 0]);
      Empty_Leaf : constant Unsigned_64 := Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 0);
      function Exclusive return Boolean is (Owner);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         pragma Assert (Table_DMA = DMA (4));
         pragma Assert (VM.Lookup (Object, 16#2000#) = 0);
         Success := Hardware (Index) = Expected;
         if (Mode = 1 and Writes = 1) or (Mode = 2 and Writes = 2) then
            Success := False; return;
         end if;
         if Success then Hardware (Index) := Replacement; end if;
         if Mode = 4 then Owner := False; end if;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin
         Flushes := Flushes + 1;
         pragma Assert (VM.Lookup (Object, 16#2000#) = 0);
         Success := Mode /= 3;
         if Mode = 5 then Owner := False; end if;
      end Invalidate;
      package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
      State : Insert.Controller;
      procedure Reject (VA : Unsigned_64; Pages : VM.Data_Pages;
                        Policy : Cache_Policy := Write_Back;
                        Access_Mode : Page_Access := Read_Write;
                        Revision : Unsigned_64 := 0) is
      begin
         pragma Assert (not Insert.Can_Reuse (State, Object,
           (if Revision = 0 then Epoch else Revision), VA, Pages, Policy, Access_Mode));
         Insert.Execute (State, Object,
           (if Revision = 0 then Epoch else Revision), VA, Pages, Policy, Access_Mode, OK);
         pragma Assert (not OK and Writes = 0 and Flushes = 0 and not Insert.Failed (State));
         pragma Assert (VM.Revision (Object) = Epoch and VM.Used (Object) = 4);
      end Reject;
   begin
      for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
      VM.Initialize (Object, DMA, OK, Scratch); pragma Assert (OK);
      VM.Map_Page (Object, 16#1000#, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Object, OK); pragma Assert (OK);
      Hardware := [others => Empty_Leaf];
      Hardware (1) := Encode_Leaf (16#100000#, Write_Back, Read_Write);
      Epoch := VM.Revision (Object);
      pragma Assert (not Insert.Range_Reusable (Object, 0, 4096));
      pragma Assert (not Insert.Range_Reusable (Object, 4096, 4096));
      pragma Assert (not Insert.Range_Reusable (Object, 8192, 0));
      pragma Assert (not Insert.Range_Reusable (Object, 8192, 4097));
      pragma Assert (not Insert.Range_Reusable (Object, 16#1FF000#, 8192));
      pragma Assert (not Insert.Range_Reusable (Object, 2 ** 48 - 4096, 8192));
      pragma Assert (Insert.Range_Reusable (Object, 8192, 8192));
      Reject (0, [1 => 16#200000#]);
      Reject (16#2001#, [1 => 16#200000#]);
      Reject (16#2000#, [1 => 16#200001#]);
      Reject (16#2000#, [1 => DMA (8)]); -- unused reserved backing also excluded
      if With_Scratch then
         for Page of Scratch loop Reject (16#2000#, [1 => Page]); end loop;
      end if;
      Reject (16#1000#, [1 => 16#200000#]); -- occupied leaf
      Reject (16#2000#, [1 => 16#100000#], Uncached); -- incompatible alias
      Reject (16#2000#, [1 => 16#200000#], Access_Mode => Read_Only);
      Reject (16#2000#, [1 => 16#200000#], Revision => Epoch + 1);
      Reject (16#1FF000#, [16#200000#, 16#201000#]); -- second directory missing
      pragma Assert (Hardware (511) = Empty_Leaf); -- whole-range preflight
      Owner := False;
      pragma Assert (Insert.Can_Reuse (State, Object, Epoch, 16#2000#,
        [16#200000#, 16#201000#], Write_Back, Read_Write));
      Insert.Publish (State, Object, Epoch, 16#2000#,
        [16#200000#, 16#201000#], Write_Back, Read_Write, OK);
      pragma Assert (not OK and Writes = 0 and Flushes = 0 and not Insert.Failed (State));
      Owner := True;
      if Mode = 6 then Hardware (3) := 16#DEAD0003#; end if;
      Insert.Execute (State, Object, Epoch, 16#2000#, [16#200000#, 16#201000#],
                      Write_Back, Read_Write, OK);
      if Mode = 0 then
         pragma Assert (OK and not Insert.Failed (State) and Writes = 2 and Flushes = 1);
         pragma Assert (VM.Lookup (Object, 16#2000#) = Encode_Leaf (16#200000#, Write_Back, Read_Write) and
                        VM.Lookup (Object, 16#3000#) = Encode_Leaf (16#201000#, Write_Back, Read_Write));
         pragma Assert (VM.Revision (Object) = Epoch + 1 and VM.Used (Object) = 4);
      else
         pragma Assert (not OK and Insert.Failed (State));
         pragma Assert (VM.Lookup (Object, 16#2000#) = 0 and VM.Revision (Object) = Epoch);
         pragma Assert (Writes = (if Mode in 1 | 4 then 1 else 2));
         pragma Assert (Flushes = (if Mode in 3 | 5 then 1 else 0));
         if Mode /= 1 then
            pragma Assert (Hardware (2) = Encode_Leaf (16#200000#, Write_Back, Read_Write));
         end if;
         declare Before : constant Natural := Writes; begin
            Owner := True;
            Insert.Execute (State, Object, Epoch, 16#2000#, [16#200000#, 16#201000#],
                            Write_Back, Read_Write, OK);
            pragma Assert (not OK and Writes = Before);
         end;
      end if;
   end Run;
   procedure Repeat_Reuse (With_Scratch : Boolean) is
      Object : VM.Image;
      DMA : VM.Backing_Pages;
      Scratch : constant Intel_GPU_PPGTT_Scratch.Backing_Pages :=
        (if With_Scratch then [16#90000#, 16#91000#, 16#92000#, 16#93000#]
         else [others => 0]);
      Empty_Leaf : constant Unsigned_64 := Intel_GPU_PPGTT_Scratch.Fallback (Scratch, 0);
      Hardware : array (Table_Index) of Unsigned_64 := [others => Empty_Leaf];
      Removing : Boolean := False;
      Writes, Flushes : Natural := 0;
      Epoch : Unsigned_64;
      OK : Boolean;
      function Exclusive return Boolean is (True);
      procedure Check_Metadata is
      begin
         pragma Assert (VM.Lookup (Object, 16#2000#) =
           (if Removing then Encode_Leaf (16#200000#, Write_Back, Read_Write) else 0));
      end Check_Metadata;
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         Check_Metadata;
         pragma Assert (Table_DMA = DMA (4) and Index in 2 .. 3);
         Success := Hardware (Index) = Expected;
         if Success then Hardware (Index) := Replacement; end if;
         Writes := Writes + 1;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin
         Check_Metadata;
         Flushes := Flushes + 1; Success := True;
      end Invalidate;
      package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
      package Remove is new VM.Removal (Exclusive, Write_Leaf, Invalidate);
      Insertion_State : Insert.Controller;
      Removal_State : Remove.Controller;
   begin
      for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
      VM.Initialize (Object, DMA, OK, Scratch); pragma Assert (OK);
      VM.Map_Page (Object, 16#1000#, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK);
      VM.Seal (Object, OK); pragma Assert (OK);
      Hardware (1) := Encode_Leaf (16#100000#, Write_Back, Read_Write);
      Epoch := VM.Revision (Object);
      for Cycle in 1 .. 4096 loop
         Removing := False;
         Insert.Execute (Insertion_State, Object, VM.Revision (Object), 16#2000#,
           [5 => 16#200000#, 6 => 16#201000#], Write_Back, Read_Write, OK);
         pragma Assert (OK and not Insert.Failed (Insertion_State));
         Removing := True;
         Remove.Execute (Removal_State, Object, VM.Revision (Object), 16#2000#,
                         [5 => 16#200000#, 6 => 16#201000#], OK);
         pragma Assert (OK and not Remove.Failed (Removal_State));
         pragma Assert (VM.Used (Object) = 4 and VM.Root_DMA (Object) = DMA (1));
         pragma Assert (VM.Revision (Object) = Epoch + Unsigned_64 (Cycle) * 2);
         pragma Assert (Hardware (2) = Empty_Leaf and Hardware (3) = Empty_Leaf);
         pragma Assert (VM.Lookup (Object, 16#2000#) = 0 and
                        VM.Lookup (Object, 16#3000#) = 0);
      end loop;
      pragma Assert (Writes = 16384 and Flushes = 8192);
   end Repeat_Reuse;
   procedure Split_Receipt (Mode : Natural) is
      Object, Other : VM.Image;
      DMA : VM.Backing_Pages;
      Data : VM.Data_Pages (5 .. 6) := [16#200000#, 16#201000#];
      Owner : Boolean := True;
      Writes : Natural := 0;
      Epoch : Unsigned_64;
      OK : Boolean;
      function Exclusive return Boolean is (Owner);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         pragma Assert (Table_DMA = 16#4000# and Index in 2 .. 3 and Expected = 0);
         pragma Assert (Replacement = Encode_Leaf
           (16#200000# + Unsigned_64 (Index - 2) * 4096, Write_Back, Read_Write));
         Writes := Writes + 1; Success := True;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin
         pragma Assert (False); -- split API must leave this to coordinator
         Success := False;
      end Invalidate;
      package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
      State : Insert.Controller;
   begin
      for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
      VM.Initialize (Object, DMA, OK); pragma Assert (OK);
      VM.Map_Page (Object, 16#1000#, 16#100000#, Write_Back, Read_Write, OK);
      pragma Assert (OK); VM.Seal (Object, OK); pragma Assert (OK);
      Epoch := VM.Revision (Object);
      Insert.Commit (State, Object, True, OK);
      pragma Assert (not OK and not Insert.Failed (State));
      Insert.Publish (State, Object, Epoch, 16#2000#, Data, Write_Back, Read_Write, OK);
      pragma Assert (OK and Insert.Failed (State) and Writes = 2);
      pragma Assert (VM.Lookup (Object, 16#2000#) = 0 and VM.Revision (Object) = Epoch);
      Data := [16#300000#, 16#301000#]; -- must not change retained receipt
      Insert.Publish (State, Object, Epoch, 16#2000#, Data, Write_Back, Read_Write, OK);
      pragma Assert (not OK and Writes = 2);
      if Mode = 2 then Owner := False; end if;
      if Mode = 3 then
         for P in DMA'Range loop DMA (P) := 16#10000# + Unsigned_64 (P) * 4096; end loop;
         VM.Initialize (Other, DMA, OK); pragma Assert (OK);
         VM.Map_Page (Other, 16#1000#, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK); VM.Seal (Other, OK); pragma Assert (OK);
         Insert.Commit (State, Other, True, OK);
         pragma Assert (VM.Lookup (Other, 16#2000#) = 0);
      else
         Insert.Commit (State, Object, Mode /= 1, OK);
      end if;
      if Mode = 0 then
         pragma Assert (OK and not Insert.Failed (State));
         pragma Assert (VM.Lookup (Object, 16#2000#) =
           Encode_Leaf (16#200000#, Write_Back, Read_Write));
      else
         pragma Assert (not OK and Insert.Failed (State));
         pragma Assert (VM.Revision (Object) = Epoch and VM.Lookup (Object, 16#2000#) = 0);
      end if;
      Owner := True;
      Insert.Commit (State, Object, True, OK);
      pragma Assert (not OK and Writes = 2); -- receipt is consumed, even on failure
   end Split_Receipt;
begin
   for Fault in 0 .. 9 loop Stepped (Fault); end loop;
   for Scratch in Boolean loop
      for Mode in 0 .. 6 loop Run (Mode, Scratch); end loop;
      Repeat_Reuse (Scratch);
   end loop;
   for Mode in 0 .. 3 loop Split_Receipt (Mode); end loop;
   Ada.Text_IO.Put_Line ("In-place insertion PASS: preflight, unchanged table count, delayed metadata commit, sticky failure (mock GPU)");
   Ada.Text_IO.Put_Line ("Stepped insertion PASS10: one leaf per turn, owned receipt, no early commit, callback reentry, lost ownership/source and no replay");
end VM_Insertion_Tests;
