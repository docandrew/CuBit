with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Test_Mutation;
with Intel_GPU_VM_Image.Range_Query;
procedure VM_Commit_Walk_Tests is
   package VM is new Intel_GPU_VM_Image (128);
   package Mutation is new VM.Test_Mutation;
   package Ranges is new VM.Range_Query;
   use type Ranges.Result;
   Source, Other : VM.Image;
   Backing : VM.Backing_Pages;
   OK, Done : Boolean;
   Held : Boolean := True;
   Epoch : Unsigned_64;
   Writes, Turns, Publication_Turns : Natural := 0;
   Base : constant Unsigned_64 := 31 * 2 ** 39;
   Target : constant Unsigned_64 := Base + 2 ** 21 - 4096;
   function Exclusive return Boolean is (Held);
   procedure Write_Leaf
     (Table_DMA : Unsigned_64; Index : Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean);
   procedure Invalidate (Success : out Boolean) is
   begin Success := True; end Invalidate;
   package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
   State : Insert.Controller;
   procedure Write_Leaf
     (Table_DMA : Unsigned_64; Index : Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
      Ordinal : constant Natural := Insert.Publication_Table (State, Source);
   begin
      pragma Assert (Ordinal > 64 and VM.Leaf_Table (Source, Ordinal));
      pragma Assert (VM.Page_DMA (Source, Ordinal) = Table_DMA);
      pragma Assert (Insert.Publication_Table (State, Other) = 0);
      Held := False;
      pragma Assert (Insert.Publication_Table (State, Source) = 0);
      Held := True;
      pragma Assert (Table_DMA /= 0 and Expected = 0 and Replacement /= 0);
      pragma Assert (Index = (if Writes = 0 then 511 else Table_Index (Writes - 1)));
      Writes := Writes + 1; Success := True;
   end Write_Leaf;
begin
   for I in Backing'Range loop Backing (I) := Unsigned_64 (I) * 4096; end loop;
   VM.Initialize (Source, Backing, OK); pragma Assert (OK);
   for I in 0 .. 31 loop
      VM.Map_Page (Source, Unsigned_64 (I) * 2 ** 39 + 4096,
        16#4000000# + Unsigned_64 (I) * 4096, Write_Back, Read_Write, OK);
      pragma Assert (OK);
   end loop;
   VM.Map_Page (Source, Base + 2 ** 21 + 8192, 16#5000000#,
     Write_Back, Read_Write, OK); pragma Assert (OK);
   VM.Seal (Source, OK); pragma Assert (OK);
   Epoch := VM.Revision (Source);
   declare
      Query : Ranges.Query;
      Count : Natural;
      Address : Unsigned_64;
   begin
      for Case_ID in 0 .. 4 loop
         Address := (case Case_ID is
           when 1 => Base + 4096, -- occupied
           when 2 => Base + 2 ** 22, -- missing directory
           when others => Target);
         Ranges.Start (Query, Source, Address, 3 * 4096, OK);
         pragma Assert (OK);
         Ranges.Start (Query, Source, 4096, 4096, OK);
         pragma Assert (not OK); -- cannot replace a retained scan
         Count := 0;
         while Ranges.Status (Query) = Ranges.Scanning loop
            Count := Count + 1; pragma Assert (Count <= 32);
            Ranges.Step (Query, Source);
            if Count = 1 then
               pragma Assert (Ranges.Status (Query) = Ranges.Scanning);
               if Case_ID = 3 then Mutation.Change_Revision (Source); end if;
               if Case_ID = 4 then Ranges.Cancel (Query); end if;
            end if;
         end loop;
         pragma Assert (Ranges.Status (Query) =
           (case Case_ID is when 0 => Ranges.Reusable,
            when 1 | 2 => Ranges.Not_Reusable, when others => Ranges.Stale));
         if Case_ID <= 2 then
            pragma Assert (Insert.Range_Reusable (Source, Address, 3 * 4096) = (Case_ID = 0));
         end if;
      end loop;
      Ranges.Start (Query, Source, 0, 4096, OK); pragma Assert (not OK);
   end;
   Epoch := VM.Revision (Source);
   Insert.Start (State, Source, Epoch, Target,
     [16#6000000#, 16#6001000#, 16#6002000#], Write_Back, Read_Write, OK);
   pragma Assert (OK and Writes = 0);
   pragma Assert (Insert.Publication_Table (State, Source) = 0);
   while Insert.Publishing (State) loop
      Publication_Turns := Publication_Turns + 1;
      pragma Assert (Publication_Turns <= 32);
      Insert.Step (State, Source);
      pragma Assert (Insert.Publication_Table (State, Source) = 0);
      if Publication_Turns = 1 then pragma Assert (Writes = 0); end if;
      pragma Assert (VM.Revision (Source) = Epoch and VM.Root_DMA (Source) /= 0);
   end loop;
   pragma Assert (Insert.Published (State) and Publication_Turns >= 18);
   pragma Assert (OK and Writes = 3);
   Insert.Begin_Commit (State, Source, True, OK); pragma Assert (OK);
   loop
      Turns := Turns + 1; pragma Assert (Turns <= 32);
      Insert.Commit_Step (State, Source, Done);
      exit when Done;
      pragma Assert (Insert.Committing (State) and VM.Root_DMA (Source) = 0);
      pragma Assert (VM.Revision (Source) = Epoch);
   end loop;
   pragma Assert (Turns >= 18 and Writes = 3);
   pragma Assert (VM.Revision (Source) = Epoch + 1);
   pragma Assert (Insert.Publication_Table (State, Source) = 0);
   for I in 0 .. 2 loop
      pragma Assert (VM.Lookup (Source, Target + Unsigned_64 (I) * 4096) =
        Encode_Leaf (16#6000000# + Unsigned_64 (I) * 4096, Write_Back, Read_Write));
   end loop;
   for Fault in 1 .. 2 loop
      declare
         Interrupted : Insert.Controller;
      begin
         Held := True;
         Insert.Start (Interrupted, Source, VM.Revision (Source), Target - 4096,
           [16#7000000#], Write_Back, Read_Write, OK);
         pragma Assert (OK);
         Insert.Step (Interrupted, Source);
         pragma Assert (Insert.Publishing (Interrupted) and Writes = 3);
         if Fault = 1 then Held := False;
         else Mutation.Change_Revision (Source); end if;
         Insert.Step (Interrupted, Source);
         pragma Assert (not Insert.Publishing (Interrupted) and
           not Insert.Published (Interrupted) and Insert.Failed (Interrupted));
         pragma Assert (Writes = 3);
         Held := True;
         Insert.Step (Interrupted, Source);
         pragma Assert (Writes = 3 and not Insert.Published (Interrupted));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("publication/commit walk PASS: high descriptor indices, PT crossing, cached adjacent route, inter-turn owner/revision rejection, turns=" & Natural'Image (Publication_Turns) & "/" & Natural'Image (Turns));
end VM_Commit_Walk_Tests;
