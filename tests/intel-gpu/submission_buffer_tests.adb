with Ada.Text_IO;
with Intel_GPU_Extent_Directory;
with Extent_Directory_Fixture;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Submission_Buffer;
with Intel_GPU_Submission_Buffer.Updates;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Physical_Extents;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_Application_Image;
with Intel_GPU_Application_Image.Publication;
with Intel_GPU_Application_Image.Retirement;
with Intel_GPU_Application_Image.Updates;
with Intel_GPU_GGTT_Reservations;
with Intel_GPU_DMA_Cache;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Render_Control;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_PPGTT_Scratch;
procedure Submission_Buffer_Tests is
   Owner_Calls, Fail_Owner : Natural := 0;
   function Owner_Ready return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner_Calls /= Fail_Owner;
   end Owner_Ready;
   package Buffers is new Intel_GPU_Submission_Buffer (Owner_Ready);
   Pixel_Ready : Boolean := False;
   function Pixel_Owner return Boolean is (Pixel_Ready);
   function Pixel_View is new Buffers.Completed_Pixel_View (Pixel_Owner);
   package Images renames Intel_GPU_Submission_Image;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   -- Exercise the production arena's high CPU aperture, not a low-address
   -- fixture that could conceal pointer truncation in native writers.
   Base : constant Unsigned_64 := 16#7000_0000_0000#;
   Span : constant := 16#40000#;
   type Words is array (Natural range 0 .. 2 * Span / 4 - 1) of Unsigned_32;
   RAM : Words with Import, Volatile, Address => To_Address (Integer_Address (Base));
   Mapping : System.Address;
   A, B, Bad : Buffers.Buffer_State;
   OK : Boolean;
   -- Test-only construction of supervisor-owned fixtures. Production passes
   -- the actual Buffer_Memory result through unchanged.
   procedure Initialize
     (Object : in out Buffers.Buffer_State;
      CPU, DMA, Capacity, GPU, Bytes : Unsigned_64; Success : out Boolean) is
      Allocation : Intel_GPU_Buffer_Reply.Backing := (Ready => False);
   begin
      if CPU >= Base and then DMA >= CPU - Base then
         Allocation := Intel_GPU_Buffer_Reply.From_Linear (DMA, CPU, Capacity, DMA - (CPU - Base));
      end if;
      Buffers.Initialize (Object, Allocation, GPU, Bytes, Success);
   end Initialize;
begin
   Initialize (Bad, 0, 4096, Span, 4096, Images.GGTT_Bytes, OK);
   pragma Assert (not OK and Buffers.Initialized_GPU_Start (Bad) = 0);
   pragma Assert (Buffers.Retained_Boot_Root (Bad).CPU = 0);
   Mapping := Mmap (To_Address (Integer_Address (Base)), 2 * Span, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Integer_Address (Base)) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 2 * Span); begin null; end;
      end if;
      raise Program_Error with "cannot reserve independent context fixture";
   end if;
   RAM := [others => 16#A5A5A5A5#];
   Initialize (A, Base, 16#1000000#, Span, 16#2000000#, Images.GGTT_Bytes, OK);
   pragma Assert (OK and Buffers.Initialized_GPU_Start (A) = 16#2000000#);
   pragma Assert (not Pixel_View (A).Ready);
   Pixel_Ready := True;
   pragma Assert (not Pixel_View (Bad).Ready and not Pixel_View (B).Ready);
   declare
      View : constant Intel_GPU_Buffer_Reply.Backing := Pixel_View (A);
   begin
      pragma Assert (View.Ready and then View.Bytes = 16384 and then
        View.CPU_Address = Base + 16#1B000#);
      pragma Assert (Intel_GPU_Buffer_Reply.Page_Address (View, 0) = 16#101B000#);
      pragma Assert (Intel_GPU_Buffer_Reply.Page_Address (View, 16384) = 0);
   end;
   Fail_Owner := Owner_Calls + 1;
   pragma Assert (not Pixel_View (A).Ready);
   Fail_Owner := Owner_Calls + 2;
   pragma Assert (not Pixel_View (A).Ready);
   Fail_Owner := 0;
   pragma Assert (Buffers.Initialized_GPU_Start (B) = 0);
   for I in Images.Byte_Count / 4 .. RAM'Last loop
      pragma Assert (RAM (I) = 16#A5A5A5A5#);
   end loop;
   Initialize (B, Base + Span, 16#1100000#, Images.Byte_Count,
                       16#2100000#, Images.GGTT_Bytes, OK);
   pragma Assert (OK and Buffers.Initialized_GPU_Start (B) = 16#2100000#);
   -- Bootstrap VM metadata survives Initialize returning. Candidate creation
   -- must not touch the registered root or any other byte of live backing.
   declare
      package VM renames Buffers.VM;
      Candidate, Denied, Overlapping, Unprepared : VM.Image;
      Fresh : VM.Backing_Pages;
      Saved : constant Words := RAM;
      Root : constant Buffers.Root_Mapping := Buffers.Retained_Boot_Root (A);
   begin
      pragma Assert (Root.CPU = Base + 16#14000#);
      pragma Assert (Root.DMA = 16#1014000#);
      for P in VM.Page_Number loop
         Fresh (P) := 16#4000000# + Unsigned_64 (P) * 4096;
      end loop;
      Buffers.Prepare_Boot_Update (Bad, Unprepared, Fresh, OK);
      pragma Assert (not OK and VM.Used (Unprepared) = 0);
      Fail_Owner := Owner_Calls + 1;
      Buffers.Prepare_Boot_Update (A, Denied, Fresh, OK);
      pragma Assert (not OK and VM.Used (Denied) = 0);
      Fail_Owner := 0;
      Buffers.Prepare_Boot_Update (A, Candidate, Fresh, OK);
      pragma Assert (OK and not VM.Sealed (Candidate));
      pragma Assert (VM.Root_DMA (Candidate) = Fresh (1));
      pragma Assert (VM.Lookup (Candidate, Images.Batch_VA) = 16#1018003#);
      pragma Assert (VM.Lookup (Candidate, Images.Completion_VA) = 16#1019003#);
      -- Engine HWSP at DMA101A000 must not appear in private PPGTT.
      pragma Assert (VM.Lookup (Candidate, Images.Offscreen_VA) = 16#101B003#);
      pragma Assert (VM.Lookup (Candidate, Images.Batch_VA + 8 * 4096) = 0);
      Fresh (1) := Root.DMA;
      Buffers.Prepare_Boot_Update (A, Overlapping, Fresh, OK);
      pragma Assert (not OK);
      for I in RAM'Range loop pragma Assert (RAM (I) = Saved (I)); end loop;
      pragma Assert (Buffers.Retained_Boot_Root (A).DMA = Root.DMA);
   end;
   declare
      IA : constant Images.Image := Images.Build (16#1000000#, 16#2000000#);
      IB : constant Images.Image := Images.Build (16#1100000#, 16#2100000#);
   begin
      for I in IA.Words'Range loop
         pragma Assert (RAM (I) = IA.Words (I));
         pragma Assert (RAM (Span / 4 + I) = IB.Words (I));
      end loop;
   end;
   Initialize (A, Base + Span, 16#1100000#, Span, 16#2200000#, Images.GGTT_Bytes, OK);
   pragma Assert (not OK and Buffers.Initialized_GPU_Start (A) = 16#2000000#);
   Initialize (Bad, Base, 16#1000000#, Span, 16#2000000#, Images.GGTT_Bytes, OK);
   pragma Assert (not OK); -- failed attempt cannot be reused
   -- A real RAM publication of one bootstrap update, retaining the exact
   -- registered root. Exclusion/flush callbacks are host fixtures, not GPU
   -- completion or TLB evidence.
   for Case_ID in 0 .. 4 loop
      declare
         State : Buffers.Buffer_State;
         package VM renames Buffers.VM;
         Candidate, After_Attempt : VM.Image;
         Fresh : VM.Backing_Pages;
         Exclusive : Boolean := Case_ID /= 1;
         Flushes : Natural := 0;
         function Held return Boolean is (Exclusive);
         function Flush (CPU : Unsigned_64) return Boolean is
         begin
            Flushes := Flushes + 1;
            return not (Case_ID = 4 and then Flushes = 5) and then
              Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096);
         end Flush;
         package Updates is new Buffers.Updates (Held, Flush);
         Pages : Updates.Tables.Mappings;
         Old : constant Images.Image := Images.Build (16#1000000#, 16#2000000#);
         Root : Buffers.Root_Mapping;
         Saved_Flushes : Natural;
         Value : Unsigned_64;
      begin
         RAM := [others => 16#A5A5A5A5#];
         Initialize (State, Base, 16#1000000#, Span,
                     16#2000000#, Images.GGTT_Bytes, OK);
         pragma Assert (OK);
         Root := Buffers.Retained_Boot_Root (State);
         for P in VM.Page_Number loop
            Fresh (P) := 16#4000000# + Unsigned_64 (P - 1) * 4096;
            Pages (P) := (Base + Span + Unsigned_64 (P - 1) * 4096, Fresh (P));
         end loop;
         if Case_ID = 3 then
            -- Context storage is not itself a boot VM leaf/table, but is
            -- still forbidden as candidate table backing.
            Fresh (4) := 16#1000000#;
            Pages (4).DMA := Fresh (4);
         end if;
         Buffers.Prepare_Boot_Update (State, Candidate, Fresh, OK);
         pragma Assert (OK);
         VM.Map_Page (Candidate, Images.Batch_VA + 8 * 4096,
           16#1000000# + Intel_GPU_Submission_Backing.Offsets
             (Intel_GPU_Submission_Backing.Batch_Buffer) - Intel_GPU_Submission_Backing.First,
           Intel_GPU_ADLN_PPGTT.Write_Back,
           Intel_GPU_ADLN_PPGTT.Read_Write, OK);
         pragma Assert (OK);
         -- Match the native boot probe: a second VA for the existing batch,
         -- not a fresh unrelated data page. Both aliases must retain exactly
         -- the same cache/access encoding through stable-root publication.
         pragma Assert (VM.Lookup (Candidate, Images.Batch_VA + 8 * 4096) =
                        VM.Lookup (Candidate, Images.Batch_VA));
         VM.Seal (Candidate, OK); pragma Assert (OK);
         if Case_ID = 2 then Pages (4).CPU := Base; end if;
         Updates.Publish_Boot_Tables (State, Candidate, Pages, OK);
         pragma Assert (OK = (Case_ID = 0));
         pragma Assert (Updates.Failed (State) = (Case_ID /= 0));
         pragma Assert (Buffers.Retained_Boot_Root (State).DMA = Root.DMA);
         if Case_ID in 1 .. 3 then pragma Assert (Flushes = 0); end if;
         if Case_ID = 0 then
            pragma Assert (Flushes = 5);
            for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
               Value := Unsigned_64 (RAM (16#14000# / 4 + 2 * I)) or
                 Shift_Left (Unsigned_64 (RAM (16#14000# / 4 + 2 * I + 1)), 32);
               pragma Assert (Value = VM.Entry_Value (Candidate, 1, I));
            end loop;
         end if;
         -- Only the registered root in the old allocation may change.
         for I in Old.Words'Range loop
            if I not in 16#14000# / 4 .. 16#15000# / 4 - 1 then
               pragma Assert (RAM (I) = Old.Words (I));
            end if;
         end loop;
         Saved_Flushes := Flushes;
         Exclusive := True;
         Updates.Publish_Boot_Tables (State, Candidate, Pages, OK);
         pragma Assert (not OK and Flushes = Saved_Flushes);
         Buffers.Prepare_Boot_Update (State, After_Attempt, Fresh, OK);
         pragma Assert (not OK and VM.Used (After_Attempt) = 0);
      end;
   end loop;
   for Failure in 1 .. 18 loop
      declare Interrupted : Buffers.Buffer_State; begin
         Owner_Calls := 0; Fail_Owner := Failure;
         RAM := [others => 16#A5A5A5A5#];
         Initialize (Interrupted, Base, 16#1000000#, Span,
           16#2000000#, Images.GGTT_Bytes, OK);
         pragma Assert (not OK and Buffers.Initialized_GPU_Start (Interrupted) = 0);
         pragma Assert (Buffers.Retained_Boot_Root (Interrupted).CPU = 0);
         for I in Images.Byte_Count / 4 .. RAM'Last loop
            pragma Assert (RAM (I) = 16#A5A5A5A5#);
         end loop;
      end;
   end loop;
   Fail_Owner := 0;
   for Fault in 1 .. 8 loop
      declare
         Rejected : Buffers.Buffer_State;
         Allocation : Intel_GPU_Buffer_Reply.Backing :=
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#);
      begin
         case Fault is
            when 1 => Allocation := (Ready => False);
            when 2 => Allocation := Intel_GPU_Buffer_Reply.From_Linear
              (Unsigned_64'Last, Base, Span, 16#1000000#);
            when 3 => Allocation.CPU_Address := Base + 1;
            when 4 => Allocation.Bytes := Span - 1;
            when 5 => Allocation.Bytes := 16#1001000#;
            when 6 => Allocation.View := Intel_GPU_Buffer_Reply.Slice (Allocation.View, 4096, 4096);
            when 7 => Allocation.Bytes := 4096;
            when 8 => Allocation.CPU_Address := Unsigned_64'Last;
            when others => null;
         end case;
         RAM := [others => 16#A5A5A5A5#];
         Buffers.Initialize (Rejected, Allocation, 16#2000000#, Images.GGTT_Bytes, OK);
         pragma Assert (not OK and Buffers.Initialized_GPU_Start (Rejected) = 0);
         for Word of RAM loop pragma Assert (Word = 16#A5A5A5A5#); end loop;
      end;
   end loop;
   -- Application VM path never falls back to bootstrap tables/batch, and
   -- validates the root against the entire retained allocation.
   for Case_ID in 0 .. 4 loop
      declare
         State : Buffers.Buffer_State;
         Allocation : constant Intel_GPU_Buffer_Reply.Backing :=
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#);
         Root : constant Unsigned_64 :=
           (case Case_ID is when 0 => 16#4000000#, when 1 => 0,
            when 2 => 16#1000000#, when 3 => 16#1000000# + Images.Byte_Count,
            when others => 16#4000001#);
         Expected : constant Images.Image :=
           Images.Build_For_VM (16#1000000#, 16#2000000#, Root);
      begin
         RAM := [others => 16#A5A5A5A5#];
         Buffers.Initialize_For_VM (State, Allocation, 16#2000000#,
                                   Images.GGTT_Bytes, Root, OK);
         pragma Assert (OK = (Case_ID = 0));
         pragma Assert (Buffers.Retained_Boot_Root (State).CPU = 0);
         pragma Assert (not Pixel_View (State).Ready);
         declare
            Candidate : Buffers.VM.Image;
            Fresh : constant Buffers.VM.Backing_Pages :=
              [16#5000000#, 16#5001000#, 16#5002000#, 16#5003000#];
            Accepted : Boolean;
         begin
            Buffers.Prepare_Boot_Update (State, Candidate, Fresh, Accepted);
            pragma Assert (not Accepted);
         end;
         if OK then
            for I in Expected.Words'Range loop
               pragma Assert (RAM (I) = Expected.Words (I));
            end loop;
         else
            pragma Assert (Buffers.Initialized_GPU_Start (State) = 0);
            for Word of RAM loop pragma Assert (Word = 16#A5A5A5A5#); end loop;
         end if;
         for I in Images.Byte_Count / 4 .. RAM'Last loop
            pragma Assert (RAM (I) = 16#A5A5A5A5#);
         end loop;
         Buffers.Initialize_For_VM (State, Allocation, 16#2000000#,
                                   Images.GGTT_Bytes, 16#4000000#, OK);
         pragma Assert (not OK);
      end;
   end loop;
   for Failure in 1 .. 4 loop
      declare State : Buffers.Buffer_State; begin
         Owner_Calls := 0; Fail_Owner := Failure;
         Buffers.Initialize_For_VM
           (State, Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
            16#2000000#, Images.GGTT_Bytes, 16#4000000#, OK);
         pragma Assert (not OK and Buffers.Initialized_GPU_Start (State) = 0);
      end;
   end loop;
   Fail_Owner := 0;
   -- Compose an external sealed VM with its context image. Reject aliases
   -- in a child or unused reserved table, not merely at the root.
   declare
      package VM is new Intel_GPU_VM_Image (5);
      function Flush (CPU : Unsigned_64) return Boolean is
        (Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096));
      package Application is new Intel_GPU_Application_Image (VM, Owner_Ready, Flush);
      Exclusive : Boolean := True;
      function Held return Boolean is (Exclusive);
      package Updates is new Application.Updates (Held);
   begin
      -- One supervisor allocation supplies disjoint retained context/table
      -- slices; no second allocation ticket or application-visible handle.
      for With_Scratch in Boolean loop
      for Update_Case in 0 .. 3 loop
      declare
         Parent : constant Intel_GPU_Buffer_Reply.Backing :=
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, 2 * Span, 16#1000000#);
         Context : constant Intel_GPU_Buffer_Reply.Backing :=
           Intel_GPU_Buffer_Reply.Slice (Parent, 0, Span);
         Table_Memory : constant Intel_GPU_Buffer_Reply.Backing :=
           Intel_GPU_Buffer_Reply.Slice (Parent, Span, 5 * 4096);
         Pages : VM.Backing_Pages;
         Backing : Application.Tables.Mappings;
         Scratch : Application.Tables.Scratch_Mappings := [others => (0, 0)];
         Descriptor : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0];
         Source, Candidate : VM.Image;
         State : Application.State;
         package Control renames Intel_GPU_Render_Control;
         Admission : Control.Controller;
         function Resolve (Sender, Stamp : Unsigned_64) return Unsigned_64 is
           (Control.Resolve (Admission, Sender, Stamp));
         package Requests is new Intel_GPU_Buffer_Requests (Resolve, Owner_Ready);
         package Binding is new Requests.Binding (VM);
         Registry : Requests.Service;
         Reply : Requests.Words;
         Admission_Reply : Control.Words;
         Ticket : Requests.Ticket;
         Session, Handle : Unsigned_64;
         Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
      begin
         pragma Assert (Context.Ready and Table_Memory.Ready);
         for P in VM.Page_Number loop
            Pages (P) := Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0) + Unsigned_64 (P - 1) * 4096;
            Backing (P) := (Table_Memory.CPU_Address + Unsigned_64 (P - 1) * 4096,
                            Pages (P));
         end loop;
         if With_Scratch then
            for L in Descriptor'Range loop
               Descriptor (L) := 16#1000000# + Span + (16 + Unsigned_64 (L)) * 4096;
               Scratch (L) := (Base + Span + (16 + Unsigned_64 (L)) * 4096, Descriptor (L));
            end loop;
         end if;
         VM.Initialize (Source, Pages, OK, Descriptor); pragma Assert (OK);
         Control.Bind (Admission, 9, 77);
         Control.Handle (Admission, 9, 77, True, Control.Label, 4, 0, 0,
           [1, Identity, 0, Control.Reserve], Admission_Reply);
         pragma Assert (Admission_Reply (0) = Control.OK);
         Session := Admission_Reply (2);
         Control.Handle (Admission, 9, 77, True, Control.Label, 4, 0, 0,
           [1, Identity, Session, Control.Activate], Admission_Reply,
           Recipient_Ready => True); -- mocked sharing-endpoint check
         pragma Assert (Admission_Reply (0) = Control.OK);
         Requests.Handle (Registry, 42, Session, Requests.Label, 4, 0, 0,
           [1, Requests.Create, 8192, 0], Reply, Ticket);
         pragma Assert (Ticket /= 0);
         -- Supervisor allocation completion fixture; the application never
         -- supplies the physical page used by the materializer below.
         Requests.Complete (Registry, Ticket,
           Intel_GPU_Buffer_Reply.From_Linear (16#5000000#, Base + 16#60000#, 8192,
            16#5000000# - 16#60000#), Reply, OK);
         pragma Assert (OK and Reply (0) = Requests.OK);
         Handle := Reply (2);
         Binding.Handle (Registry, Source, Session, 43, Session,
           Binding.Bind_Label, 4, 0, 0, [1, Handle, 4096, 4096], Reply);
         pragma Assert (Reply (0) = Requests.Denied and VM.Lookup (Source, 4096) = 0);
         Binding.Handle (Registry, Source, Session, 42, Session,
           Binding.Bind_Label, 4, 0, 0, [1, Handle, 4096, 4096], Reply);
         pragma Assert (Reply (0) = Requests.OK);
         pragma Assert (VM.Lookup (Source, 4096) = 16#5000003#);
         pragma Assert (VM.Lookup (Source, Images.Batch_VA) = 0);
         pragma Assert (VM.Lookup (Source, Images.Completion_VA) = 0);
         pragma Assert (VM.Lookup (Source, Images.Offscreen_VA) = 0);
         VM.Seal (Source, OK); pragma Assert (OK);
         RAM := [others => 16#A5A5A5A5#];
         Application.Prepare (State, Source, Backing, Context,
                              16#2000000#, Images.GGTT_Bytes, OK, Scratch);
         pragma Assert (OK and Application.GPU_Start (State) = 16#2000000#);
         pragma Assert (Application.Retained_Root (State).CPU = Table_Memory.CPU_Address
                        and Application.Retained_Root (State).DMA = Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0));
         for I in (Span + 5 * 4096) / 4 .. RAM'Last loop
            if not With_Scratch or else I not in
              (Span + 16 * 4096) / 4 .. (Span + 20 * 4096) / 4 - 1
            then pragma Assert (RAM (I) = 16#A5A5A5A5#); end if;
         end loop;
         if With_Scratch then
            -- Model GPU stores; update must preserve data, not zero it again.
            RAM ((Span + 16 * 4096) / 4) := 16#BADC0DE#;
         end if;
         -- New tables occupy a disjoint portion of the same host fixture;
         -- context/root remain at their original retained addresses.
         for P in VM.Page_Number loop
            Pages (P) := Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0) + 8 * 4096 + Unsigned_64 (P - 1) * 4096;
            Backing (P) := (Table_Memory.CPU_Address + 8 * 4096 + Unsigned_64 (P - 1) * 4096,
                            Pages (P));
         end loop;
         if Update_Case = 2 then Pages (5) := Intel_GPU_Buffer_Reply.Page_Address (Context, 0) + 4096; end if;
         VM.Prepare_Update (Candidate, Source, Pages, OK); pragma Assert (OK);
         VM.Map_Page (Candidate, 8192,
           (if Update_Case = 3 then Intel_GPU_Buffer_Reply.Page_Address (Context, 0) else 16#5001000#),
           Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK);
         pragma Assert (OK); VM.Seal (Candidate, OK); pragma Assert (OK);
         if Update_Case = 1 then Backing (4).CPU := Context.CPU_Address + 4096; end if;
         Updates.Publish_Tables (State, Source, Candidate, Backing, OK);
         pragma Assert (OK = (Update_Case = 0));
         if With_Scratch then
            pragma Assert (RAM ((Span + 16 * 4096) / 4) = 16#BADC0DE#);
         end if;
         pragma Assert (Updates.Failed (State) = (Update_Case /= 0));
         pragma Assert (Application.Retained_Root (State).DMA = Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0));
         declare
            Expected : constant Images.Image := Images.Build_For_VM
              (Intel_GPU_Buffer_Reply.Page_Address (Context, 0), 16#2000000#, Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0));
         begin
            for I in Expected.Words'Range loop
               pragma Assert (RAM (I) = Expected.Words (I));
            end loop;
         end;
         if Update_Case = 0 then
            declare
               package Snapshots is new VM.Snapshots;
               type Leaf_Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
               Leaf : Leaf_Words with Import, Volatile,
                 Address => To_Address (Integer_Address (Backing (4).CPU));
               Next_Image : VM.Image;
               Next_Pages : VM.Backing_Pages;
               Next_Backing : Application.Tables.Mappings;
               Invalidations : Natural := 0;
               procedure Write_Leaf
                 (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
                  Expected, Replacement : Unsigned_64; Success : out Boolean) is
               begin
                  Updates.Remove_Leaf (State, Source, Backing, Table_DMA, Index,
                    Expected, Replacement, Success);
               end Write_Leaf;
               procedure Invalidate (Success : out Boolean) is
               begin
                  pragma Assert (VM.Lookup (Source, 4096) = 16#5000003#);
                  pragma Assert (Leaf (1) = VM.Scratch_Entry (Source, 1));
                  Invalidations := Invalidations + 1;
                  Success := True; -- simulated completion, not Intel HW
               end Invalidate;
               package Removal is new VM.Removal (Held, Write_Leaf, Invalidate);
               Removal_State : Removal.Controller;
               procedure Insert_Word
                 (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
                  Expected, Replacement : Unsigned_64; Success : out Boolean) is
               begin
                  Updates.Insert_Leaf (State, Source, Backing, Table_DMA, Index,
                    Expected, Replacement, Success);
               end Insert_Word;
               procedure Insert_Invalidate (Success : out Boolean) is
               begin
                  pragma Assert (VM.Lookup (Source, 4096) = 0);
                  pragma Assert (Leaf (1) = 16#5000003#);
                  Success := True;
               end Insert_Invalidate;
               package Insertion is new VM.Insertion (Held, Insert_Word, Insert_Invalidate);
               Insert_State : Insertion.Controller;
            begin
               Snapshots.Adopt_Committed (Source, Candidate, OK); pragma Assert (OK);
               Removal.Execute (Removal_State, Source, VM.Revision (Source), 4096,
                 VM.Data_Pages'[16#5000000#], OK);
               pragma Assert (OK and Invalidations = 1 and not Updates.Failed (State));
               pragma Assert (VM.Lookup (Source, 4096) = 0);
               pragma Assert (Insertion.Range_Reusable (Source, 4096, 4096));
               Insertion.Execute (Insert_State, Source, VM.Revision (Source), 4096,
                 VM.Data_Pages'[16#5000000#], Intel_GPU_ADLN_PPGTT.Write_Back,
                 Intel_GPU_ADLN_PPGTT.Read_Write, OK);
               pragma Assert (OK and not Updates.Failed (State));
               pragma Assert (not Insertion.Range_Reusable (Source, 4096, 4096));
               Removal.Execute (Removal_State, Source, VM.Revision (Source), 4096,
                 VM.Data_Pages'[16#5000000#], OK);
               pragma Assert (OK and Invalidations = 2 and VM.Lookup (Source, 4096) = 0);
               -- Historical allocation receipt is not rewritten as live state.
               pragma Assert (VM.Lookup (Candidate, 4096) = 16#5000003#);
               pragma Assert (Leaf (2) = 16#5001003#);
               for P in VM.Page_Number loop
                  Next_Pages (P) := Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0)
                    + 24 * 4096 + Unsigned_64 (P - 1) * 4096;
                  Next_Backing (P) :=
                    (Table_Memory.CPU_Address + 24 * 4096 + Unsigned_64 (P - 1) * 4096,
                     Next_Pages (P));
               end loop;
               VM.Prepare_Update (Next_Image, Source, Next_Pages, OK); pragma Assert (OK);
               VM.Map_Page (Next_Image, 4096, 16#5000000#,
                 Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK);
               pragma Assert (OK);
               VM.Seal_Update (Next_Image, OK); pragma Assert (OK);
               Updates.Publish_Tables (State, Source, Next_Image, Next_Backing, OK);
               pragma Assert (OK and not Updates.Failed (State));
               pragma Assert (Application.Retained_Root (State).DMA =
                 Intel_GPU_Buffer_Reply.Page_Address (Table_Memory, 0));
               declare
                  New_Leaf : Leaf_Words with Import, Volatile,
                    Address => To_Address (Integer_Address (Next_Backing (4).CPU));
               begin
                  pragma Assert (New_Leaf (1) = 16#5000003# and New_Leaf (2) = 16#5001003#);
               end;
            end;
            -- A directory cannot be erased by calling the leaf-only API.
            Updates.Remove_Leaf (State, Candidate, Backing, Pages (2), 0,
              VM.Entry_Value (Candidate, 2, 0), VM.Scratch_Entry (Candidate, 1), OK);
            pragma Assert (not OK and Updates.Failed (State));
         end if;
         Exclusive := False;
         Updates.Publish_Tables (State, Candidate, Source, Backing, OK);
         pragma Assert (not OK and Updates.Failed (State));
         Exclusive := True;
         Updates.Publish_Tables (State, Candidate, Source, Backing, OK);
         pragma Assert (not OK);
      end;
      end loop;
      end loop;
      for Scenario in 0 .. 24 + Span / 4096 loop
         declare
            Source : VM.Image;
            Pages : VM.Backing_Pages := [16#4000000#, 16#4001000#,
              16#4002000#, 16#4003000#, 16#4004000#];
            Backing : Application.Tables.Mappings;
            State : Application.State;
            Expected : constant Images.Image :=
              Images.Build_For_VM (16#1000000#, 16#2000000#, Pages (1));
         begin
            if Scenario = 1 then Pages (4) := 16#1001000#; end if;
            if Scenario = 2 then Pages (5) := 16#1002000#; end if;
            for P in VM.Page_Number loop
               Backing (P) := (Base + Span + Unsigned_64 (P - 1) * 4096, Pages (P));
            end loop;
            if Scenario = 3 then Backing (4).CPU := Base + 4096; end if;
            VM.Initialize (Source, Pages, OK); pragma Assert (OK);
            VM.Map_Page (Source, 4096,
              (if Scenario >= 25 then 16#1000000# + Unsigned_64 (Scenario - 25) * 4096
               else 16#5000000#),
              Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK);
            pragma Assert (OK);
            VM.Seal (Source, OK); pragma Assert (OK);
            RAM := [others => 16#A5A5A5A5#];
            Owner_Calls := 0;
            if Scenario in 4 .. 24 then Fail_Owner := Scenario - 3; end if;
            Application.Prepare (State, Source, Backing,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
              16#2000000#, Images.GGTT_Bytes, OK);
            Fail_Owner := 0;
            pragma Assert (OK = (Scenario = 0));
            pragma Assert (Application.GPU_Start (State) =
              (if OK then 16#2000000# else 0));
            pragma Assert (Application.Retained_Root (State).CPU =
              (if OK then Backing (1).CPU else 0));
            pragma Assert (Application.Retained_Root (State).DMA =
              (if OK then Backing (1).DMA else 0));
            if OK then
               pragma Assert (Owner_Calls = 21);
               for I in Expected.Words'Range loop
                  pragma Assert (RAM (I) = Expected.Words (I));
               end loop;
            elsif Scenario <= 4 or else Scenario >= 25 then
               for Word of RAM loop pragma Assert (Word = 16#A5A5A5A5#); end loop;
            end if;
            -- No preparation stage may write outside context/table extents.
            for I in Images.Byte_Count / 4 .. Span / 4 - 1 loop
               pragma Assert (RAM (I) = 16#A5A5A5A5#);
            end loop;
            for I in (Span + 4 * 4096) / 4 .. RAM'Last loop
               pragma Assert (RAM (I) = 16#A5A5A5A5#);
            end loop;
            Application.Prepare (State, Source, Backing,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
              16#2000000#, Images.GGTT_Bytes, OK);
            pragma Assert (not OK);
         end;
      end loop;
   end;
   declare
      package VM is new Intel_GPU_VM_Image (4);
      function Flush (CPU : Unsigned_64) return Boolean is
        (Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096));
      Session_Open : Boolean := True;
      function Preparation_Owner return Boolean is
        (Session_Open and then Owner_Ready);
      package Application is new Intel_GPU_Application_Image (VM, Preparation_Owner, Flush);
      GPU : constant Unsigned_64 := 16#2000000#;
      PTEs : array (0 .. 19) of Unsigned_64 := [others => 0];
      Writes : Natural := 0;
      Flush_OK : Boolean := True;
      Read_OK, Lose_On_Invalidate : Boolean := True;
      Fail_Write, Saved_Writes : Natural := 0;
      function Allowed (First, Bytes : Unsigned_64) return Boolean is
        (First >= GPU and then First < GPU + Images.GGTT_Bytes and then
         Bytes <= GPU + Images.GGTT_Bytes - First);
      procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                          Success : out Boolean) is
      begin Value := PTEs (Natural (Index - GPU / 4096)); Success := Read_OK; end Read_PTE;
      procedure Write_PTE (Index, Value : Unsigned_64; Success : out Boolean) is
         Expected : constant Images.Image := Images.Build_For_VM (16#1000000#, GPU, 16#4000000#);
      begin
         -- Actual context bytes must already be initialized before MMIO.
         for I in Expected.Words'Range loop pragma Assert (RAM (I) = Expected.Words (I)); end loop;
         Writes := Writes + 1;
         PTEs (Natural (Index - GPU / 4096)) := Value; Success := Writes /= Fail_Write;
      end Write_PTE;
      procedure Invalidate (Success : out Boolean) is
      begin
         pragma Assert (Writes = 20); Success := Flush_OK;
         if Lose_On_Invalidate then Fail_Owner := Owner_Calls + 1; end if;
      end Invalidate;
      package Publication is new Application.Publication (Allowed, Read_PTE, Write_PTE, Invalidate);
      use type Publication.Result;
      Retirement_Allowed : Boolean := True;
      function Retire_Gate (First, Bytes : Unsigned_64) return Boolean is
        (Retirement_Allowed and then Allowed (First, Bytes));
      procedure Invalidate_Retirement (Success : out Boolean) is
      begin
         pragma Assert (Writes = 40);
         Success := Flush_OK;
      end Invalidate_Retirement;
      package Retirement is new Application.Retirement
        (Retire_Gate, Read_PTE, Write_PTE, Invalidate_Retirement);
      use type Retirement.Result;
      function Exclusive return Boolean is (True);
      package Updates is new Application.Updates (Exclusive);
   begin
      for Scenario in 0 .. 28 loop
         declare
            Source : VM.Image;
            Backing : Application.Tables.Mappings;
            State : Application.State;
            Ledger : Intel_GPU_GGTT_Reservations.Ledger;
            Status : Publication.Result;
            Retired : Retirement.Result;
            Saved_Root : Application.Tables.Page_Mapping;
         begin
            Session_Open := True;
            Writes := 0; PTEs := [others => 0]; Flush_OK := Scenario /= 1;
            Read_OK := Scenario /= 2; Lose_On_Invalidate := Scenario = 23;
            Fail_Write := (if Scenario in 3 .. 22 then Scenario - 2 else 0);
            Fail_Owner := 0;
            VM.Initialize (Source, [16#4000000#, 16#4001000#, 16#4002000#, 16#4003000#], OK);
            pragma Assert (OK);
            VM.Map_Page (Source, 4096, 16#5000000#, Intel_GPU_ADLN_PPGTT.Write_Back,
                         Intel_GPU_ADLN_PPGTT.Read_Write, OK); pragma Assert (OK);
            VM.Seal (Source, OK); pragma Assert (OK);
            for P in VM.Page_Number loop
               Backing (P) := (Base + Span + Unsigned_64 (P - 1) * 4096,
                               16#4000000# + Unsigned_64 (P - 1) * 4096);
            end loop;
            Intel_GPU_GGTT_Reservations.Admit (Ledger, 8 * 16384, GPU, Images.GGTT_Bytes, OK);
            pragma Assert (OK);
            Publication.Publish (State, Source, Backing,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#), Ledger, Status);
            pragma Assert (Status = (if Scenario = 0 or Scenario >= 24 then Publication.Published
                                    elsif Scenario = 2 then Publication.Mapping_Failed
                                    else Publication.Quarantined));
            pragma Assert (Publication.GPU_Address (State) =
              (if Scenario = 0 or Scenario >= 24 then GPU else 0));
            pragma Assert (Intel_GPU_GGTT_Reservations.Count (Ledger) = 1);
            pragma Assert (Writes = (if Scenario = 2 then 0 elsif Fail_Write /= 0 then Fail_Write else 20));
            Saved_Writes := Writes;
            Publication.Publish (State, Source, Backing,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#), Ledger, Status);
            pragma Assert (Status = Publication.Rejected and Writes = Saved_Writes);
            Fail_Owner := 0;
            -- Cleanup uses retained driver authority after application
            -- admission closes, not the now-false preparation authority.
            Session_Open := Scenario < 27;
            if not Session_Open then pragma Assert (not Preparation_Owner); end if;
            Retirement_Allowed := Scenario /= 24 and Scenario /= 28;
            Flush_OK := Scenario /= 25;
            Read_OK := Scenario /= 26;
            Retirement.Forget_Backing_Receipt (State, GPU, Backing (1), True, OK);
            pragma Assert (not OK); -- supervisor flag alone cannot bypass GPU retirement
            Retirement.Execute (State, Ledger, 16#6000000#, Retired);
            pragma Assert (Retired = (if Scenario = 0 or Scenario = 27 then Retirement.Address_Released
              elsif Scenario = 25 then Retirement.Quarantined
              else Retirement.Rejected));
            if Scenario = 0 or Scenario >= 24 then
               pragma Assert (Application.GPU_Start (State) = 0);
               pragma Assert (Publication.GPU_Address (State) = 0);
               -- The retained root is still a receipt, never refunded.
               pragma Assert (Application.Retained_Root (State).DMA = Backing (1).DMA);
               Updates.Publish_Tables (State, Source, Source, Backing, OK);
               pragma Assert (not OK);
            end if;
            if Scenario = 0 or Scenario = 27 then
               for PTE of PTEs loop pragma Assert (PTE = 16#6000001#); end loop;
               pragma Assert (Intel_GPU_GGTT_Reservations.Count (Ledger) = 0);
               pragma Assert (Intel_GPU_GGTT_Reservations.Space_Free (Ledger, GPU, Images.GGTT_Bytes));
               -- Another allocation may now claim this VA. The old image
               -- must never republish, update or retire the new occupant.
               declare
                  Claim : Intel_GPU_GGTT_Reservations.Result;
                  use type Intel_GPU_GGTT_Reservations.Result;
               begin
                  Intel_GPU_GGTT_Reservations.Reserve (Ledger, GPU, Images.GGTT_Bytes, Claim);
                  pragma Assert (Claim = Intel_GPU_GGTT_Reservations.Reserved);
               end;
            end if;
            if Scenario = 28 then
               pragma Assert (Writes = 20);
               for P in PTEs'Range loop
                  pragma Assert (PTEs (P) = 16#1000001# + Unsigned_64 (P) * 4096);
               end loop;
            end if;
            pragma Assert (Intel_GPU_GGTT_Reservations.Count (Ledger) = 1);
            Saved_Writes := Writes;
            Retirement.Execute (State, Ledger, 16#6000000#, Retired);
            pragma Assert (Retired = Retirement.Rejected and Writes = Saved_Writes);
            pragma Assert (Intel_GPU_GGTT_Reservations.Count (Ledger) = 1);
            pragma Assert (Intel_GPU_GGTT_Reservations.Has_Claim (Ledger, GPU, Images.GGTT_Bytes));
            pragma Assert (Intel_GPU_GGTT_Reservations.Valid (Ledger));
            Publication.Publish (State, Source, Backing,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#), Ledger, Status);
            pragma Assert (Status = Publication.Rejected and Writes = Saved_Writes);
            Retirement.Forget_Backing_Receipt (State, GPU, Backing (1), False, OK);
            pragma Assert (not OK);
            Retirement.Forget_Backing_Receipt (State, GPU + 4096, Backing (1), True, OK);
            pragma Assert (not OK);
            Retirement.Forget_Backing_Receipt (State, GPU,
              (Backing (1).CPU + 4096, Backing (1).DMA), True, OK);
            pragma Assert (not OK);
            Retirement.Forget_Backing_Receipt (State, GPU,
              (Backing (1).CPU, Backing (1).DMA + 4096), True, OK);
            pragma Assert (not OK);
            Saved_Root := Application.Retained_Root (State);
            Retirement.Forget_Backing_Receipt (State, GPU, Backing (1), True, OK);
            pragma Assert (OK = (Scenario = 0 or Scenario = 27));
            if OK then
               pragma Assert (Application.Retained_Root (State).CPU = 0 and
                              Application.Retained_Root (State).DMA = 0);
               pragma Assert (Application.GPU_Start (State) = 0 and Publication.GPU_Address (State) = 0);
               Updates.Publish_Tables (State, Source, Source, Backing, OK);
               pragma Assert (not OK);
               Publication.Publish (State, Source, Backing,
                 Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#), Ledger, Status);
               pragma Assert (Status = Publication.Rejected and Writes = Saved_Writes);
               Retirement.Forget_Backing_Receipt (State, GPU, Backing (1), True, OK);
               pragma Assert (not OK);
            else
               pragma Assert (Application.Retained_Root (State).CPU = Saved_Root.CPU and
                              Application.Retained_Root (State).DMA = Saved_Root.DMA);
            end if;
            pragma Assert (Writes = Saved_Writes);
            pragma Assert (Intel_GPU_GGTT_Reservations.Has_Claim (Ledger, GPU, Images.GGTT_Bytes));
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Application image retirement PASS29 paths: revoked cleanup, exact release, failure retention, same-VA stale image rejection");
   end;
   declare
      package E renames Intel_GPU_Physical_Extents;
      package Replies renames Intel_GPU_Buffer_Reply;
      Window_CPU : constant Unsigned_64 := Base + E.Block_Bytes - Images.Byte_Count;
      Window_Bytes : constant := 2 * Images.Byte_Count;
      type Window_Words is array (Natural range 0 .. Window_Bytes / 4 - 1) of Unsigned_32;
      Window : Window_Words with Import, Volatile,
        Address => To_Address (Integer_Address (Window_CPU));
      Window_Mapping : System.Address;
      Bases : E.Addresses;
      Map : Intel_GPU_Extent_Directory.Borrowed_View;
      Owner : aliased Intel_GPU_Extent_Directory.Directory;
   begin
      Window_Mapping := Mmap (To_Address (Integer_Address (Window_CPU)),
                             Window_Bytes, 3, 16#100022#, -1, 0);
      if Window_Mapping /= To_Address (Integer_Address (Window_CPU)) then
         raise Program_Error with "cannot reserve scattered context fixture";
      end if;
      for I in E.Block_Index loop
         Bases (I) := 16#40000000# - Unsigned_64 (I) * 2 * E.Block_Bytes;
      end loop;
      Extent_Directory_Fixture.Initialize (Owner, Bases);
      Map := Intel_GPU_Extent_Directory.Borrow (Owner);
      Fail_Owner := 0;
      -- Place the physical discontinuity at every interior context page
      -- boundary: context, ring, tables, batch, completion and render storage.
      for Split in 1 .. Images.Backing_Pages'Last loop
         declare
            Offset : constant Unsigned_64 := E.Block_Bytes - Unsigned_64 (Split) * 4096;
            Allocation : constant Replies.Backing := Replies.From_View
              (Replies.From_Extents (Map, 7, Offset, Images.Byte_Count));
            State : Buffers.Buffer_State;
            Pages : Images.Backing_Pages;
            First : constant Natural := Natural
              ((Allocation.CPU_Address - Window_CPU) / 4);
         begin
            Window := [others => 16#A5A5A5A5#];
            for P in Pages'Range loop
               Pages (P) := Replies.Page_Address (Allocation, Unsigned_64 (P) * 4096);
            end loop;
            pragma Assert (Pages (Split) /= Pages (Split - 1) + 4096);
            Buffers.Initialize (State, Allocation, 16#2000000#, Images.GGTT_Bytes, OK);
            pragma Assert (OK and Buffers.Initialized_GPU_Start (State) = 16#2000000#);
            pragma Assert (Buffers.Retained_Boot_Root (State).DMA = Pages (20));
            declare
               View : constant Replies.Backing := Pixel_View (State);
            begin
               pragma Assert (View.Ready and then View.Bytes = 16384 and then
                 View.CPU_Address = Allocation.CPU_Address + 16#1B000#);
               for P in 0 .. 3 loop
                  pragma Assert (Replies.Page_Address (View, Unsigned_64 (P) * 4096) =
                    Pages (27 + P));
               end loop;
               pragma Assert (Replies.Page_Address (View, 16384) = 0);
            end;
            declare Expected : constant Images.Image := Images.Build (Pages, 16#2000000#); begin
               pragma Assert (Expected.Valid);
               for I in Window'Range loop
                  pragma Assert (Window (I) =
                    (if I >= First and then I - First < Images.Byte_Count / 4
                     then Expected.Words (I - First) else 16#A5A5A5A5#));
               end loop;
            end;
         end;
      end loop;
      pragma Assert (Munmap (Window_Mapping, Window_Bytes) = 0);
      Ada.Text_IO.Put_Line ("Scattered context PASS: all32 interior physical splits, image readback and untouched guards (host RAM, NOT GPU execution)");
   end;
   for Fault in 1 .. 3 loop
      declare
         package VM is new Intel_GPU_VM_Image (4);
         Held : Boolean := True;
         Armed : Boolean := False;
         Flushes : Natural := 0;
         function Owner return Boolean is (Held);
         function Flush (CPU : Unsigned_64) return Boolean is
         begin
            if Armed then
               Flushes := Flushes + 1;
               if Fault = 2 then return False; end if;
               if Fault = 3 then Held := False; end if;
            end if;
            return Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096);
         end Flush;
         package App is new Intel_GPU_Application_Image (VM, Owner, Flush);
         package Updates is new App.Updates (Owner);
         Source : VM.Image;
         State : App.State;
         Pages : VM.Backing_Pages := [16#4000000#, 16#4001000#, 16#4002000#, 16#4003000#];
         Backing : App.Tables.Mappings;
         type Leaf_Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
         Leaf : Leaf_Words with Import, Volatile,
           Address => To_Address (Integer_Address (Base + Span + 3 * 4096));
      begin
         for P in VM.Page_Number loop
            Backing (P) := (Base + Span + Unsigned_64 (P - 1) * 4096, Pages (P));
         end loop;
         VM.Initialize (Source, Pages, OK); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#5000000#, Intel_GPU_ADLN_PPGTT.Write_Back,
           Intel_GPU_ADLN_PPGTT.Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         App.Prepare (State, Source, Backing,
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
           16#2000000#, Images.GGTT_Bytes, OK); pragma Assert (OK);
         Armed := True;
         if Fault = 1 then Leaf (1) := 16#5001003#; end if;
         Updates.Remove_Leaf (State, Source, Backing, Pages (4), 1, 16#5000003#, 0, OK);
         pragma Assert (not OK and Updates.Failed (State));
         pragma Assert (VM.Lookup (Source, 4096) = 16#5000003#);
         pragma Assert (Leaf (1) = (if Fault = 1 then 16#5001003# else 0));
         pragma Assert (Flushes = (if Fault = 1 then 0 else 1));
         -- Even restoring authority and the old word cannot revive the writer.
         Held := True; Leaf (1) := 16#5000003#; Armed := False;
         Updates.Remove_Leaf (State, Source, Backing, Pages (4), 1, 16#5000003#, 0, OK);
         pragma Assert (not OK and Updates.Failed (State) and Leaf (1) = 16#5000003#);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Retained leaf failure PASS: stale hardware word, failed flush, post-write ownership loss, permanent retry rejection (host RAM)");
   for Fault in 0 .. 6 loop
      declare
         package VM is new Intel_GPU_VM_Image (4);
         Held, Armed : Boolean := False;
         Flushes : Natural := 0;
         function Owner return Boolean is (Held);
         function Flush (CPU : Unsigned_64) return Boolean is
         begin
            if Armed then
               Flushes := Flushes + 1;
               if Fault = 2 then return False; end if;
               if Fault = 3 then Held := False; end if;
            end if;
            return Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096);
         end Flush;
         package App is new Intel_GPU_Application_Image (VM, Owner, Flush);
         package Updates is new App.Updates (Owner);
         Source : VM.Image;
         State : App.State;
         Pages : VM.Backing_Pages := [16#4000000#, 16#4001000#, 16#4002000#, 16#4003000#];
         Backing : App.Tables.Mappings;
         type Leaf_Words is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
         Leaf : Leaf_Words with Import, Volatile,
           Address => To_Address (Integer_Address (Base + Span + 3 * 4096));
         Replacement : Unsigned_64 := 16#5001003#;
         Index : Intel_GPU_ADLN_PPGTT.Table_Index := 2;
      begin
         Held := True;
         for P in VM.Page_Number loop
            Backing (P) := (Base + Span + Unsigned_64 (P - 1) * 4096, Pages (P));
         end loop;
         VM.Initialize (Source, Pages, OK); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#5000000#, Intel_GPU_ADLN_PPGTT.Write_Back,
           Intel_GPU_ADLN_PPGTT.Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         App.Prepare (State, Source, Backing,
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
           16#2000000#, Images.GGTT_Bytes, OK); pragma Assert (OK);
         Armed := True;
         case Fault is
            when 1 => Leaf (2) := 16#BAD0003#;
            when 4 => Replacement := Pages (1) + 3;
            when 5 => Replacement := 16#5001001#; -- unsupported read-only PTE
            when 6 => Index := 1; -- occupied logical leaf
            when others => null;
         end case;
         Updates.Insert_Leaf (State, Source, Backing, Pages (4), Index, 0, Replacement, OK);
         pragma Assert (OK = (Fault = 0));
         pragma Assert (Updates.Failed (State) = (Fault /= 0));
         pragma Assert (VM.Lookup (Source, 8192) = 0); -- commit belongs to coordinator
         pragma Assert (Flushes = (if Fault in 0 | 2 | 3 then 1 else 0));
         if Fault in 0 | 2 | 3 then pragma Assert (Leaf (2) = Replacement); end if;
         if Fault /= 0 then
            Held := True; Armed := False; Leaf (2) := 0;
            Updates.Insert_Leaf (State, Source, Backing, Pages (4), 2, 0, 16#5001003#, OK);
            pragma Assert (not OK and Leaf (2) = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Retained insertion writer PASS: empty leaf, stale hardware, flush/owner failure, table alias, unsupported PTE and occupied leaf (host RAM)");
   pragma Assert (Munmap (Mapping, 2 * Span) = 0);
   Ada.Text_IO.Put_Line ("Submission buffer PASS: independent one-shot mappings (host fixture)");
end Submission_Buffer_Tests;
