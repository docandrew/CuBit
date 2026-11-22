package body Intel_GPU_VM_Image.Growth.Backing is
   use Intel_GPU_ADLN_PPGTT;
   function Phase (State : Resolution) return Resolution_Phase is (State.Value);
   function Current (State : Resolution; Source : Image) return Boolean is
     (not State.Cancelled and then Sealed (Source) and then Root_DMA (Source) = State.Root
      and then Revision (Source) = State.Epoch);
   function Resolution_Valid (State : Resolution; Source : Image) return Boolean is
     (State.Value = Complete_Resolution and then Current (State, Source));
   procedure Cancel_Resolution (State : in out Resolution) is
   begin
      State.Cancelled := True; State.Value := Failed_Resolution;
      Cancel_Inspection (State.Query); Cancel_Description (State.Topology);
   end Cancel_Resolution;
   procedure Start_Resolution
     (State : in out Resolution; Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Value in Inspecting .. Resolving then return; end if;
      Cancel_Resolution (State);
      if Page_Count = 0 or else Output_Capacity < Page_Count or else
        not Valid_DMA_Page (Retained_Root) then return; end if;
      Start_Inspection (State.Query, Source, GPU, Bytes, Accepted);
      if not Accepted then return; end if;
      State.GPU := GPU; State.Bytes := Bytes; State.Retained_Root := Retained_Root;
      State.Root := Root_DMA (Source); State.Epoch := Revision (Source);
      State.Count := Page_Count; State.Limit := Output_Capacity;
      State.Cursor := 1; State.Previous := 1; State.Scan := 0; State.Child := 0;
      State.Cancelled := False; State.Value := Inspecting;
   end Start_Resolution;
   procedure Step_Resolution (State : in out Resolution; Source : Image) is
      OK : Boolean;
      function Owner_Current return Boolean is
        (Current (State, Source) and then Authorized and then Current (State, Source));
      procedure Emit_Node (Ordinal : Positive; Item : Node; Accepted : out Boolean) is
         Parent, Child : Unsigned_64;
      begin
         Accepted := False;
         if not Owner_Current then return; end if;
         Child := Read_Page (Ordinal);
         if not Owner_Current then return; end if;
         if Item.Existing_Parent = 1 then Parent := State.Retained_Root;
         elsif Item.Existing_Parent /= 0 then Parent := Page_DMA (Source, Item.Existing_Parent);
         else Parent := Read_Page (Item.New_Parent); end if;
         if not Owner_Current or else not Valid_DMA_Page (Parent) or else
           not Owned_Table (Parent) or else not Owner_Current then return; end if;
         if State.Value = Resolving then
            Emit (Ordinal,
              (Parent_DMA => Parent, Child_DMA => Child,
               Expected => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Item.Level + 1),
               Value => Encode_Directory (Child),
               Fill => Intel_GPU_PPGTT_Scratch.Fallback (Source.Scratch, Item.Level),
               Index => Item.Index), Item, Accepted);
            Accepted := Accepted and then Owner_Current;
            return;
         end if;
         Accepted := True;
      end Emit_Node;
      procedure Describe_Step is new Step_Description (Owner_Current, Emit_Node);
   begin
      if State.Value not in Inspecting .. Resolving then return; end if;
      if not Owner_Current then Cancel_Resolution (State); return; end if;
      case State.Value is
         when Inspecting =>
            Step_Inspection (State.Query, Source);
            if Inspection_State (State.Query) = Scanning then return; end if;
            declare Needed : constant Requirements := Inspection_Result (State.Query, Source); begin
               if Needed.Status /= Ready or else Needed.Additional_Tables /= State.Count or else
                 not Owned_Table (State.Retained_Root) or else not Owner_Current
               then Cancel_Resolution (State); return; end if;
            end;
            State.Value := Capturing;
         when Capturing =>
            State.Child := Read_Page (State.Cursor);
            if not Owner_Current or else not Valid_DMA_Page (State.Child) or else
              State.Child = State.Retained_Root or else not Owned_Table (State.Child) or else
              not Owner_Current then Cancel_Resolution (State); return; end if;
            State.Scan := 0; State.Value := Checking_Aliases;
         when Checking_Aliases =>
            -- Include ALL reserved tables, all scratch pages, and every raw
            -- directory/leaf word. WB leaves and directories share low flags.
            for Work in 1 .. 32 loop
               declare Address : Unsigned_64; Position : Natural; begin
                  if State.Scan < Source.Backed then
                     Address := Descriptor (Source, State.Scan + 1).DMA;
                  elsif State.Scan < Source.Backed + 4 then
                     Address := Source.Scratch (State.Scan - Source.Backed);
                  else
                     Position := State.Scan - Source.Backed - 4;
                     Address := Raw_Word (Source, Position / 512 + 1, Table_Index (Position mod 512));
                     Address := Address - Address mod 4096;
                  end if;
                  if Address = State.Child then Cancel_Resolution (State); return; end if;
               end;
               State.Scan := State.Scan + 1;
               if State.Scan = Source.Backed + 4 + Source.Count * 512 then
                  State.Previous := 1; State.Value := Checking_Duplicates; return;
               end if;
            end loop;
         when Checking_Duplicates =>
            for Work in 1 .. 32 loop
               if State.Previous = State.Cursor then
                  if State.Cursor < State.Count then
                     State.Cursor := State.Cursor + 1; State.Value := Capturing;
                  else
                     Start_Description (State.Topology, Source, State.GPU, State.Bytes, State.Limit, OK);
                     if not OK then Cancel_Resolution (State); return; end if;
                     State.Value := Preflighting;
                  end if;
                  return;
               end if;
               declare Other : constant Unsigned_64 := Read_Page (State.Previous); begin
                  if not Owner_Current or else Other = State.Child then
                     Cancel_Resolution (State); return;
                  end if;
               end;
               State.Previous := State.Previous + 1;
            end loop;
         when Preflighting | Resolving =>
            Describe_Step (State.Topology, Source);
            if not Owner_Current then Cancel_Resolution (State); return; end if;
            if Description_State (State.Topology) in Checking | Emitting then return; end if;
            if not Description_Valid (State.Topology, Source) or else
              Description_Count (State.Topology, Source) /= State.Count
            then Cancel_Resolution (State); return; end if;
            if State.Value = Preflighting then
               -- All existing parents passed before any output is exposed.
               Start_Description (State.Topology, Source, State.GPU, State.Bytes, State.Limit, OK);
               if not OK then Cancel_Resolution (State); return; end if;
               State.Value := Resolving;
            else State.Value := Complete_Resolution; end if;
         when others => null;
      end case;
   end Step_Resolution;
   procedure Resolve_Into
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean)
   is
      State : Resolution;
      function Authorized return Boolean is (True);
      procedure Step is new Step_Resolution (Authorized, Read_Page, Emit);
   begin
      Start_Resolution (State, Source, GPU, Bytes, Retained_Root, Page_Count, Output_Capacity, Accepted);
      if not Accepted then return; end if;
      while Phase (State) in Inspecting .. Resolving loop Step (State, Source); end loop;
      Accepted := Resolution_Valid (State, Source);
   end Resolve_Into;
   procedure Resolve_From_Pages
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Output : out Links; Accepted : out Boolean) is
      procedure Emit (Ordinal : Positive; Item : Link; Topology : Node; OK : out Boolean) is
         pragma Unreferenced (Topology);
      begin
         Output (Output'First + (Ordinal - 1)) := Item;
         OK := True;
      end Emit;
      procedure Stream is new Resolve_Into (Read_Page, Emit);
   begin
      Output := [others => (others => <>)];
      Stream (Source, GPU, Bytes, Retained_Root, Page_Count, Output'Length, Accepted);
      if not Accepted then Output := [others => (others => <>)]; end if;
   end Resolve_From_Pages;
   procedure Resolve
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Output : out Links; Accepted : out Boolean) is
      function Page (Ordinal : Positive) return Unsigned_64 is
        (New_Pages (New_Pages'First + (Ordinal - 1)));
      procedure Stream is new Resolve_From_Pages (Page);
   begin
      Stream (Source, GPU, Bytes, Retained_Root, New_Pages'Length, Output, Accepted);
   end Resolve;
end Intel_GPU_VM_Image.Growth.Backing;
