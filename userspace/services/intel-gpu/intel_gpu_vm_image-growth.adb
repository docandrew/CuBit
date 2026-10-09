package body Intel_GPU_VM_Image.Growth is
   use Intel_GPU_ADLN_PPGTT;
   function Inspection_State (State : Inspection) return Inspection_Status is (State.State);
   procedure Cancel_Inspection (State : in out Inspection) is
   begin
      State.State := Stale;
      State.Value := (others => <>);
   end Cancel_Inspection;
   function Matches (State : Inspection; Object : Image) return Boolean is
     (Object.Valid and then Object.Frozen = State.Frozen and then
      Root_DMA (Object) = State.Root and then Revision (Object) = State.Epoch);
   function Inspection_Result (State : Inspection; Object : Image) return Requirements is
     (if State.State = Complete and then Matches (State, Object) then State.Value
      else (others => <>));
   procedure Start_State
     (State : in out Inspection; Object : Image; GPU, Bytes : Unsigned_64;
      Frozen : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.State = Scanning then return; end if;
      State.State := Idle; State.Value := (others => <>);
      if not Object.Valid or else Object.Frozen /= Frozen or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > 2 ** 48 - GPU or else
        Bytes / 4096 > Unsigned_64 (Capacity * 512)
      then return; end if;
      State.Root := Root_DMA (Object); State.Epoch := Revision (Object);
      State.Frozen := Frozen; State.Address := GPU;
      State.Remaining := Natural (Bytes / 4096);
      State.Span := 0; State.Leaf := 0; State.Missing := 0;
      State.Depth := 1; State.Current := 1; State.Candidate := 0;
      State.Last := [others => Unsigned_64'Last];
      State.State := Scanning; Accepted := True;
   end Start_State;
   procedure Start_Inspection
     (State : in out Inspection; Object : Image; GPU, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Start_State (State, Object, GPU, Bytes, True, Accepted);
   end Start_Inspection;
   procedure Step_Inspection (State : in out Inspection; Object : Image) is
      procedure Advance_Span is
      begin
         State.Address := State.Address + Unsigned_64 (State.Span) * 4096;
         State.Remaining := State.Remaining - State.Span;
         State.Span := 0; State.Depth := 1;
         State.Current := 1; State.Candidate := 0;
         if State.Remaining = 0 then
            State.Value := (Status => Ready, Additional_Tables => State.Missing,
              Fits_Reserved => State.Missing <= Metadata_Capacity (Object) - Object.Count,
              Required_Tables => Object.Count + State.Missing,
              Fits_Quota => State.Missing <= Capacity - Object.Count);
            State.State := Complete;
         end if;
      end Advance_Span;
   begin
      if State.State /= Scanning then return; end if;
      if not Matches (State, Object) then State.State := Stale; return; end if;
      for Work in 1 .. 32 loop
         declare
            W : constant Walk := Locate (State.Address);
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            if State.Span = 0 then
               State.Span := Natural'Min (State.Remaining, 512 - Natural (W.PT));
               State.Leaf := Natural (W.PT);
            end if;
            if State.Depth = 4 then
               if Raw_Word (Object, State.Current, Table_Index (State.Leaf)) /= 0 then
                  State.Value := (Status => Occupied, others => <>);
                  State.State := Complete; return;
               end if;
               State.Leaf := State.Leaf + 1;
               if State.Leaf = Natural (W.PT) + State.Span then Advance_Span; end if;
            elsif State.Candidate = 0 then
               if Raw_Word (Object, State.Current, Route (State.Depth)) = 0 then
                  for Depth in State.Depth .. 3 loop
                     declare
                        Prefix : constant Unsigned_64 := Shift_Right (State.Address, 48 - 9 * Depth);
                     begin
                        if Prefix /= State.Last (Depth) then
                           State.Missing := State.Missing + 1;
                           State.Last (Depth) := Prefix;
                        end if;
                     end;
                  end loop;
                  Advance_Span;
               else State.Candidate := 2; end if;
            elsif State.Candidate > Object.Count then
               State.State := Complete; return; -- malformed route, invalid result
            elsif Raw_Word (Object, State.Current, Route (State.Depth)) =
              Encode_Directory (Descriptor (Object, State.Candidate).DMA)
            then
               State.Current := State.Candidate;
               State.Depth := State.Depth + 1; State.Candidate := 0;
            else State.Candidate := State.Candidate + 1; end if;
         end;
         exit when State.State /= Scanning;
      end loop;
   end Step_Inspection;
   function Inspect_State
     (Object : Image; GPU, Bytes : Unsigned_64; Frozen : Boolean) return Requirements is
      State : Inspection;
      Accepted : Boolean;
   begin
      Start_State (State, Object, GPU, Bytes, Frozen, Accepted);
      if not Accepted then return (others => <>); end if;
      while Inspection_State (State) = Scanning loop Step_Inspection (State, Object); end loop;
      return Inspection_Result (State, Object);
   end Inspect_State;
   function Inspect (Object : Image; GPU, Bytes : Unsigned_64) return Requirements is
     (Inspect_State (Object, GPU, Bytes, True));
   function Inspect_Offline (Object : Image; GPU, Bytes : Unsigned_64)
     return Offline_Requirements
   is
      Needed : constant Requirements := Inspect_State (Object, GPU, Bytes, False);
      Backed : constant Natural := Object.Backed;
      Available : Natural;
   begin
      if Needed.Status /= Ready then return (Needed, 0); end if;
      if Backed < Object.Count then return (others => <>); end if;
      Available := Backed - Object.Count;
      return (Needed, (if Needed.Additional_Tables > Available then
        Needed.Additional_Tables - Available else 0));
   end Inspect_Offline;
   function Description_State (State : Description) return Description_Status is (State.State);
   function Description_Valid (State : Description; Object : Image) return Boolean is
     (State.State = Described and then not State.Cancelled and then Matches (State.Query, Object));
   function Description_Count (State : Description; Object : Image) return Natural is
     (if Description_Valid (State, Object)
      then State.Made else 0);
   procedure Cancel_Description (State : in out Description) is
   begin
      State.Cancelled := True; State.State := Rejected;
      Cancel_Inspection (State.Query);
   end Cancel_Description;
   procedure Start_Description
     (State : in out Description; Object : Image; GPU, Bytes : Unsigned_64;
      Output_Capacity : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.State in Checking | Emitting then return; end if;
      State.State := Rejected;
      Cancel_Inspection (State.Query);
      Start_Inspection (State.Query, Object, GPU, Bytes, Accepted);
      if not Accepted then return; end if;
      State.Address := GPU;
      State.Remaining := Natural (Bytes / 4096);
      State.Limit := Output_Capacity; State.Made := 0;
      State.Depth := 1; State.Missing_Depth := 0;
      State.Current := 1; State.Candidate := 0;
      State.Last_Prefix := [others => Unsigned_64'Last];
      State.Last_Node := [others => 0]; State.Cancelled := False;
      State.State := Checking;
   end Start_Description;
   procedure Step_Description (State : in out Description; Object : Image) is
      OK : Boolean;
      function Owner_Current return Boolean is
        (not State.Cancelled and then Authorized and then not State.Cancelled
         and then Matches (State.Query, Object));
      procedure Advance_Span is
         Span : constant Positive := Natural'Min
           (State.Remaining, 512 - Natural (Locate (State.Address).PT));
      begin
         State.Address := State.Address + Unsigned_64 (Span) * 4096;
         State.Remaining := State.Remaining - Span;
         State.Depth := 1; State.Current := 1; State.Candidate := 0;
         State.Missing_Depth := 0;
         if State.Remaining = 0 then
            State.State := (if State.Made = State.Query.Value.Additional_Tables then
              Described else Rejected);
         end if;
      end Advance_Span;
   begin
      if State.State not in Checking | Emitting then return; end if;
      if not Owner_Current then Cancel_Description (State); return; end if;
      if State.State = Checking then
         Step_Inspection (State.Query, Object);
         if Inspection_State (State.Query) = Scanning then return; end if;
         if Inspection_State (State.Query) /= Complete or else
           State.Query.Value.Status /= Ready or else
           State.Query.Value.Additional_Tables > State.Limit
         then Cancel_Description (State); return; end if;
         State.State := Emitting;
         return; -- Never combine inspection and emission budgets.
      end if;
      for Work in 1 .. 32 loop
         declare
            W : constant Walk := Locate (State.Address);
            Route : constant array (1 .. 3) of Table_Index := [W.PML4, W.PDP, W.PD];
         begin
            if State.Missing_Depth /= 0 then
               declare
                  Depth : constant Positive := State.Depth;
                  Prefix : constant Unsigned_64 := Shift_Right (State.Address, 48 - 9 * Depth);
               begin
                  if Prefix /= State.Last_Prefix (Depth) then
                     if State.Made >= State.Query.Value.Additional_Tables then
                        Cancel_Description (State); return;
                     end if;
                     State.Made := State.Made + 1;
                     Emit (State.Made,
                       (Existing_Parent => (if Depth = State.Missing_Depth then State.Current else 0),
                        New_Parent => (if Depth = State.Missing_Depth then 0 else State.Last_Node (Depth - 1)),
                        Index => Route (Depth), Level => 3 - Depth), OK);
                     if not OK or else not Owner_Current then Cancel_Description (State); return; end if;
                     State.Last_Prefix (Depth) := Prefix; State.Last_Node (Depth) := State.Made;
                  end if;
               end;
               State.Depth := State.Depth + 1;
               if State.Depth = 4 then Advance_Span; end if;
            elsif State.Candidate = 0 then
               if Raw_Word (Object, State.Current, Route (State.Depth)) = 0 then
                  State.Missing_Depth := State.Depth;
               else State.Candidate := 2; end if;
            elsif State.Candidate > Object.Count then
               Cancel_Description (State); return;
            elsif Raw_Word (Object, State.Current, Route (State.Depth)) =
              Encode_Directory (Descriptor (Object, State.Candidate).DMA)
            then
               State.Current := State.Candidate;
               State.Depth := State.Depth + 1; State.Candidate := 0;
               if State.Depth = 4 then Advance_Span; end if;
            else
               State.Candidate := State.Candidate + 1;
            end if;
         end;
         exit when State.State /= Emitting;
      end loop;
   end Step_Description;
   procedure Describe_Into
     (Object : Image; GPU, Bytes : Unsigned_64; Output_Capacity : Natural;
      Count : out Natural; Accepted : out Boolean)
   is
      State : Description;
      procedure Step is new Step_Description (Authorized, Emit);
   begin
      Count := 0;
      Start_Description (State, Object, GPU, Bytes, Output_Capacity, Accepted);
      if not Accepted then return; end if;
      while Description_State (State) in Checking | Emitting loop Step (State, Object); end loop;
      Accepted := Description_Valid (State, Object);
      if Accepted then Count := Description_Count (State, Object); end if;
   end Describe_Into;
   procedure Describe
     (Object : Image; GPU, Bytes : Unsigned_64; Nodes : out Node_List;
      Count : out Natural; Accepted : out Boolean) is
      function Authorized return Boolean is (True);
      procedure Emit (Ordinal : Positive; Item : Node; OK : out Boolean) is
      begin
         Nodes (Nodes'First + (Ordinal - 1)) := Item;
         OK := True;
      end Emit;
      procedure Stream is new Describe_Into (Authorized, Emit);
   begin
      Nodes := [others => (others => <>)];
      Stream (Object, GPU, Bytes, Nodes'Length, Count, Accepted);
      if not Accepted then Nodes := [others => (others => <>)]; end if;
   end Describe;
end Intel_GPU_VM_Image.Growth;
