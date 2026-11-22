package body AML_Frame_Roots with SPARK_Mode is
   package FH renames AML_Frame_Handles;
   package R renames AML_Objects.Root_Snapshots;
   use type R.Header;
   use type AML_Object_Identifiers.Slot_Incarnation;
   function Empty (Object_Capacity : AML_Objects.Storage_Capacity := AML_Objects.Max_Objects)
      return State is (Object_Capacity => Object_Capacity, others => <>);
   function Find (Store : State; Frame : FH.Frame_Handle) return Root_Count is
   begin
      if Frame = FH.No_Frame then return 0; end if;
      for I in Root_Index loop
         if Store.Entries (I).Frame = Frame then return I; end if;
      end loop;
      return 0;
   end Find;
   -- Runtime checks scale with frame count and current roots, not heap capacity.
   -- The private dense mask is related to the touched list by Coherent.
   function Coherent (Store : State) return Boolean is
     (for all I in Root_Index => Store.Touched_Count (I) <= Store.Object_Capacity
       and then (for all J in 1 .. Store.Object_Capacity =>
         (Store.Stamps (I,J) /= AML_Object_Identifiers.No_Incarnation) =
           (for some K in 1 .. Store.Touched_Count (I) => Store.Touched (I,K) = J))
       and then (for all K in 1 .. Store.Touched_Count (I) =>
         Store.Touched (I,K) in 1 .. Store.Object_Capacity
         and then (for all L in 1 .. K-1 => Store.Touched (I,L) /= Store.Touched (I,K))));
   procedure Clear_Census (Store : in out State; Slot : Root_Index) is
   begin
      -- No per-operation work proportional to store capacity: only roots
      -- previously published by this frame are cleared. Addresses remain
      -- resolved at publication, so later namespace mutation cannot retarget them.
      for K in 1 .. Store.Touched_Count (Slot) loop
         Store.Stamps (Slot,Store.Touched (Slot,K)) := AML_Object_Identifiers.No_Incarnation;
         Store.Touched (Slot,K) := 0;
      end loop;
      Store.Touched_Count (Slot) := 0;
   end Clear_Census;
   function Valid (Store : State) return Boolean is
      Found : Root_Count := 0;
   begin
      for I in Root_Index loop
         if Store.Touched_Count (I) > Store.Object_Capacity then return False; end if;
         for K in 1 .. Store.Touched_Count (I) loop
            if Store.Touched (I,K) not in 1 .. Store.Object_Capacity
              or else Store.Stamps (I,Store.Touched (I,K)) = AML_Object_Identifiers.No_Incarnation
            then return False; end if;
         end loop;
         if Store.Entries (I).Frame = FH.No_Frame then
            if Store.Entries (I) /= Entry_Data'(others => <>) then return False; end if;
            if Store.Touched_Count (I) /= 0 then return False; end if;
         else
            if not FH.Present (Store.Entries (I).Frame) then return False; end if;
            for J in Root_Index'First .. I - 1 loop
               if Store.Entries (J).Frame = Store.Entries (I).Frame then return False; end if;
            end loop;
            for C in FH.Cell_ID loop
               if not Store.Entries (I).Initialized (C) and then Store.Entries (I).Values (C) /= Empty_Value
               then return False; end if;
            end loop;
            for Root in AML_Root_Slots.Held_Root loop
               if not Store.Entries (I).Held_Initialized (Root) and then Store.Entries (I).Held (Root) /= Empty_Value
               then return False; end if;
            end loop;
            Found := Found + 1;
         end if;
      end loop;
      return Found = Store.Used;
   end Valid;
   function Reserved_Frame (After, Before : State; Frame : FH.Frame_Handle) return Boolean is
      Slot : constant Root_Count := Find (After, Frame);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot > 0 and then Find (Before, Frame) = 0 and then FH.Present (Frame)
        and then Before.Entries (Slot).Frame = FH.No_Frame and then Before.Used < Capacity
        and then After = (Before with delta Used => Before.Used + 1,
          Entries => (Before.Entries with delta Slot => (Frame => Frame, others => <>)));
   end Reserved_Frame;
   function Updated_Cell (After, Before : State; Frame : FH.Frame_Handle;
      Cell : FH.Cell_ID; Initialized : Boolean; Value : Value_Type) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot > 0 and then (Initialized or else Value = Empty_Value)
        and then After = (Before with delta Entries => (Before.Entries with delta
          Slot => (Before.Entries (Slot) with delta
            Initialized => (Before.Entries (Slot).Initialized with delta Cell => Initialized),
            Values => (Before.Entries (Slot).Values with delta Cell => Value))));
   end Updated_Cell;
   function Released_Frame (After, Before : State; Frame : FH.Frame_Handle) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot > 0
        and then Before.Used > 0 and then After.Used = Before.Used - 1
        and then After.Entries = (Before.Entries with delta Slot => (others => <>))
        and then (for all I in Root_Index =>
          (for all J in 1 .. Before.Object_Capacity => After.Stamps (I, J) =
            (if I = Slot then AML_Object_Identifiers.No_Incarnation else Before.Stamps (I, J))));
   end Released_Frame;
   procedure Reserve (Store : in out State; Frame : FH.Frame_Handle; Status : out Result_Status) is
   begin
      if not FH.Present (Frame) then Status := Invalid_Frame; return; end if;
      if Find (Store, Frame) > 0 then Status := Duplicate_Frame; return; end if;
      if Store.Used = Capacity then Status := Root_Limit; return; end if;
      for I in Root_Index loop
         if Store.Entries (I).Frame = FH.No_Frame then
            Store.Entries (I) := (Frame => Frame, others => <>);
            Store.Used := Store.Used + 1;
            Status := Ready; return;
         end if;
      end loop;
      raise Program_Error with "frame root capacity invariant";
   end Reserve;
   procedure Update (Store : in out State; Frame : FH.Frame_Handle;
      Cell : FH.Cell_ID; Initialized : Boolean; Value : Value_Type; Status : out Result_Status) is
      Slot : constant Root_Count := Find (Store, Frame);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      if not Initialized and then Value /= Empty_Value then Status := Invalid_Value; return; end if;
      Store.Entries (Slot).Initialized (Cell) := Initialized;
      Store.Entries (Slot).Values (Cell) := Value;
      Status := Ready;
   end Update;
   procedure Release (Store : in out State; Frame : FH.Frame_Handle; Status : out Result_Status) is
      Slot : constant Root_Count := Find (Store, Frame);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      Store.Entries (Slot) := (others => <>);
      Clear_Census (Store, Slot);
      Store.Used := Store.Used - 1;
      Status := Ready;
   end Release;
   function Updated_Held (After, Before : State; Frame : FH.Frame_Handle;
      Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Value_Type) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot > 0 and then (Initialized or else Value = Empty_Value)
        and then After = (Before with delta Entries => (Before.Entries with delta
          Slot => (Before.Entries (Slot) with delta
            Held_Initialized => (Before.Entries (Slot).Held_Initialized with delta Root => Initialized),
            Held => (Before.Entries (Slot).Held with delta Root => Value))));
   end Updated_Held;
   procedure Update_Held (Store : in out State; Frame : FH.Frame_Handle;
      Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Value_Type; Status : out Result_Status) is
      Slot : constant Root_Count := Find (Store, Frame);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      if not Initialized and then Value /= Empty_Value then Status := Invalid_Value; return; end if;
      Store.Entries (Slot).Held_Initialized (Root) := Initialized;
      Store.Entries (Slot).Held (Root) := Value;
      Status := Ready;
   end Update_Held;
   function Held_Read (Store : State; Index : Root_Index; Root : AML_Root_Slots.Held_Root;
      Result : Read_Result) return Boolean is
     (Result = Read_Result'(Reserved => Store.Entries (Index).Frame /= FH.No_Frame,
       Initialized => Store.Entries (Index).Held_Initialized (Root), Value => Store.Entries (Index).Held (Root)));
   function Read_Held (Store : State; Index : Root_Index; Root : AML_Root_Slots.Held_Root) return Read_Result is
     (Reserved => Store.Entries (Index).Frame /= FH.No_Frame,
      Initialized => Store.Entries (Index).Held_Initialized (Root), Value => Store.Entries (Index).Held (Root));
   function Cell_Read (Store : State; Index : Root_Index; Cell : FH.Cell_ID;
      Result : Read_Result) return Boolean is
     (Result = Read_Result'(Reserved => Store.Entries (Index).Frame /= FH.No_Frame,
       Initialized => Store.Entries (Index).Initialized (Cell), Value => Store.Entries (Index).Values (Cell)));
   function Read_Cell (Store : State; Index : Root_Index; Cell : FH.Cell_ID) return Read_Result is
     (Reserved => Store.Entries (Index).Frame /= FH.No_Frame,
      Initialized => Store.Entries (Index).Initialized (Cell), Value => Store.Entries (Index).Values (Cell));
   function Updated_Snapshot (After, Before : State; Frame : FH.Frame_Handle;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot > 0
        and then Roots.Object_Capacity = Before.Object_Capacity
        and then After.Used = Before.Used
        and then After.Entries = (Before.Entries with delta Slot =>
          (Before.Entries (Slot) with delta Snapshot_Header => R.Header_Of (Roots)))
        and then (for all I in Root_Index =>
          (for all J in 1 .. Before.Object_Capacity => After.Stamps (I, J) =
            (if I = Slot then R.Stamp_At (Roots, J) else Before.Stamps (I, J))));
   end Updated_Snapshot;
   procedure Update_Snapshot (Store : in out State; Frame : FH.Frame_Handle;
      Roots : AML_Objects.Root_Snapshots.Snapshot; Status : out Result_Status) is
      Slot : constant Root_Count := Find (Store, Frame);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      if Roots.Object_Capacity /= Store.Object_Capacity then Status := Invalid_Shape; return; end if;
      Clear_Census (Store, Slot);
      Store.Entries (Slot).Snapshot_Header := R.Header_Of (Roots);
      -- Compatibility import of an already dense snapshot. Interpreter
      -- publication uses Replace_Expression_Roots and never takes this scan.
      for J in 1 .. Store.Object_Capacity loop
         declare Stamp : constant AML_Object_Identifiers.Slot_Incarnation := R.Stamp_At (Roots,J); begin
            if Stamp /= AML_Object_Identifiers.No_Incarnation then
               Store.Stamps (Slot,J) := Stamp;
               Store.Touched_Count (Slot) := Store.Touched_Count (Slot)+1;
               Store.Touched (Slot,Store.Touched_Count (Slot)) := J;
            end if;
         end;
      end loop;
      Status := Ready;
   end Update_Snapshot;
   -- Dense logical reference model. This is ghost-only: the running
   -- interpreter never materializes this capacity-sized array per operation.
   function Expected_Census
     (Object_Capacity : AML_Objects.Storage_Capacity;
      Metadata : R.Header; Addresses : Address_List) return R.Snapshot with Ghost
   is
      package IDs renames AML_Object_Identifiers;
      Stamps : R.Stamp_Vector (1 .. Object_Capacity) := [others => IDs.No_Incarnation];
      Header_Copy : R.Snapshot := R.From_Parts (Metadata,[1 => IDs.No_Incarnation]);
   begin
      for Address of Addresses loop
         if not IDs.Present (Address) or else IDs.Slot_Of (Address) not in Stamps'Range then
            R.Reject (Header_Copy); exit;
         end if;
         declare
            Slot : constant Positive := IDs.Slot_Of (Address);
            Stamp : constant IDs.Slot_Incarnation := IDs.Incarnation_Of (Address);
         begin
            if Stamps (Slot) = IDs.No_Incarnation then Stamps (Slot) := Stamp;
            elsif Stamps (Slot) /= Stamp then R.Reject (Header_Copy); exit;
            end if;
         end;
      end loop;
      return R.From_Parts (R.Header_Of (Header_Copy),Stamps);
   end Expected_Census;
   function Expression_Replaced
     (After, Before : State; Frame : FH.Frame_Handle; Metadata : R.Header;
      Addresses : Address_List) return Boolean is
      Slot : constant Root_Count := Find (Before,Frame);
      Expected : constant R.Snapshot := Expected_Census (Before.Object_Capacity,Metadata,Addresses);
   begin
      return After.Object_Capacity = Before.Object_Capacity and then Slot /= 0
        and then After.Used = Before.Used
        and then After.Entries = (Before.Entries with delta Slot =>
          (Before.Entries (Slot) with delta Snapshot_Header => R.Header_Of (Expected)))
        and then (for all I in Root_Index =>
          (for all J in 1 .. Before.Object_Capacity => After.Stamps (I,J) =
            (if I = Slot then R.Stamp_At (Expected,J) else Before.Stamps (I,J))))
        and then (for all I in Root_Index => (if I /= Slot then
          After.Touched_Count (I) = Before.Touched_Count (I)
          and then (for all J in 1 .. Before.Object_Capacity =>
            After.Touched (I,J) = Before.Touched (I,J))));
   end Expression_Replaced;
   procedure Replace_Expression_Roots
     (Store : in out State; Frame : FH.Frame_Handle; Metadata : R.Header;
      Addresses : Address_List; Status : out Result_Status) is
      package IDs renames AML_Object_Identifiers;
      Slot : constant Root_Count := Find (Store, Frame);
      Header_Copy : R.Snapshot := R.From_Parts (Metadata, [1 => IDs.No_Incarnation]);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      Clear_Census (Store,Slot);
      for Address of Addresses loop
         if not IDs.Present (Address) or else IDs.Slot_Of (Address) not in 1 .. Store.Object_Capacity then
            R.Reject (Header_Copy); exit;
         end if;
         declare
            Object : constant Positive := IDs.Slot_Of (Address);
            Stamp : constant IDs.Slot_Incarnation := IDs.Incarnation_Of (Address);
            Prior : constant IDs.Slot_Incarnation := Store.Stamps (Slot,Object);
         begin
            if Prior = IDs.No_Incarnation then
               Store.Touched_Count (Slot) := Store.Touched_Count (Slot)+1;
               Store.Touched (Slot,Store.Touched_Count (Slot)) := Object;
               Store.Stamps (Slot,Object) := Stamp;
            elsif Prior /= Stamp then R.Reject (Header_Copy); exit;
            end if;
         end;
      end loop;
      Store.Entries (Slot).Snapshot_Header := R.Header_Of (Header_Copy);
      Status := Ready;
   end Replace_Expression_Roots;
   function Snapshot_Read (Store : State; Index : Root_Index;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean is
     (Roots.Object_Capacity = Store.Object_Capacity
      and then R.Header_Of (Roots) = Store.Entries (Index).Snapshot_Header
      and then (for all J in 1 .. Store.Object_Capacity =>
        R.Stamp_At (Roots, J) = Store.Stamps (Index, J)));
   function Read_Snapshot (Store : State; Index : Root_Index)
      return AML_Objects.Root_Snapshots.Snapshot is
      Stamps : R.Stamp_Vector (1 .. Store.Object_Capacity);
   begin
      for J in Stamps'Range loop Stamps (J) := Store.Stamps (Index, J); end loop;
      return R.From_Parts (Store.Entries (Index).Snapshot_Header, Stamps);
   end Read_Snapshot;
end AML_Frame_Roots;
