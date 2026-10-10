package body AML_Frame_Roots with SPARK_Mode is
   package FH renames AML_Frame_Handles;
   use type AML_Objects.Root_Snapshots.Snapshot;
   function Empty return State is (others => <>);
   function Find (Store : State; Frame : FH.Frame_Handle) return Root_Count is
   begin
      if Frame = FH.No_Frame then return 0; end if;
      for I in Root_Index loop
         if Store.Entries (I).Frame = Frame then return I; end if;
      end loop;
      return 0;
   end Find;
   function Valid (Store : State) return Boolean is
      Found : Root_Count := 0;
   begin
      for I in Root_Index loop
         if Store.Entries (I).Frame = FH.No_Frame then
            if Store.Entries (I) /= Entry_Data'(others => <>) then return False; end if;
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
      return Slot > 0 and then Find (Before, Frame) = 0 and then FH.Present (Frame)
        and then Before.Entries (Slot).Frame = FH.No_Frame and then Before.Used < Capacity
        and then After = (Before with delta Used => Before.Used + 1,
          Entries => (Before.Entries with delta Slot => (Frame => Frame, others => <>)));
   end Reserved_Frame;
   function Updated_Cell (After, Before : State; Frame : FH.Frame_Handle;
      Cell : FH.Cell_ID; Initialized : Boolean; Value : Value_Type) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return Slot > 0 and then (Initialized or else Value = Empty_Value)
        and then After = (Before with delta Entries => (Before.Entries with delta
          Slot => (Before.Entries (Slot) with delta
            Initialized => (Before.Entries (Slot).Initialized with delta Cell => Initialized),
            Values => (Before.Entries (Slot).Values with delta Cell => Value))));
   end Updated_Cell;
   function Released_Frame (After, Before : State; Frame : FH.Frame_Handle) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return Slot > 0 and then Before.Used > 0 and then After =
        (Before with delta Used => Before.Used - 1,
          Entries => (Before.Entries with delta Slot => (others => <>)));
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
      Store.Used := Store.Used - 1;
      Status := Ready;
   end Release;
   function Updated_Held (After, Before : State; Frame : FH.Frame_Handle;
      Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Value_Type) return Boolean is
      Slot : constant Root_Count := Find (Before, Frame);
   begin
      return Slot > 0 and then (Initialized or else Value = Empty_Value)
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
      return Slot > 0 and then After = (Before with delta Entries =>
        (Before.Entries with delta Slot => (Before.Entries (Slot) with delta Snapshot => Roots)));
   end Updated_Snapshot;
   procedure Update_Snapshot (Store : in out State; Frame : FH.Frame_Handle;
      Roots : AML_Objects.Root_Snapshots.Snapshot; Status : out Result_Status) is
      Slot : constant Root_Count := Find (Store, Frame);
   begin
      if Slot = 0 then Status := Invalid_Frame; return; end if;
      Store.Entries (Slot).Snapshot := Roots;
      Status := Ready;
   end Update_Snapshot;
   function Snapshot_Read (Store : State; Index : Root_Index;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean is
     (Roots = Store.Entries (Index).Snapshot);
   function Read_Snapshot (Store : State; Index : Root_Index)
      return AML_Objects.Root_Snapshots.Snapshot is (Store.Entries (Index).Snapshot);
end AML_Frame_Roots;
