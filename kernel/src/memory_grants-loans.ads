pragma Ada_2022;

--  One serialized forwarding scope for one acquired parent grant. The parent
--  owner must explicitly allow forwarding. Children are terminal loans: no
--  hidden descendant may outlive the readers tracked here.
generic
   Maximum_Sequence : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package Memory_Grants.Loans with Pure, SPARK_Mode is
   type Forwarding_Policy is (No_Forwarding, Forward_Once);
   type Parent_Phase is (Unconfigured, Accepting, Closing, Retired);
   type Loan_Phase is (Absent, Mapping, Available, Draining, Unmapping);
   subtype Loan_Index is Positive range 1 .. Grants_Per_Process;
   type Terms is record
      Offset : Page_Offset := 0;
      Pages : Page_Count := 1;
      Access_Mode : Permission := Borrowed_Read_Only;
   end record;
   type Loan_Reference is private;
   No_Loan : constant Loan_Reference;
   type State is private;

   function Phase (Item : State) return Parent_Phase;
   function Holds_Parent (Item : State) return Boolean is
     (Phase (Item) in Accepting | Closing);
   function Empty (Item : State) return Boolean;
   function Sequence (Item : State) return Unsigned_64;
   function Live (Item : State; Loan : Loan_Reference) return Boolean;
   function Phase_Of (Item : State; Loan : Loan_Reference) return Loan_Phase;
   function Readers (Item : State; Loan : Loan_Reference)
      return Acquisition_Count;
   function Describe (Item : State; Loan : Loan_Reference) return Terms
      with Pre => Live (Item, Loan);
   function Admits (Item : State; Requested : Terms) return Boolean;

   --  Adapter has authenticated the parent, checked its forwarding authority
   --  and retained a dedicated parent acquisition. Never reset/recreate a
   --  scope under the same parent reference. This model does not acquire it.
   procedure Configure
     (Item : in out State; Parent : Reference; Pages : Page_Count;
      Access_Mode : Permission; Forwarding : Forwarding_Policy;
      Applied : out Boolean)
     with Post =>
       (Applied = (Phase (Item'Old) = Unconfigured)) and
       (if Applied then Phase (Item) = Accepting and Empty (Item)
        else Item = Item'Old);

   type Reservation_Result is (Reserved, Denied, Full, Exhausted);
   --  Reserve before installing any mappings. Failed/partial mapping follows
   --  Revoke -> unmap/shootdown -> Finish_Retirement, never direct slot reuse.
   procedure Reserve
     (Item : in out State; Requested : Terms; Loan : out Loan_Reference;
      Result : out Reservation_Result)
     with Post =>
       (Holds_Parent (Item) = Holds_Parent (Item'Old)) and
       (if Result = Reserved then
          Admits (Item'Old, Requested) and
          Phase_Of (Item, Loan) = Mapping and Describe (Item, Loan) = Requested
          and Sequence (Item) = Sequence (Item'Old) + 1
        else Item = Item'Old and Loan = No_Loan);

   procedure Publish
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
     with Post =>
       Applied = (Phase_Of (Item'Old, Loan) = Mapping) and
       Holds_Parent (Item) = Holds_Parent (Item'Old) and
       (if Applied then Phase_Of (Item, Loan) = Available
        else Item = Item'Old);

   procedure Acquire
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
     with Post =>
       Applied = (Phase_Of (Item'Old, Loan) = Available and
                  Readers (Item'Old, Loan) < Acquisition_Count'Last) and
       Holds_Parent (Item) = Holds_Parent (Item'Old) and
       (if Applied then Readers (Item, Loan) = Readers (Item'Old, Loan) + 1
        else Item = Item'Old);

   procedure Return_Reader
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
     with Post =>
       Applied = (Live (Item'Old, Loan) and Readers (Item'Old, Loan) > 0) and
       Holds_Parent (Item) = Holds_Parent (Item'Old) and
       (if Applied then
          Readers (Item, Loan) = Readers (Item'Old, Loan) - 1 and
          (if Phase_Of (Item'Old, Loan) = Draining and
              Readers (Item'Old, Loan) = 1 then Phase_Of (Item, Loan) = Unmapping)
        else Item = Item'Old);

   procedure Revoke
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
     with Post =>
       Applied = (Phase_Of (Item'Old, Loan) in Mapping | Available) and
       Holds_Parent (Item) = Holds_Parent (Item'Old) and
       (if Applied then
          Readers (Item, Loan) = Readers (Item'Old, Loan) and
          Phase_Of (Item, Loan) =
            (if Readers (Item'Old, Loan) = 0 then Unmapping else Draining)
        else Item = Item'Old);

   --  Called only after real unmap + acknowledged TLB shootdown, not merely
   --  receipt of a userspace "done" message. DMA quiescence is separate.
   procedure Finish_Retirement
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
     with Post =>
       Applied = (Phase_Of (Item'Old, Loan) = Unmapping) and
       Holds_Parent (Item) = Holds_Parent (Item'Old) and
       (if Applied then not Live (Item, Loan) else Item = Item'Old);

   procedure Close (Item : in out State)
     with Post =>
       (Holds_Parent (Item) = Holds_Parent (Item'Old)) and
       (if Phase (Item'Old) = Accepting then Phase (Item) = Closing
        else Item = Item'Old);

   --  One-shot obligation to return the dedicated parent acquisition. Commit
   --  a trial state only when the real kernel return succeeds. Neither child
   --  returns nor Close can release this hold.
   procedure Release_Parent (Item : in out State; Applied : out Boolean)
     with Post =>
       (Applied = (Phase (Item'Old) = Closing and Empty (Item'Old))) and
       (if Applied then Phase (Item) = Retired and not Holds_Parent (Item)
        else Item = Item'Old);
private
   function Valid (Item : State) return Boolean with Ghost;
   type Loan_Reference is record
      Parent : Reference := (0, Initial_Generation);
      Index : Loan_Index := Loan_Index'First;
      Stamp : Unsigned_64 := 0;
   end record;
   No_Loan : constant Loan_Reference := (others => <>);
   type Entry_State is record
      Stage : Loan_Phase := Absent;
      Stamp : Unsigned_64 := 0;
      Count : Acquisition_Count := 0;
      Description : Terms;
   end record;
   type Entry_Array is array (Loan_Index) of Entry_State;
   type State is record
      Stage : Parent_Phase := Unconfigured;
      Parent : Reference := (0, Initial_Generation);
      Pages : Page_Count := 1;
      Access_Mode : Permission := Borrowed_Read_Only;
      Forwarding : Forwarding_Policy := No_Forwarding;
      Last_Sequence : Unsigned_64 := 0;
      Entries : Entry_Array := [others => <>];
   end record with Type_Invariant => Valid (State);
   function Phase (Item : State) return Parent_Phase is (Item.Stage);
   function Empty (Item : State) return Boolean is
     (for all E of Item.Entries => E.Stage = Absent);
   function Sequence (Item : State) return Unsigned_64 is (Item.Last_Sequence);
   function Live (Item : State; Loan : Loan_Reference) return Boolean is
     (Holds_Parent (Item) and then Loan.Parent = Item.Parent and then
      Loan.Stamp /= 0 and then Item.Entries (Loan.Index).Stage /= Absent and then
      Item.Entries (Loan.Index).Stamp = Loan.Stamp);
   function Phase_Of (Item : State; Loan : Loan_Reference) return Loan_Phase is
     (if Live (Item, Loan) then Item.Entries (Loan.Index).Stage else Absent);
   function Readers (Item : State; Loan : Loan_Reference)
      return Acquisition_Count is
     (if Live (Item, Loan) then Item.Entries (Loan.Index).Count else 0);
   function Describe (Item : State; Loan : Loan_Reference) return Terms is
     (Item.Entries (Loan.Index).Description);
   function Admits (Item : State; Requested : Terms) return Boolean is
     (Item.Stage = Accepting and then Item.Forwarding = Forward_Once and then
      Range_Attenuates (Item.Pages, Requested.Offset, Requested.Pages) and then
      Permission_Attenuates (Item.Access_Mode, Requested.Access_Mode));
   function Valid (Item : State) return Boolean is
     (Item.Last_Sequence <= Maximum_Sequence and then
      (if Item.Stage in Unconfigured | Retired then
         (for all E of Item.Entries => E.Stage = Absent)) and then
      (for all E of Item.Entries =>
        (if E.Stage = Absent then E.Count = 0 and E.Stamp = 0
         else Item.Stage in Accepting | Closing and E.Stamp > 0 and
           E.Stamp <= Item.Last_Sequence and
           Item.Forwarding = Forward_Once and
           Range_Attenuates (Item.Pages, E.Description.Offset,
                            E.Description.Pages) and
           Permission_Attenuates (Item.Access_Mode, E.Description.Access_Mode)
           and
           (if E.Stage in Mapping | Available then Item.Stage = Accepting) and
           (if E.Stage in Mapping | Unmapping then E.Count = 0) and
           (if E.Stage = Draining then E.Count > 0))));
end Memory_Grants.Loans;
