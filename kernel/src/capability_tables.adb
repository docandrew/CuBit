-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- Capability handle table (CAP-002 step 1). See the spec for the rules and
-- what is proved. The only code outside SPARK is the page adapter
-- (Acquire_*/Return_*): turning a page address from Allocate_Page into an
-- owning pointer and back.
-------------------------------------------------------------------------------
pragma Ada_2022;

with Ada.Unchecked_Conversion;

package body Capability_Tables with
    SPARK_Mode => On
is
    use type System.Address;

    -- Completed here, not in the spec's private part: GNAT 15.3 crashes
    -- (process_freeze_entity) freezing these there in an instance.
    -- The entries depend on the storage only, so updating a counter of the
    -- table leaves its model unchanged by congruence.
    function Stored_Entry (D : Directory_Access; S : Slot_Index) return Slot_Entry is
      (if D /= null and then D (Leaf_Of (S)) /= null
       then D (Leaf_Of (S)).all (Offset_Of (S))
       else Unused_Entry);

    function Stored_Model (D : Directory_Access) return Model_Array is
      ([for S in Slot_Index => Stored_Entry (D, S)])
    with Ghost;

    function Entry_Of (T : Table; S : Slot_Index) return Slot_Entry is
      (Stored_Entry (T.Dir, S));

    function Model (T : Table) return Model_Array is (Stored_Model (T.Dir));

    ---------------------------------------------------------------------------
    -- Page adapter (not SPARK; contracts are the trusted boundary)
    ---------------------------------------------------------------------------

    -- A fresh leaf of Unused_Entry from Allocate_Page, or null if refused.
    procedure Acquire_Leaf (For_Owner : Unsigned_64; L : out Leaf_Access) with
        Post   => (if L /= null then
                     (for all O in Leaf_Offset => L.all (O) = Unused_Entry));

    -- A fresh directory of null leaves, or null if refused.
    procedure Acquire_Directory
      (For_Owner : Unsigned_64; D : out Directory_Access)
    with
        Post   => (if D /= null then
                     (for all I in Leaf_Index => D.all (I) = null));

    procedure Return_Leaf (L : in out Leaf_Access) with
        Pre    => L /= null,
        Post   => L = null;

    -- Only an empty directory: its leaves must be returned first.
    procedure Return_Directory (D : in out Directory_Access) with
        Pre    => D /= null and then (for all I in Leaf_Index => D.all (I) = null),
        Post   => D = null;

    Page_Bits : constant := Page_Bytes * System.Storage_Unit;

    procedure Acquire_Leaf (For_Owner : Unsigned_64; L : out Leaf_Access) with
        SPARK_Mode => Off
    is
        function To_Leaf is new Ada.Unchecked_Conversion
          (System.Address, Leaf_Access);
        Page : System.Address;
        OK   : Boolean;
    begin
        L := null;
        -- Static per instance: an Element too large for 64 entries per page
        -- makes every growth refuse instead of overrunning the page.
        if Leaf'Size > Page_Bits or else Leaf'Alignment > Page_Bytes then
            return;
        end if;
        Allocate_Page (For_Owner, Page, OK);
        if not OK or else Page = System.Null_Address then
            return;
        end if;
        L := To_Leaf (Page);
        L.all := [others => Unused_Entry];
    end Acquire_Leaf;

    procedure Acquire_Directory
      (For_Owner : Unsigned_64; D : out Directory_Access)
    with
        SPARK_Mode => Off
    is
        function To_Directory is new Ada.Unchecked_Conversion
          (System.Address, Directory_Access);
        Page : System.Address;
        OK   : Boolean;
    begin
        D := null;
        if Directory'Size > Page_Bits or else Directory'Alignment > Page_Bytes then
            return;
        end if;
        Allocate_Page (For_Owner, Page, OK);
        if not OK or else Page = System.Null_Address then
            return;
        end if;
        D := To_Directory (Page);
        D.all := [others => null];
    end Acquire_Directory;

    procedure Return_Leaf (L : in out Leaf_Access) with
        SPARK_Mode => Off
    is
        function To_Address is new Ada.Unchecked_Conversion
          (Leaf_Access, System.Address);
    begin
        Release_Page (To_Address (L));
        L := null;
    end Return_Leaf;

    procedure Return_Directory (D : in out Directory_Access) with
        SPARK_Mode => Off
    is
        function To_Address is new Ada.Unchecked_Conversion
          (Directory_Access, System.Address);
    begin
        Release_Page (To_Address (D));
        D := null;
    end Return_Directory;

    ---------------------------------------------------------------------------
    -- Counting lemmas
    ---------------------------------------------------------------------------

    function Same_Liveness (A, B : Slot_Entry) return Boolean is
      ((A.State = Live) = (B.State = Live))
    with Ghost;

    -- Equal liveness below N gives equal counts.
    procedure Lemma_Count_Frame (M1, M2 : Model_Array; N : Slot_Count) with
        Ghost,
        Global => null,
        Subprogram_Variant => (Decreases => N),
        Pre  => (for all J in Slot_Index =>
                   (if J < N then Same_Liveness (M1 (J), M2 (J)))),
        Post => Count_Live (M1, N) = Count_Live (M2, N);

    procedure Lemma_Count_Frame (M1, M2 : Model_Array; N : Slot_Count) is
    begin
        if N > 0 then
            Lemma_Count_Frame (M1, M2, N - 1);
        end if;
    end Lemma_Count_Frame;

    -- One slot below N goes from live to not live.
    procedure Lemma_Count_Remove
      (M1, M2 : Model_Array; S : Slot_Index; N : Slot_Count)
    with
        Ghost,
        Global => null,
        Subprogram_Variant => (Decreases => N),
        Pre  => S < N
                and then M1 (S).State = Live and then M2 (S).State /= Live
                and then (for all J in Slot_Index =>
                            (if J /= S then Same_Liveness (M1 (J), M2 (J)))),
        Post => Count_Live (M1, N) >= 1
                and then Count_Live (M2, N) = Count_Live (M1, N) - 1;

    procedure Lemma_Count_Remove
      (M1, M2 : Model_Array; S : Slot_Index; N : Slot_Count) is
    begin
        if N - 1 = S then
            Lemma_Count_Frame (M1, M2, N - 1);
        else
            Lemma_Count_Remove (M1, M2, S, N - 1);
        end if;
    end Lemma_Count_Remove;

    -- One slot below N goes from not live to live.
    procedure Lemma_Count_Add
      (M1, M2 : Model_Array; S : Slot_Index; N : Slot_Count)
    with
        Ghost,
        Global => null,
        Subprogram_Variant => (Decreases => N),
        Pre  => S < N
                and then M1 (S).State /= Live and then M2 (S).State = Live
                and then (for all J in Slot_Index =>
                            (if J /= S then Same_Liveness (M1 (J), M2 (J)))),
        Post => Count_Live (M2, N) = Count_Live (M1, N) + 1;

    procedure Lemma_Count_Add
      (M1, M2 : Model_Array; S : Slot_Index; N : Slot_Count) is
    begin
        if N - 1 = S then
            Lemma_Count_Frame (M1, M2, N - 1);
        else
            Lemma_Count_Add (M1, M2, S, N - 1);
        end if;
    end Lemma_Count_Add;


    -- Count one more slot.
    procedure Lemma_Count_Step (M : Model_Array; N : Slot_Index) with
        Ghost,
        Global => null,
        Post => Count_Live (M, N + 1) =
                Count_Live (M, N) + (if M (N).State = Live then 1 else 0);

    procedure Lemma_Count_Step (M : Model_Array; N : Slot_Index) is null;


    -- Entry_Of is the model's view of a slot.
    procedure Lemma_Entry_In_Model (T : Table; S : Slot_Index) with
        Ghost,
        Global => null,
        Post => Entry_Of (T, S) = Model (T) (S);

    procedure Lemma_Entry_In_Model (T : Table; S : Slot_Index) is null;

    ---------------------------------------------------------------------------
    -- Invariant transitions on models (pure array reasoning, no pointers)
    ---------------------------------------------------------------------------

    -- Extending the high-water mark with a live slot.
    procedure Lemma_Fresh
      (M1, M2 : Model_Array; High : Slot_Index; Head, Tail : Slot_Link)
    with
        Ghost,
        Global => null,
        Pre  => Entries_Valid (M1, High)
                and then Free_List_Valid (M1, High, Head, Tail)
                and then M2 (High).State = Live
                and then M2 (High).Gen = Initial_Generation
                and then (for all J in Slot_Index =>
                            (if J /= High then M2 (J) = M1 (J))),
        Post => Entries_Valid (M2, High + 1)
                and then Free_List_Valid (M2, High + 1, Head, Tail);

    procedure Lemma_Fresh
      (M1, M2 : Model_Array; High : Slot_Index; Head, Tail : Slot_Link) is
    begin
        pragma Assert
          (for all X in Slot_Link => Listed (M2, High + 1, X) = Listed (M1, High, X));
    end Lemma_Fresh;

    -- Taking the head of the free list for a live entry.
    procedure Lemma_Reuse
      (M1, M2    : Model_Array;
       High      : Slot_Count;
       Head      : Slot_Index;
       Tail      : Slot_Link;
       New_Tail  : Slot_Link)
    with
        Ghost,
        Global => null,
        Pre  => Entries_Valid (M1, High)
                and then Free_List_Valid (M1, High, Head, Tail)
                and then New_Tail = (if M1 (Head).Next = No_Link then No_Link
                                     else Tail)
                and then M2 (Head).State = Live
                and then M2 (Head).Gen = M1 (Head).Gen + 1
                and then (for all J in Slot_Index =>
                            (if J /= Head then M2 (J) = M1 (J))),
        Post => Entries_Valid (M2, High)
                and then Free_List_Valid (M2, High, M1 (Head).Next, New_Tail);

    procedure Lemma_Reuse
      (M1, M2    : Model_Array;
       High      : Slot_Count;
       Head      : Slot_Index;
       Tail      : Slot_Link;
       New_Tail  : Slot_Link) is
    begin
        pragma Assert
          (for all X in Slot_Link =>
             Listed (M2, High, X) = (Listed (M1, High, X) and then X /= Head));
    end Lemma_Reuse;

    -- Appending a vacated slot to the free list.
    procedure Lemma_Push
      (M1, M2 : Model_Array;
       High   : Slot_Count;
       Head   : Slot_Link;
       Tail   : Slot_Link;
       S      : Slot_Index)
    with
        Ghost,
        Global => null,
        Pre  => Entries_Valid (M1, High)
                and then Free_List_Valid (M1, High, Head, Tail)
                and then S < High
                and then M1 (S).State = Live
                and then M1 (S).Gen < Last_Generation
                and then M2 (S).State = Free
                and then M2 (S).Gen = M1 (S).Gen
                and then M2 (S).Next = No_Link
                and then (if Tail /= No_Link then
                            M2 (Tail) = (M1 (Tail) with delta Next => S))
                and then (for all J in Slot_Index =>
                            (if J /= S and then J /= Tail then M2 (J) = M1 (J))),
        Post => Entries_Valid (M2, High)
                and then Free_List_Valid
                           (M2, High, (if Tail = No_Link then S else Head), S);

    procedure Lemma_Push
      (M1, M2 : Model_Array;
       High   : Slot_Count;
       Head   : Slot_Link;
       Tail   : Slot_Link;
       S      : Slot_Index) is
    begin
        pragma Assert
          (for all X in Slot_Link =>
             Listed (M2, High, X) = (Listed (M1, High, X) or else X = S));
        pragma Assert
          (for all X in Slot_Index =>
             (if Listed (M1, High, X) then M1 (X).Next /= S));
    end Lemma_Push;

    -- Retiring a live slot: the free list is untouched.
    procedure Lemma_Retire
      (M1, M2 : Model_Array;
       High   : Slot_Count;
       Head   : Slot_Link;
       Tail   : Slot_Link;
       S      : Slot_Index)
    with
        Ghost,
        Global => null,
        Pre  => Entries_Valid (M1, High)
                and then Free_List_Valid (M1, High, Head, Tail)
                and then S < High
                and then M1 (S).State = Live
                and then M1 (S).Gen = Last_Generation
                and then M2 (S).State = Retired
                and then M2 (S).Gen = M1 (S).Gen
                and then (for all J in Slot_Index =>
                            (if J /= S then M2 (J) = M1 (J))),
        Post => Entries_Valid (M2, High)
                and then Free_List_Valid (M2, High, Head, Tail);

    procedure Lemma_Retire
      (M1, M2 : Model_Array;
       High   : Slot_Count;
       Head   : Slot_Link;
       Tail   : Slot_Link;
       S      : Slot_Index) is
    begin
        pragma Assert
          (for all X in Slot_Link => Listed (M2, High, X) = Listed (M1, High, X));
    end Lemma_Retire;

    ---------------------------------------------------------------------------
    -- Entry access
    ---------------------------------------------------------------------------

    -- Overwrite one held slot; nothing else changes.
    procedure Set_Entry (T : in out Table; S : Slot_Index; E : Slot_Entry) with
        Global => null,
        Pre    => Storage_Shape (T) and then S < T.Leaves * Entries_Per_Leaf,
        Post   => Model (T) = (Model (T)'Old with delta S => E)
                  and then Storage_Shape (T)
                  and then T.High_Water = T.High_Water'Old
                  and then T.Leaves = T.Leaves'Old
                  and then T.Live = T.Live'Old
                  and then T.Free_Head = T.Free_Head'Old
                  and then T.Free_Tail = T.Free_Tail'Old
                  and then T.Charged = T.Charged'Old
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Set_Entry (T : in out Table; S : Slot_Index; E : Slot_Entry) is
    begin
        T.Dir.all (Leaf_Of (S)).all (Offset_Of (S)) := E;
    end Set_Entry;

    -- Add leaf T.Leaves (and the directory first if there is none). On
    -- refusal nothing changes: a directory obtained for this growth is
    -- returned.
    procedure Grow (T : in out Table; Grown : out Boolean) with
        Pre    => Storage_Shape (T) and then T.Leaves < Directory_Leaves,
        Post   => Model (T) = Model (T)'Old
                  and then Storage_Shape (T)
                  and then T.Leaves = (if Grown then T.Leaves'Old + 1
                                       else T.Leaves'Old)
                  and then T.High_Water = T.High_Water'Old
                  and then T.Live = T.Live'Old
                  and then T.Free_Head = T.Free_Head'Old
                  and then T.Free_Tail = T.Free_Tail'Old
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Grow (T : in out Table; Grown : out Boolean) is
        New_Leaf : Leaf_Access;
        New_Dir  : Boolean := False;
    begin
        Grown := False;
        if T.Dir = null then
            Acquire_Directory (T.Owner, T.Dir);
            if T.Dir = null then
                return;
            end if;
            New_Dir := True;
            T.Charged := T.Charged + 1;
        end if;

        Acquire_Leaf (T.Owner, New_Leaf);
        if New_Leaf = null then
            if New_Dir then
                Return_Directory (T.Dir);
                T.Charged := T.Charged - 1;
            end if;
            return;
        end if;

        T.Dir.all (T.Leaves) := New_Leaf;
        T.Leaves := T.Leaves + 1;
        T.Charged := T.Charged + 1;
        Grown := True;
    end Grow;

    ---------------------------------------------------------------------------
    -- Public operations
    ---------------------------------------------------------------------------

    function From_Word (W : Unsigned_64) return Handle is
        Low  : constant Unsigned_32 := Unsigned_32 (W and Slot_Field_Mask);
        High : constant Unsigned_64 := Shift_Right (W, Slot_Field_Bits);
        H    : constant Handle :=
          (Slot => Slot_Field (Low), Gen => Unsigned_32 (High));
    begin
        pragma Assert (Unsigned_32 (H.Slot) = Low);
        pragma Assert (Unsigned_64 (Low) = (W and Slot_Field_Mask));
        pragma Assert (Unsigned_64 (H.Gen) = High);
        pragma Assert
          ((Shift_Left (High, Slot_Field_Bits) or Unsigned_64 (Low)) = W);
        return H;
    end From_Word;

    function Handle_At (T : Table; S : Slot_Index) return Handle is
    begin
        pragma Assert (Model (T) (S) = Entry_Of (T, S));
        return Handle_For (S, Entry_Of (T, S).Gen);
    end Handle_At;

    procedure Clear (T : out Table) is
    begin
        T.Dir := null;
        T.Owner := 0;
        T.Limit := Default_Limit;
        T.High_Water := 0;
        T.Leaves := 0;
        T.Live := 0;
        T.Free_Head := No_Link;
        T.Free_Tail := No_Link;
        T.Charged := 0;
    end Clear;

    procedure Configure
      (T : in out Table; New_Owner : Unsigned_64; New_Limit : Handle_Limit) is
    begin
        T.Owner := New_Owner;
        T.Limit := New_Limit;
    end Configure;

    -- The hot path: reads the slot's fields in place (no entry copy) and
    -- copies the item only when the handle is Current.
    function Lookup (T : Table; H : Handle) return Lookup_Result is
        S : Slot_Index;
        G : Generation;
    begin
        if not Names_Slot (H) or else Slot_Of (H) >= T.High_Water
          or else H.Gen = No_Generation
        then
            return (Check => Invalid);
        end if;
        S := Slot_Of (H);
        pragma Assert (T.Dir /= null and then T.Dir (Leaf_Of (S)) /= null);
        G := T.Dir.all (Leaf_Of (S)).all (Offset_Of (S)).Gen;
        if H.Gen > G then
            return (Check => Invalid);
        elsif H.Gen < G then
            return (Check => Stale);
        elsif T.Dir.all (Leaf_Of (S)).all (Offset_Of (S)).State = Live then
            return (Check => Current,
                    Item  => T.Dir.all (Leaf_Of (S)).all (Offset_Of (S)).Item);
        elsif T.Dir.all (Leaf_Of (S)).all (Offset_Of (S)).Cause = Revoked then
            return (Check => Revoked);
        else
            return (Check => Stale);
        end if;
    end Lookup;

    -- Insert into the least recently freed slot: the head of the FIFO.
    procedure Insert_Reused
      (T : in out Table; Item : Element; S : out Slot_Index)
    with
        Global => null,
        Pre    => Valid (T) and then T.Free_Head /= No_Link
                  and then T.Live < Bound (T.Limit),
        Post   => Opened_Into (Model (T)'Old, Model (T), S, Item)
                  and then Valid (T)
                  and then T.Live = T.Live'Old + 1
                  and then T.High_Water = T.High_Water'Old
                  and then T.Charged = T.Charged'Old
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Insert_Reused
      (T : in out Table; Item : Element; S : out Slot_Index)
    is
        Old_Model : constant Model_Array := Model (T) with Ghost;
        Old_Tail  : constant Slot_Link := T.Free_Tail with Ghost;
        E         : Slot_Entry;
    begin
        S := T.Free_Head;
        E := Entry_Of (T, S);
        pragma Assert (Old_Model (S) = E and then E.State = Free);
        T.Free_Head := E.Next;
        if T.Free_Head = No_Link then
            T.Free_Tail := No_Link;
        end if;
        Set_Entry (T, S, (State => Live, Gen => E.Gen + 1,
                          Cause => Closed, Next => No_Link, Item => Item));
        pragma Assert (Opened_Into (Old_Model, Model (T), S, Item));
        Lemma_Reuse (Old_Model, Model (T), T.High_Water, S, Old_Tail,
                     T.Free_Tail);
        Lemma_Count_Add (Old_Model, Model (T), S, T.High_Water);
        T.Live := T.Live + 1;
        pragma Assert (T.Live = Count_Live (Model (T), T.High_Water));
    end Insert_Reused;

    function Fresh_Done
      (T           : Table;
       Item        : Element;
       S           : Slot_Index;
       OK          : Boolean;
       Old_Model   : Model_Array;
       Old_High    : Slot_Count;
       Old_Live    : Slot_Count;
       Old_Charged : Page_Count) return Boolean
    is
      (if OK then
          Opened_Into (Old_Model, Model (T), S, Item)
          and then S = Old_High
          and then T.Live = Old_Live + 1
          and then T.High_Water = Old_High + 1
          and then T.Charged >= Old_Charged
       else
          Unchanged (Old_Model, Model (T))
          and then T.Live = Old_Live
          and then T.High_Water = Old_High
          and then T.Charged = Old_Charged)
    with Ghost;

    -- Insert into the next never-used slot, adding a leaf if it needs one.
    procedure Insert_Fresh
      (T : in out Table; Item : Element; S : out Slot_Index;
       Grown_Or_Fits : out Boolean)
    with
        Pre    => Valid (T) and then T.Free_Head = No_Link
                  and then T.High_Water < Max_Slots
                  and then T.Live < Bound (T.Limit),
        Post   => Fresh_Done (T, Item, S, Grown_Or_Fits, Model (T)'Old,
                    T.High_Water'Old, T.Live'Old, T.Charged'Old)
                  and then Valid (T)
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Insert_Fresh
      (T : in out Table; Item : Element; S : out Slot_Index;
       Grown_Or_Fits : out Boolean)
    is
        Old_Model : constant Model_Array := Model (T) with Ghost;
    begin
        S := T.High_Water;
        Grown_Or_Fits := True;
        if S = T.Leaves * Entries_Per_Leaf then
            Grow (T, Grown_Or_Fits);
            if not Grown_Or_Fits then
                Lemma_Count_Frame (Old_Model, Model (T), T.High_Water);
                return;
            end if;
        end if;
        pragma Assert (Old_Model (S) = Unused_Entry);
        Set_Entry (T, S, (State => Live, Gen => Initial_Generation,
                          Cause => Closed, Next => No_Link, Item => Item));
        pragma Assert (Opened_Into (Old_Model, Model (T), S, Item));
        Lemma_Fresh (Old_Model, Model (T), S, T.Free_Head, T.Free_Tail);
        Lemma_Count_Frame (Old_Model, Model (T), S);
        Lemma_Count_Step (Model (T), S);
        T.High_Water := S + 1;
        T.Live := T.Live + 1;
        pragma Assert (T.Live = Count_Live (Model (T), T.High_Water));
    end Insert_Fresh;

    procedure Insert (T : in out Table; Item : Element; Result : out Insert_Result)
    is
        S  : Slot_Index;
        OK : Boolean;
    begin
        if At_Limit (T.Limit, T.Live) then
            Result := (Status => Limit_Reached);
        elsif T.Live >= Bound (T.Limit) then
            -- Every slot of the capacity is live (Kernel_Limit, or a bound
            -- above the capacity).
            Result := (Status => Table_Full);
        elsif T.Free_Head /= No_Link then
            Insert_Reused (T, Item, S);
            Result := (Status        => Opened,
                       Opened_Handle => Handle_For (S, Entry_Of (T, S).Gen));
        elsif T.High_Water < Max_Slots then
            Insert_Fresh (T, Item, S, OK);
            if OK then
                Result := (Status        => Opened,
                           Opened_Handle => Handle_For (S, Entry_Of (T, S).Gen));
            else
                Result := (Status => Out_Of_Memory);
            end if;
        else
            Result := (Status => Table_Full);
        end if;
    end Insert;

    -- Vacate a live slot whose generation can advance: append it to the FIFO
    -- so it is reused after every slot freed before it.
    procedure Vacate_To_Free
      (T : in out Table; S : Slot_Index; Cause : Vacancy_Cause)
    with
        Global => null,
        Pre    => Valid (T) and then S < T.High_Water
                  and then Entry_Of (T, S).State = Live
                  and then Entry_Of (T, S).Gen < Last_Generation,
        Post   => Vacated (Model (T)'Old, Model (T), S, Cause)
                  and then Valid (T)
                  and then T.Live = T.Live'Old - 1
                  and then T.High_Water = T.High_Water'Old
                  and then T.Charged = T.Charged'Old
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Vacate_To_Free
      (T : in out Table; S : Slot_Index; Cause : Vacancy_Cause)
    is
        Old_Model : constant Model_Array := Model (T) with Ghost;
        Old_Head  : constant Slot_Link := T.Free_Head with Ghost;
        Old_Tail  : constant Slot_Link := T.Free_Tail with Ghost;
        Mid_Model : Model_Array with Ghost;
        E         : constant Slot_Entry := Entry_Of (T, S);
    begin
        Set_Entry (T, S, (State => Free, Gen => E.Gen, Cause => Cause,
                          Next  => No_Link, Item => Empty_Element));
        Mid_Model := Model (T);
        Lemma_Count_Remove (Old_Model, Mid_Model, S, T.High_Water);
        if T.Free_Tail = No_Link then
            T.Free_Head := S;
        else
            pragma Assert (T.Free_Tail /= S
                           and then Old_Model (T.Free_Tail).State = Free);
            Lemma_Entry_In_Model (T, T.Free_Tail);
            pragma Assert (Model (T) (T.Free_Tail) = Old_Model (T.Free_Tail));
            Set_Entry (T, T.Free_Tail,
                       (Entry_Of (T, T.Free_Tail) with delta Next => S));
            Lemma_Count_Frame (Mid_Model, Model (T), T.High_Water);
        end if;
        T.Free_Tail := S;
        T.Live := T.Live - 1;
        pragma Assert (T.Live = Count_Live (Model (T), T.High_Water));
        Lemma_Push (Old_Model, Model (T), T.High_Water, Old_Head, Old_Tail, S);
        pragma Assert (Vacated (Old_Model, Model (T), S, Cause));
    end Vacate_To_Free;

    -- Vacate a live slot at Last_Generation: retired, never reused.
    procedure Vacate_To_Retired
      (T : in out Table; S : Slot_Index; Cause : Vacancy_Cause)
    with
        Global => null,
        Pre    => Valid (T) and then S < T.High_Water
                  and then Entry_Of (T, S).State = Live
                  and then Entry_Of (T, S).Gen = Last_Generation,
        Post   => Vacated (Model (T)'Old, Model (T), S, Cause)
                  and then Valid (T)
                  and then T.Live = T.Live'Old - 1
                  and then T.High_Water = T.High_Water'Old
                  and then T.Charged = T.Charged'Old
                  and then T.Owner = T.Owner'Old
                  and then T.Limit = T.Limit'Old;

    procedure Vacate_To_Retired
      (T : in out Table; S : Slot_Index; Cause : Vacancy_Cause)
    is
        Old_Model : constant Model_Array := Model (T) with Ghost;
        E         : constant Slot_Entry := Entry_Of (T, S);
    begin
        Set_Entry (T, S, (State => Retired, Gen => E.Gen, Cause => Cause,
                          Next  => No_Link, Item => Empty_Element));
        Lemma_Count_Remove (Old_Model, Model (T), S, T.High_Water);
        Lemma_Retire (Old_Model, Model (T), T.High_Water, T.Free_Head,
                      T.Free_Tail, S);
        T.Live := T.Live - 1;
        pragma Assert (T.Live = Count_Live (Model (T), T.High_Water));
    end Vacate_To_Retired;

    -- Close and Revoke.
    procedure Vacate
      (T      : in out Table;
       H      : Handle;
       Cause  : Vacancy_Cause;
       Result : out Remove_Result)
    with
        Global => null,
        Pre    => Valid (T) and then not Result'Constrained,
        Post   => Removal_Done (T, H, Result, Cause,
                    Model (T)'Old, Check (T, H)'Old, Live_Count (T)'Old,
                    High_Water (T)'Old, Pages_Held (T)'Old,
                    Owner (T)'Old, Limit (T)'Old)
                  and then Valid (T);

    procedure Vacate
      (T      : in out Table;
       H      : Handle;
       Cause  : Vacancy_Cause;
       Result : out Remove_Result)
    is
        S : Slot_Index;
        E : Slot_Entry;
    begin
        case Check (T, H) is
            when Invalid =>
                Result := (Check => Invalid);
            when Stale =>
                Result := (Check => Stale);
            when Revoked =>
                Result := (Check => Revoked);
            when Current =>
                S := Slot_Of (H);
                E := Entry_Of (T, S);
                Lemma_Entry_In_Model (T, S);
                if E.Gen < Last_Generation then
                    Vacate_To_Free (T, S, Cause);
                else
                    Vacate_To_Retired (T, S, Cause);
                end if;
                Lemma_Entry_In_Model (T, S);
                Result := (Check => Current, Removed => E.Item);
        end case;
    end Vacate;

    procedure Close (T : in out Table; H : Handle; Result : out Remove_Result)
    is
    begin
        Vacate (T, H, Closed, Result);
    end Close;

    procedure Revoke (T : in out Table; H : Handle; Result : out Remove_Result)
    is
    begin
        Vacate (T, H, Revoked, Result);
    end Revoke;

    procedure Release_All (T : in out Table) is
    begin
        if T.Dir /= null then
            for L in reverse Leaf_Index range 0 .. T.Leaves - 1 loop
                Return_Leaf (T.Dir.all (L));
                pragma Loop_Invariant
                  (T.Dir /= null
                   and then (for all K in Leaf_Index =>
                               (T.Dir (K) /= null) = (K < L)));
            end loop;
            Return_Directory (T.Dir);
        end if;
        T.High_Water := 0;
        T.Leaves     := 0;
        T.Live       := 0;
        T.Free_Head  := No_Link;
        T.Free_Tail  := No_Link;
        T.Charged    := 0;
    end Release_All;

end Capability_Tables;
