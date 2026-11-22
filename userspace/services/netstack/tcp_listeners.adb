package body TCP_Listeners with SPARK_Mode is

   --  Changing connection C alone changes a count only by what C itself
   --  counted before and after.
   procedure Lemma_Update
     (A, B : Child_Array; C : Connection_Index; L : Listener_Index; N : Natural)
   with Ghost,
        Pre  => N <= Connection_Index'Last + 1 and then
                (for all K in Connection_Index => (if K /= C then A (K) = B (K))),
        Post => (if N <= C then Waiting_Upto (B, L, N) = Waiting_Upto (A, L, N)
                 else Waiting_Upto (B, L, N) + Waits (A, C, L) =
                      Waiting_Upto (A, L, N) + Waits (B, C, L)),
        Subprogram_Variant => (Decreases => N)
   is
   begin
      if N > 0 then
         Lemma_Update (A, B, C, L, N - 1);
      end if;
   end Lemma_Update;

   procedure Lemma_Update_All (A, B : Child_Array; C : Connection_Index)
   with Ghost,
        Pre  => (for all K in Connection_Index => (if K /= C then A (K) = B (K))),
        Post => (for all L in Listener_Index =>
                   Waiting (B, L) + Waits (A, C, L) = Waiting (A, L) + Waits (B, C, L))
   is
   begin
      for L in Listener_Index loop
         Lemma_Update (A, B, C, L, Connection_Index'Last + 1);
         pragma Loop_Invariant
           (for all K in Listener_Index range 1 .. L =>
              Waiting (B, K) + Waits (A, C, K) = Waiting (A, K) + Waits (B, C, K));
      end loop;
   end Lemma_Update_All;

   --  No connection waits on L: its count is zero.
   procedure Lemma_None (Ch : Child_Array; L : Listener_Index; N : Natural)
   with Ghost,
        Pre  => N <= Connection_Index'Last + 1 and then
                (for all K in Connection_Index => Waits (Ch, K, L) = 0),
        Post => Waiting_Upto (Ch, L, N) = 0,
        Subprogram_Variant => (Decreases => N)
   is
   begin
      if N > 0 then
         Lemma_None (Ch, L, N - 1);
      end if;
   end Lemma_None;

   procedure Initialize (Item : out Table) is
   begin
      Item := (others => <>);
      for L in Listener_Index loop
         Lemma_None (Item.Children, L, Connection_Index'Last + 1);
         pragma Loop_Invariant
           (for all K in Listener_Index range 1 .. L => Waiting (Item.Children, K) = 0);
      end loop;
   end Initialize;

   function Find (Item : Table; Address : Unsigned_32; Port : Unsigned_16) return Handle is
   begin
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Address = Address and E.Port = Port then return E.Id; end if;
      end loop;
      return No_Handle;
   end Find;

   procedure Bind
     (Item : in out Table; Owner : Owner_Id; Address : Unsigned_32;
      Port : Unsigned_16; Listener : out Handle; Status : out Bind_Status) is
   begin
      Listener := No_Handle;
      if Address = 0 or Port = 0 then Status := Invalid_Address; return; end if;
      if Find (Item, Address, Port) /= No_Handle then Status := Address_In_Use; return; end if;
      if Item.Next_Id = Unsigned_64'Last then Status := Handles_Exhausted; return; end if;
      Status := Table_Full;
      for L in Listener_Index loop
         pragma Loop_Invariant
           (for all K in Listener_Index range 1 .. L - 1 => Item.Entries (K).Id /= No_Handle);
         if Item.Entries (L).Id = No_Handle then
            --  Above every open handle, so distinct from each; no connection
            --  waits on this slot, which was closed.
            pragma Assert (for all K in Listener_Index => Item.Entries (K).Id < Item.Next_Id);
            pragma Assert (for all K in Connection_Index => Waits (Item.Children, K, L) = 0);
            Lemma_None (Item.Children, L, Connection_Index'Last + 1);
            Listener := Item.Next_Id;
            Item.Entries (L) :=
              (Id => Listener, Owner => Owner, Address => Address, Port => Port, Held => 0);
            Item.Next_Id := Item.Next_Id + 1;
            Status := Bound;
            return;
         end if;
      end loop;
   end Bind;

   --  The open listener with handle Listener, if any.
   procedure Locate (Item : Table; Listener : Handle; Index : out Listener_Index;
                     Found : out Boolean)
     with Post => (if Found then Listener /= No_Handle and then
                     Item.Entries (Index).Id = Listener)
   is
   begin
      Index := Listener_Index'First;
      Found := False;
      if Listener = No_Handle then return; end if;
      for L in Listener_Index loop
         if Item.Entries (L).Id = Listener then
            Index := L;
            Found := True;
            return;
         end if;
      end loop;
   end Locate;

   procedure Reserve
     (Item : in out Table; Listener : Handle; Connection : Connection_Index;
      Deadline : Unsigned_64; Success : out Boolean)
   is
      L : Listener_Index;
      Found : Boolean;
   begin
      Success := False;
      --  A connection waits in at most one backlog.
      if Item.Children (Connection).State /= Vacant then return; end if;
      Locate (Item, Listener, L, Found);
      if not Found or else Item.Entries (L).Held = Maximum_Backlog then return; end if;
      declare
         Before : constant Child_Array := Item.Children with Ghost;
      begin
         Item.Entries (L).Held := Item.Entries (L).Held + 1;
         Item.Children (Connection) := (State => Handshaking, Parent => L, Deadline => Deadline);
         Lemma_Update_All (Before, Item.Children, Connection);
      end;
      Success := True;
   end Reserve;

   procedure Mark_Ready (Item : in out Table; Connection : Connection_Index) is
   begin
      if Item.Children (Connection).State = Handshaking then
         declare
            Before : constant Child_Array := Item.Children with Ghost;
         begin
            Item.Children (Connection).State := Ready;
            Lemma_Update_All (Before, Item.Children, Connection);
         end;
      end if;
   end Mark_Ready;

   function Has_Ready (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean is
      L : Listener_Index;
      Found : Boolean;
   begin
      Locate (Item, Listener, L, Found);
      if not Found or else Item.Entries (L).Owner /= Owner then return False; end if;
      for C in Connection_Index loop
         if Item.Children (C).State = Ready and then Item.Children (C).Parent = L then
            return True;
         end if;
      end loop;
      return False;
   end Has_Ready;

   --  Take Connection out of its backlog.
   procedure Vacate (Item : in out Table; Connection : Connection_Index)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then not Pending (Item, Connection) and then
                  Item.Next_Id = Item.Next_Id'Old and then
                  (for all K in Listener_Index =>
                     Item.Entries (K).Id = Item.Entries'Old (K).Id and then
                     Item.Entries (K).Owner = Item.Entries'Old (K).Owner) and then
                  (for all C in Connection_Index =>
                     (if C /= Connection then Item.Children (C) = Item.Children'Old (C)))
   is
      P : constant Listener_Index := Item.Children (Connection).Parent;
   begin
      if Item.Children (Connection).State /= Vacant then
         declare
            Before : constant Child_Array := Item.Children with Ghost;
         begin
            Item.Children (Connection) := (others => <>);
            Lemma_Update_All (Before, Item.Children, Connection);
            pragma Assert (Item.Entries (P).Held > 0);
            Item.Entries (P).Held := Item.Entries (P).Held - 1;
         end;
      end if;
   end Vacate;

   procedure Accept_Ready
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Connection : out Connection_Index; Found : out Boolean)
   is
      L : Listener_Index;
      Open : Boolean;
   begin
      Connection := 0;
      Found := False;
      Locate (Item, Listener, L, Open);
      if not Open or else Item.Entries (L).Owner /= Owner then return; end if;
      for C in Connection_Index loop
         if Item.Children (C).State = Ready and then Item.Children (C).Parent = L then
            Connection := C;
            Vacate (Item, C);
            Found := True;
            return;
         end if;
      end loop;
   end Accept_Ready;

   procedure Remove (Item : in out Table; Connection : Connection_Index) is
   begin
      Vacate (Item, Connection);
   end Remove;

   --  Empty listener L's backlog, reporting its connections, then close it.
   procedure Close_Index (Item : in out Table; L : Listener_Index;
                          Children : in out Connection_List)
     with Pre  => Valid (Item) and then Item.Entries (L).Id /= No_Handle,
          Post => Valid (Item) and then Item.Entries (L).Id = No_Handle and then
                  Item.Next_Id = Item.Next_Id'Old and then
                  (for all K in Listener_Index =>
                     (if K /= L then Item.Entries (K).Id = Item.Entries'Old (K).Id and then
                                     Item.Entries (K).Owner = Item.Entries'Old (K).Owner)) and then
                  (for all C in Connection_Index =>
                     Children (C) =
                       (Children'Old (C) or else
                        (Item.Children'Old (C).State /= Vacant and then
                         Item.Children'Old (C).Parent = L))) and then
                  (for all C in Connection_Index =>
                     (if Item.Children'Old (C).State /= Vacant and then
                         Item.Children'Old (C).Parent = L
                      then Item.Children (C).State = Vacant
                      else Item.Children (C) = Item.Children'Old (C)))
   is
      Old_Children : constant Child_Array := Item.Children with Ghost;
      Old_Report : constant Connection_List := Children with Ghost;
      Old_Entries : constant Listener_Array := Item.Entries with Ghost;
   begin
      for C in Connection_Index loop
         if Item.Children (C).State /= Vacant and then Item.Children (C).Parent = L then
            Children (C) := True;
            Vacate (Item, C);
         end if;
         pragma Loop_Invariant (Valid (Item));
         pragma Loop_Invariant (Item.Next_Id = Item.Next_Id'Loop_Entry);
         pragma Loop_Invariant
           (for all K in Listener_Index =>
              Item.Entries (K).Id = Old_Entries (K).Id and then
              Item.Entries (K).Owner = Old_Entries (K).Owner);
         pragma Loop_Invariant
           (for all D in Connection_Index =>
              (if D <= C then
                 Children (D) = (Old_Report (D) or else
                                 (Old_Children (D).State /= Vacant and then
                                  Old_Children (D).Parent = L))
               else Children (D) = Old_Report (D)));
         pragma Loop_Invariant
           (for all D in Connection_Index =>
              (if D <= C and then Old_Children (D).State /= Vacant and then
                  Old_Children (D).Parent = L
               then Item.Children (D).State = Vacant
               else Item.Children (D) = Old_Children (D)));
      end loop;
      pragma Assert (for all K in Connection_Index => Waits (Item.Children, K, L) = 0);
      Lemma_None (Item.Children, L, Connection_Index'Last + 1);
      Item.Entries (L) := (others => <>);
   end Close_Index;

   procedure Close
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Children : out Connection_List; Success : out Boolean)
   is
      L : Listener_Index;
      Open : Boolean;
   begin
      Children := [others => False];
      Success := False;
      Locate (Item, Listener, L, Open);
      if not Open or else Item.Entries (L).Owner /= Owner then return; end if;
      Close_Index (Item, L, Children);
      Success := True;
   end Close;

   procedure Close_Owned
     (Item : in out Table; Owner : Owner_Id; Children : out Connection_List) is
   begin
      Children := [others => False];
      for L in Listener_Index loop
         if Item.Entries (L).Id /= No_Handle and then Item.Entries (L).Owner = Owner then
            Close_Index (Item, L, Children);
         end if;
         pragma Loop_Invariant (Valid (Item));
         pragma Loop_Invariant
           (for all K in Listener_Index =>
              (if K <= L then
                 Item.Entries (K).Id = No_Handle or else Item.Entries (K).Owner /= Owner));
         pragma Loop_Invariant
           (for all C in Connection_Index => (if Children (C) then Item.Children (C).State = Vacant));
      end loop;
   end Close_Owned;

   procedure Expire
     (Item : in out Table; Now : Unsigned_64; Children : out Connection_List)
   is
      Old_Children : constant Child_Array := Item.Children with Ghost;
   begin
      Children := [others => False];
      for C in Connection_Index loop
         if Item.Children (C).State /= Vacant and then Now >= Item.Children (C).Deadline then
            Children (C) := True;
            Vacate (Item, C);
         end if;
         pragma Loop_Invariant (Valid (Item));
         pragma Loop_Invariant
           (for all D in Connection_Index =>
              (if Children (D) then
                 D <= C and then Old_Children (D).State /= Vacant and then
                 Item.Children (D).State = Vacant));
         pragma Loop_Invariant
           (for all D in Connection_Index =>
              (if D > C then Item.Children (D) = Old_Children (D)));
      end loop;
   end Expire;

   function Next_Deadline (Item : Table) return Unsigned_64 is
      Result : Unsigned_64 := Unsigned_64'Last;
   begin
      for C of Item.Children loop
         if C.State /= Vacant then Result := Unsigned_64'Min (Result, C.Deadline); end if;
      end loop;
      return Result;
   end Next_Deadline;
end TCP_Listeners;
