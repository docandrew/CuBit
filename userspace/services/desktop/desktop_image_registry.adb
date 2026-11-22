package body Desktop_Image_Registry with SPARK_Mode is
   use type I.Phase, I.Outcome, I.V.Source_Slot, C.Source_Key,
     C.Content_Version, Book.Entry_Record;
   function Last_Pressure (S : State) return Capacity_Pressure is (S.Pressure);
   function Upload_Work (S : State) return Boolean is
     (for some N in I.V.Client_Slot =>
       I.Current (S.Owners (N)) in I.Uploading | I.Ready_To_Upload);
   function Faulted (S : State) return Boolean is
     (for some N in I.V.Client_Slot => I.Current (S.Owners (N)) = I.Quarantined);
   function Resident_Keys (S : State) return Natural is
      Count : Natural := 0;
   begin
      for N in I.V.Client_Slot loop
         pragma Loop_Invariant (Count < Natural (N - I.V.Client_Slot'First) + 1);
         if S.Keys.Entries (N).Key /= C.No_Key then Count := Count + 1; end if;
      end loop;
      return Count;
   end Resident_Keys;
   procedure Note_Change (S : in out State; Key : C.Source_Key; Rows : C.Row_Band) is
   begin
      Book.Note (S.Keys, Key, C.Band (Rows.First, Rows.Last));
   end Note_Change;
   -- Close slot N's owner and return the slot to the free pool.
   procedure Release_Slot (S : in out State; N : I.V.Client_Slot; Safe : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid and
         (for all J in I.V.Client_Slot =>
            (if J /= N then S.Keys.Entries (J) = S.Keys.Entries'Old (J))) and
         S.Keys.Entries (N).Key in S.Keys.Entries'Old (N).Key | C.No_Key
   is
      Rearmed : Boolean;
   begin
      I.Close (S.Owners (N), True, Safe);
      if not Safe then return; end if;
      I.Rearm (S.Owners (N), Rearmed);
      Safe := Rearmed;
      if not Safe then return; end if;
      Book.Unbind (S.Keys, N);
      S.Retired (N) := False;
   end Release_Slot;
   -- Free the least-recently-used idle slot (other than Except when
   -- Exclude). Memory eviction (Need_Backing) only considers slots that hold
   -- an allocation; retired keys go first.
   procedure Evict (S : in out State; Except : I.V.Client_Slot; Exclude, Need_Backing : Boolean;
      Evicted : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid and
         -- Keys are only ever removed, never added or moved.
         (for all J in I.V.Client_Slot =>
            S.Keys.Entries (J).Key in S.Keys.Entries'Old (J).Key | C.No_Key) and
         (if Exclude then S.Keys.Entries (Except) = S.Keys.Entries'Old (Except))
   is
      Allowed, Retired_Only : Book.Candidates := (others => False);
   begin
      Evicted := False;
      for N in I.V.Client_Slot loop
         Allowed (N) := not (Exclude and then N = Except) and then S.Keys.Entries (N).Key /= C.No_Key and then
           I.Evictable (S.Owners (N)) and then (not Need_Backing or else I.Holds_Backing (S.Owners (N)));
         Retired_Only (N) := Allowed (N) and then S.Retired (N);
         pragma Loop_Invariant (if Exclude and Except <= N then not Allowed (Except) and not Retired_Only (Except));
         pragma Loop_Invariant (for all J in I.V.Client_Slot => (if Retired_Only (J) then Allowed (J)));
      end loop;
      if Book.Has_Candidate (Retired_Only) then Allowed := Retired_Only; end if;
      if not Book.Has_Candidate (Allowed) then return; end if;
      Release_Slot (S, Book.Victim (S.Keys, Allowed), Evicted);
   end Evict;
   procedure Ensure (S : in out State; Key : C.Source_Key; Version : C.Content_Version;
      Image : Compositor_Formats.Image; Bytes : Natural;
      Source : out I.V.Source_Ticket; Result : out I.Outcome) is
      N : I.V.Client_Slot := I.V.Client_Slot'First;
      Started, Evicted : Boolean;
   begin
      Source := I.V.No_Source; Result := I.Rejected; S.Pressure := None;
      if Key = C.No_Key or else Version = C.No_Version then return; end if;
      if Book.Holds (S.Keys, Key) then
         N := Book.Find (S.Keys, Key);
         if S.Retired (N) then return; end if;
      else
         if not Book.Has_Free (S.Keys) then
            Evict (S, N, False, False, Evicted);
            if not Book.Has_Free (S.Keys) then
               -- Only possible when every slot is read by this very scene or
               -- in flight: twice the surface limit makes it unreachable.
               S.Pressure := Slots_Full;
               Result := (if Faulted (S) then I.Unsafe else I.Deferred);
               return;
            end if;
         end if;
         N := Book.First_Free (S.Keys);
         if I.Current (S.Owners (N)) /= I.Fresh then
            -- Free slots always hold a rearmed owner; anything else is a bug.
            Result := I.Unsafe; return;
         end if;
         Book.Bind (S.Keys, N, Key);
      end if;
      Book.Touch (S.Keys, N);
      for Attempt in I.V.Client_Slot loop
         pragma Loop_Invariant (Valid (S) and D.Valid);
         pragma Loop_Invariant (S.Keys.Entries (N).Key = Key);
         I.Acquire (S.Owners (N), N, Version, Image, Bytes, S.Keys.Entries (N).Stale,
           Started, Source, Result);
         if Started then Book.Clear_Stale (S.Keys, N); end if;
         exit when Result /= I.Unaffordable;
         -- Return memory held by idle keys, least recently used first.
         Evict (S, N, True, True, Evicted);
         if not Evicted then S.Pressure := Memory_Exhausted; exit; end if;
      end loop;
   end Ensure;
   procedure Poll (S : in out State; Result : out I.Outcome) is
      N : I.V.Client_Slot;
   begin
      Result := I.Deferred;
      for Attempt in I.V.Client_Slot loop
         pragma Loop_Invariant (Valid (S) and D.Valid);
         N := S.Cursor;
         S.Cursor := (if N = I.V.Client_Slot'Last then I.V.Client_Slot'First else N + 1);
         if I.Current (S.Owners (N)) in I.Uploading | I.Ready_To_Upload then
            I.Poll (S.Owners (N), Result); return;
         end if;
      end loop;
   end Poll;
   procedure Forget (S : in out State; Pixels : System.Address; Safe : out Boolean) is
      Detached : Boolean;
   begin
      Safe := True;
      for N in I.V.Client_Slot loop
         pragma Loop_Invariant (Valid (S));
         I.Detach (S.Owners (N), Pixels, Detached);
         Safe := Safe and Detached;
      end loop;
   end Forget;
   procedure Retire (S : in out State; Key : C.Source_Key) is
   begin
      if Book.Holds (S.Keys, Key) then S.Retired (Book.Find (S.Keys, Key)) := True; end if;
   end Retire;
   procedure Collect (S : in out State) is
      Safe : Boolean;
   begin
      for N in I.V.Client_Slot loop
         pragma Loop_Invariant (Valid (S) and D.Valid);
         if S.Retired (N) and then I.Evictable (S.Owners (N)) then
            Release_Slot (S, N, Safe);
         end if;
      end loop;
   end Collect;
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean) is
   begin
      Safe := False;
      if not Capture_Retired then return; end if;
      for N in I.V.Client_Slot loop
         pragma Loop_Invariant (Valid (S) and D.Valid);
         if S.Keys.Entries (N).Key /= C.No_Key or else I.Current (S.Owners (N)) /= I.Fresh then
            Release_Slot (S, N, Safe);
            if not Safe then return; end if;
         end if;
      end loop;
      Safe := True;
   end Close;
end Desktop_Image_Registry;
