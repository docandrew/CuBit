package body Client_Glyphs with SPARK_Mode is
   use type C.Token, C.Phase;
   procedure Clear (V : in out View) with Post => not Ready (V) is
   begin
      V.Mask := C.No_Token; V.Reading := C.No_Lease;
      V.Address := System.Null_Address; V.Layout := L.Plan ((1, 1));
   end Clear;
   procedure Retire (S : in out State; T : C.Token; OK : out Boolean)
     with Pre => Valid (S), Post => Valid (S)
   is
   begin
      OK := False;
      if not C.Current (S.Registry, T) then return; end if;
      if C.Status (S.Registry, T) /= C.Retiring then
         C.Begin_Retirement (S.Registry, T, OK);
         if not OK then return; end if;
      end if;
      if Storage.Has (S.Backing, T.Position) then
         pragma Assert (not C.Pinned (S.Registry, T.Position));
         Storage.Release (S.Backing, T.Position, OK);
         if not OK then S.Running := False; return; end if;
      end if;
      C.Retired (S.Registry, T, True);
      OK := not C.Current (S.Registry, T);
   end Retire;
   procedure Obtain (S : in out State; Key : C.Key; T : out C.Token)
     with Pre => Valid (S), Post => Valid (S)
   is
      Found : C.Search_Result := C.Find (S.Registry, Key);
      Layout : constant L.Layout := L.Plan (Key.Scale);
      OK, Allocated : Boolean;
      Advance : Natural;
   begin
      T := C.No_Token;
      if not S.Running then return; end if;
      if Found /= 0 then
         declare Existing : constant C.Token := C.At_Slot (S.Registry, Found); begin
            if C.Current (S.Registry, Existing) and then
              C.Status (S.Registry, Existing) = C.Ready and then Storage.Has (S.Backing, Found)
            then T := Existing; end if;
         end;
         return;
      end if;
      -- At most one pass over the fixed slots. Never wait on a pinned reader.
      for Attempt in 0 .. C.Slot'Last loop
         pragma Loop_Invariant (Valid (S));
         C.Reserve (S.Registry, Key, T);
         if T /= C.No_Token then
            if Storage.Has (S.Backing, T.Position) then
               S.Running := False; T := C.No_Token; return;
            end if;
            Storage.Allocate (S.Backing, T.Position, Layout, Allocated);
            if Allocated then
               Storage.Rasterize (S.Backing, T.Position, Key.Face, Key.Code, Layout, Advance, OK);
               OK := OK and Advance in 1 .. Layout.Width;
               C.Publish (S.Registry, T, OK);
               if OK then return; end if;
            end if;
            Retire (S, T, OK); T := C.No_Token;
            if not OK or Allocated then return; end if;
         end if;
         Found := C.Victim (S.Registry);
         if Found = 0 then return; end if;
         Retire (S, C.At_Slot (S.Registry, Found), OK);
         if not OK then return; end if;
      end loop;
   end Obtain;
   procedure Read (S : in out State; Key : C.Key; V : in out View) is
      T : C.Token;
      R : C.Lease;
      Layout : constant L.Layout := L.Plan (Key.Scale);
   begin
      Clear (V);
      Obtain (S, Key, T);
      if T = C.No_Token or else not C.Current (S.Registry, T) or else
        C.Status (S.Registry, T) /= C.Ready or else not Storage.Has (S.Backing, T.Position) or else
        Storage.Capacity (S.Backing, T.Position) < Layout.Bytes or else
        Storage.Pixels (S.Backing, T.Position) = System.Null_Address
      then return; end if;
      C.Acquire (S.Registry, T, R);
      if R = C.No_Lease then return; end if;
      V.Mask := T; V.Reading := R;
      V.Address := Storage.Pixels (S.Backing, T.Position); V.Layout := Layout;
   end Read;
   procedure Finish (S : in out State; V : in out View) is
   begin
      if Ready (V) and then not Belongs (S, V) then return; end if;
      C.Complete (S.Registry, V.Reading, True);
      Clear (V);
   end Finish;
   procedure Close (S : in out State; Retired : out Boolean) is
      T : C.Token;
      OK : Boolean;
   begin
      S.Running := False;
      for I in C.Slot loop
         pragma Loop_Invariant (Valid (S));
         T := C.At_Slot (S.Registry, I);
         if C.Current (S.Registry, T) and then not C.Pinned (S.Registry, I) then
            Retire (S, T, OK);
            if not OK then Retired := Charged (S) = 0; return; end if;
         end if;
      end loop;
      Retired := Charged (S) = 0;
   end Close;
end Client_Glyphs;
