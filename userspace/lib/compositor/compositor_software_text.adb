package body Compositor_Software_Text with SPARK_Mode is
   use type C.Token, C.Lease, C.Phase, G.Physical_Extent;
   procedure Disable (S : in out State) is
   begin
      S.Running := False;
   end Disable;
   procedure Retire (S : in out State; T : C.Token; Success : out Boolean)
     with Pre => Consistent (S), Post => Consistent (S)
   is
      OK : Boolean;
   begin
      Success := False;
      if not C.Current (S.Registry, T) then return; end if;
      if C.Status (S.Registry, T) /= C.Retiring then
         C.Begin_Retirement (S.Registry, T, OK);
         if not OK then return; end if;
      end if;
      if Storage.Has (S.Backing, T.Position) then
         Storage.Release (S.Backing, T.Position, OK);
         if not OK then return; end if;
      end if;
      C.Retired (S.Registry, T, True);
      Success := not C.Current (S.Registry, T);
   end Retire;
   procedure Obtain (S : in out State; Key : C.Key; T : out C.Token)
     with Pre => Consistent (S), Post => Consistent (S)
   is
      Found : C.Search_Result := C.Find (S.Registry, Key);
      Layout : constant Storage.L.Layout := Storage.L.Plan (Key.Scale);
      OK, Allocated : Boolean;
      Advance : Natural;
   begin
      T := C.No_Token;
      if Found /= 0 then
         declare Existing : constant C.Token := C.At_Slot (S.Registry, Found); begin
            if C.Current (S.Registry, Existing) and then
              C.Status (S.Registry, Existing) = C.Ready and then
              Storage.Has (S.Backing, Found) then T := Existing; end if;
         end;
         return;
      end if;
      -- Admission/fragmentation relief makes one bounded pass, never waits.
      for Attempt in 0 .. C.Slot'Last loop
         pragma Loop_Invariant (Consistent (S));
         C.Reserve (S.Registry, Key, T);
         if T /= C.No_Token then
            if Storage.Has (S.Backing, T.Position) then T := C.No_Token; return; end if;
            Storage.Allocate (S.Backing, T.Position, Layout, Allocated);
            if Allocated then
               Storage.Rasterize (S.Backing, T.Position, Key.Face, Key.Code,
                                  Layout, Advance, OK);
               if OK then C.Publish (S.Registry, T, True); return; end if;
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
   procedure Paint
     (S : in out State; Key : C.Key; Screen : G.Output;
      Origin : G.Logical_Point; Damage : G.Physical_Rectangle;
      Target : in out Software.Pixels; Pitch : Positive;
      Tint : Software.Word; Success : out Boolean)
   is
      T : C.Token;
      R : C.Lease;
      Area : constant G.Physical_Rectangle := Software.Bounds (Screen, Origin, Damage);
   begin
      Success := False;
      if not S.Running or else not C.Same (Key, (Key.Face, Key.Code, Screen.Scale)) then return; end if;
      if Area.Left >= Area.Right or Area.Top >= Area.Bottom then Success := True; return; end if;
      Obtain (S, Key, T);
      if T = C.No_Token or else not Storage.Can_Paint (S.Backing, T.Position, Screen) then return; end if;
      C.Acquire (S.Registry, T, R);
      if R = C.No_Lease then return; end if;
      Storage.Paint (S.Backing, T.Position, Screen, Origin, Damage, Target, Pitch, Tint, Success);
      C.Complete (S.Registry, R, True);
   end Paint;
   procedure Shutdown (S : in out State; Success : out Boolean) is
      T : C.Token;
      OK : Boolean;
   begin
      for I in C.Slot loop
         pragma Loop_Invariant (Consistent (S));
         T := C.At_Slot (S.Registry, I);
         if C.Current (S.Registry, T) then
            Retire (S, T, OK);
            if not OK then Success := False; return; end if;
         end if;
      end loop;
      Success := Charged (S) = 0;
   end Shutdown;
end Compositor_Software_Text;
