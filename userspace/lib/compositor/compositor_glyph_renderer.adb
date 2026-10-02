with Mesa_Masks;
with Compositor_Policy;
with Compositor_Formats;
package body Compositor_Glyph_Renderer with SPARK_Mode is
   use type C.Token, C.Lease, C.Phase, Mesa_Cache.Slot, Compositor_Policy.State;
   use type P.G.Physical_Extent;
   function View_Slot (I : C.Slot) return Mesa_Cache.Mask_Slot is
     (Mesa_Cache.Mask_Slot'First + Mesa_Cache.Slot (I - 1));
   procedure Retire
     (S : in out State; Views : in out Mesa_Cache.State; T : C.Token; Safe : out Boolean)
     with Pre => Consistent (S), Post => Consistent (S) and S.CPU = S.CPU'Old and Queued (S) = Queued (S)'Old and
       (if Safe then Mesa_Cache.Can_Retire (Views))
   is
      OK : Boolean;
   begin
      Safe := False;
      if not Mesa_Cache.Can_Retire (Views) or else not C.Current (S.Registry, T) then return; end if;
      if C.Status (S.Registry, T) /= C.Retiring then
         C.Begin_Retirement (S.Registry, T, OK);
         if not OK then return; end if;
      end if;
      Mesa_Cache.Forget (Views, View_Slot (T.Position));
      if not Mesa_Cache.Can_Retire (Views) then S.Running := False; return; end if;
      if Storage.Has (S.Backing, T.Position) then
         pragma Assert (not C.Pinned (S.Registry, T.Position));
         pragma Assert (for all I in 1 .. S.Packet.Length => S.Packet.Items (I).Mask + 1 /= T.Position);
         Storage.Release (S.Backing, T.Position, OK);
         if not OK then S.Running := False; return; end if;
      end if;
      C.Retired (S.Registry, T, True);
      Safe := not C.Current (S.Registry, T);
   end Retire;
   procedure Obtain
     (S : in out State; Views : in out Mesa_Cache.State; Key : C.Key; T : out C.Token)
     with Pre => Consistent (S), Post => Consistent (S) and S.CPU = S.CPU'Old and Queued (S) = Queued (S)'Old
   is
      Found : C.Search_Result := C.Find (S.Registry, Key);
      Layout : constant P.L.Layout := P.L.Plan (Key.Scale);
      OK, Allocated : Boolean;
      Advance : Natural;
      Was_CPU : constant Boolean := S.CPU;
      Was_Queued : constant B.Count := S.Packet.Length;
   begin
      T := C.No_Token;
      if Found /= 0 then
         declare Existing : constant C.Token := C.At_Slot (S.Registry, Found); begin
            if C.Current (S.Registry, Existing) and then C.Status (S.Registry, Existing) = C.Ready and then
              Storage.Has (S.Backing, Found) and then (S.CPU or else not Mesa_Cache.Empty (Views, View_Slot (Found)))
            then T := Existing; end if;
         end;
         return;
      end if;
      -- One bounded pass may retire unpinned masks to relieve either payload
      -- pressure or arena fragmentation. No retry loop waits for a reader.
      for Attempt in 0 .. C.Slot'Last loop
         pragma Loop_Invariant (Consistent (S) and S.CPU = Was_CPU and S.Packet.Length = Was_Queued);
         if not Mesa_Cache.Can_Retire (Views) or else (not S.CPU and then Mesa_Cache.Mode (Views) /= Compositor_Policy.Ready) then return; end if;
         C.Reserve (S.Registry, Key, T);
         if T /= C.No_Token then
            if Storage.Has (S.Backing, T.Position) then S.Running := False; T := C.No_Token; return; end if;
            Storage.Allocate (S.Backing, T.Position, Layout, Allocated);
            if Allocated then
               Storage.Rasterize (S.Backing, T.Position, Key.Face, Key.Code, Layout, Advance, OK);
               if OK and not S.CPU then
                  Mesa_Masks.Ensure (Views, View_Slot (T.Position), Storage.Pixels (S.Backing, T.Position),
                    Layout, Compositor_Formats.Byte_Count (Storage.Capacity (S.Backing, T.Position)), OK);
               end if;
               if OK then C.Publish (S.Registry, T, True); return; end if;
            end if;
            Retire (S, Views, T, OK); T := C.No_Token;
            if not OK or Allocated then return; end if;
         end if;
         Found := C.Victim (S.Registry);
         if Found = 0 then return; end if;
         Retire (S, Views, C.At_Slot (S.Registry, Found), OK);
         if not OK then return; end if;
      end loop;
   end Obtain;
   procedure Discard (S : in out State) with Pre => Consistent (S),
     Post => Consistent (S) and Queued (S) = 0 and S.CPU = S.CPU'Old and Charged (S) = Charged (S)'Old
   is
      Before : constant C.Byte_Count := Charged (S);
   begin
      for I in 1 .. S.Packet.Length loop
         C.Complete (S.Registry, S.Reading (I), True);
         S.Reading (I) := C.No_Lease;
         pragma Loop_Invariant (C.Valid (S.Registry) and C.Charged (S.Registry) = Before);
      end loop;
      S.Packet.Length := 0;
   end Discard;
   procedure Flush (S : in out State; Views : in out Mesa_Cache.State; Success : out Boolean) is
   begin
      Success := False;
      if not Mesa_Cache.Can_Retire (Views) then S.Running := False; return; end if;
      if S.Packet.Length = 0 then Success := True; return; end if;
      Mesa_Masks.Render_Batch (Views, S.Target, S.Packet, Success);
      if Mesa_Cache.Can_Retire (Views) then Discard (S); end if;
      if not Success then S.Running := False; end if;
   end Flush;
   procedure Queue
     (S : in out State; Views : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot;
      Key : C.Key; Screen : P.G.Output; Origin : P.G.Logical_Point;
      Damage : P.G.Physical_Rectangle; Tint : B.A.Word; Success : out Boolean) is
      Full : constant B.A.Result := P.Plan (Screen, Origin);
      Draw : constant B.A.Result := (if Full.Visible then B.A.Clip (Full.Value, Screen.Width, Screen.Height, Damage)
                                    else (Visible => False));
      T : C.Token;
      R : C.Lease;
      OK : Boolean;
   begin
      Success := False;
      if not S.Running or else S.CPU or else not Mesa_Cache.Can_Retire (Views) or else
        Mesa_Cache.Mode (Views) /= Compositor_Policy.Ready or else
        not C.Same (Key, (Key.Face, Key.Code, Screen.Scale)) then return; end if;
      if not Draw.Visible then Success := True; return; end if;
      if S.Packet.Length > 0 and then (S.Packet.Length = B.Maximum or S.Target /= Target or
        S.Packet.Width /= Screen.Width or S.Packet.Height /= Screen.Height) then
         Flush (S, Views, OK); if not OK then return; end if;
      end if;
      if S.Packet.Length = 0 then
         S.Packet.Width := Screen.Width; S.Packet.Height := Screen.Height; S.Target := Target;
      end if;
      Obtain (S, Views, Key, T);
      pragma Assert (not S.CPU);
      if T = C.No_Token then return; end if;
      C.Acquire (S.Registry, T, R);
      if R = C.No_Lease then return; end if;
      B.Append (S.Packet, (T.Position - 1, Draw.Value, Tint), OK);
      if not OK then C.Complete (S.Registry, R, True); return; end if;
      S.Reading (S.Packet.Length) := R;
      Success := True;
   end Queue;
   procedure Use_Software (S : in out State; Views : in out Mesa_Cache.State; Safe : out Boolean) is
   begin
      Safe := False;
      if not Mesa_Cache.Can_Retire (Views) then S.Running := False; return; end if;
      if S.CPU and then (for all I in Mesa_Cache.Mask_Slot => Mesa_Cache.Empty (Views, I)) then
         Safe := True; return;
      end if;
      Discard (S);
      for I in Mesa_Cache.Mask_Slot loop
         pragma Loop_Invariant (Consistent (S) and Queued (S) = 0 and Mesa_Cache.Can_Retire (Views));
         pragma Loop_Invariant (for all J in Mesa_Cache.Mask_Slot => (if J < I then Mesa_Cache.Empty (Views, J)));
         Mesa_Cache.Forget (Views, I);
         if not Mesa_Cache.Can_Retire (Views) then S.Running := False; return; end if;
      end loop;
      if not S.CPU then S.Running := True; end if;
      S.CPU := True; Safe := True;
   end Use_Software;
   procedure Paint (S : in out State; Views : in out Mesa_Cache.State;
                    Key : C.Key; Screen : P.G.Output; Origin : P.G.Logical_Point;
                    Damage : P.G.Physical_Rectangle; Target : in out Software.Pixels;
                    Pitch : Positive; Tint : Software.Word; Success : out Boolean) is
      T : C.Token;
      R : C.Lease;
      Area : constant P.G.Physical_Rectangle := Software.Bounds (Screen, Origin, Damage);
   begin
      Success := False;
      if not S.Running or else not S.CPU or else not Mesa_Cache.Can_Retire (Views) or else
        not C.Same (Key, (Key.Face, Key.Code, Screen.Scale)) then return; end if;
      if Area.Left >= Area.Right or Area.Top >= Area.Bottom then Success := True; return; end if;
      Obtain (S, Views, Key, T);
      if T = C.No_Token or else not Storage.Can_Paint (S.Backing, T.Position, Screen) then return; end if;
      C.Acquire (S.Registry, T, R);
      if R = C.No_Lease then return; end if;
      pragma Assert (C.Pinned (S.Registry, T.Position));
      Storage.Paint (S.Backing, T.Position, Screen, Origin, Damage, Target, Pitch, Tint, Success);
      C.Complete (S.Registry, R, True);
   end Paint;
   procedure Shutdown (S : in out State; Views : in out Mesa_Cache.State; Safe : out Boolean) is
      OK : Boolean;
      T : C.Token;
   begin
      Safe := False; S.Running := False;
      if not Mesa_Cache.Can_Retire (Views) then return; end if;
      Discard (S);
      for I in C.Slot loop
         pragma Loop_Invariant (Consistent (S) and Queued (S) = 0 and Mesa_Cache.Can_Retire (Views));
         T := C.At_Slot (S.Registry, I);
         if C.Current (S.Registry, T) then
            Retire (S, Views, T, OK); if not OK then return; end if;
         end if;
      end loop;
      Safe := Charged (S) = 0;
   end Shutdown;
end Compositor_Glyph_Renderer;
