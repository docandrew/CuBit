with Ada.Text_IO;
with Compositor_Damage;
with Compositor_Repaint; use Compositor_Repaint;
procedure Repaint_Tests is
   package D renames Compositor_Damage;
   use type D.Box;
   S : State := Open ((0, 0, 32, 32));
   Areas : D.State;
   type Pixels is array (Slot, 0 .. 31, 0 .. 31) of Boolean;
   Dirty : Pixels := (others => (others => (others => True)));
   procedure Verify is
      Found : Boolean;
   begin
      pragma Assert (Valid (S));
      for B in Slot loop
         for Y in 0 .. 31 loop
            for X in 0 .. 31 loop
               Found := False;
               for I in 1 .. D.Count (Pending (S, B)) loop
                  declare R : constant D.Box := D.Item (Pending (S, B), I);
                  begin
                     Found := Found or (X >= R.Left and X < R.Right and Y >= R.Top and Y < R.Bottom);
                  end;
               end loop;
               pragma Assert (not Dirty (B, Y, X) or Found);
            end loop;
         end loop;
      end loop;
   end Verify;
begin
   for Step in 1 .. 300 loop
      declare
         X : constant Natural := (Step * 7) mod 31;
         Y : constant Natural := (Step * 11) mod 31;
         B : constant Slot := Slot (Step mod 3 + 1);
      begin
         Invalidate (S, (X, Y, X + 1, Y + 1));
         for J in Slot loop Dirty (J, Y, X) := True; end loop;
         Verify;
         Take (S, B, Areas);
         for YY in 0 .. 31 loop
            for XX in 0 .. 31 loop Dirty (B, YY, XX) := False; end loop;
         end loop;
         -- A change arriving after the snapshot must remain queued.
         Invalidate (S, (31, 31, 32, 32));
         for J in Slot loop Dirty (J, 31, 31) := True; end loop;
         if Step mod 11 = 0 then
            Failed_Render (S, B);
            for YY in 0 .. 31 loop
               for XX in 0 .. 31 loop Dirty (B, YY, XX) := True; end loop;
            end loop;
         end if;
         Verify;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("repaint: PASS 600 independent dirty-grid checks");
   declare
      type Image is array (0 .. 31, 0 .. 31) of Natural;
      Scene : Image := (others => (others => 0));
      type Targets is array (Slot) of Image;
      Backing : Targets := (others => (others => (others => Natural'Last)));
      History : State := Open ((0, 0, 32, 32));
      Writer : Slot := 1;
      Painted, Idle : Natural := 0;
      procedure Paint (Area : D.Box) is
      begin
         for Y in Area.Top .. Area.Bottom - 1 loop
            for X in Area.Left .. Area.Right - 1 loop
               Backing (Writer) (Y, X) := Scene (Y, X);
            end loop;
         end loop;
      end Paint;
   begin
      -- Independent pixel history: mutate the latest scene, repair old slot
      -- contents only at visible demand, then draw the new damage and rotate.
      -- Poisoned initial storage and idle gaps expose premature queue clearing.
      for Step in 1 .. 1_000 loop
         declare
            Work : constant Boolean := Step mod 5 /= 0;
            Full : constant Boolean := Step mod 13 = 1;
            X : constant Natural := (Step * 7) mod 31;
            Y : constant Natural := (Step * 11) mod 31;
            Changed : constant D.Box := (X, Y, X + 1, Y + 1);
            Before : constant Targets := Backing;
         begin
            if Work then Scene (Y, X) := Step; end if;
            if Preparation_Required (History, Writer, Work, Full) then
               Take (History, Writer, Areas);
               for I in 1 .. D.Count (Areas) loop Paint (D.Item (Areas, I)); end loop;
            end if;
            if Work then
               declare Area : constant D.Box := (if Full then (0, 0, 32, 32) else Changed);
               begin
                  Paint (Area);
                  Invalidate (History, Area);
                  Take (History, Writer, Areas);
               end;
               pragma Assert (Backing (Writer) = Scene);
               -- Slots other than the authorized writer remain untouched.
               for B in Slot loop
                  pragma Assert (B = Writer or else Backing (B) = Before (B));
               end loop;
               Writer := (if Writer = Slot'Last then Slot'First else Writer + 1);
               Painted := Painted + 1;
            else
               pragma Assert (Backing = Before);
               Idle := Idle + 1;
            end if;
         end;
      end loop;
      pragma Assert (Painted = 800 and Idle = 200);
      Ada.Text_IO.Put_Line ("repaint: PASS 800 exact deferred frames, 200 idle gaps, three targets");
   end;
   declare
      function Inside (R : D.Box; X, Y : Natural) return Boolean is
        (X >= R.Left and X < R.Right and Y >= R.Top and Y < R.Bottom);
      Checks : Natural := 0;
   begin
      for Step in 0 .. 4095 loop
         declare
            X : constant Natural := Step mod 8;
            Y : constant Natural := (Step / 8) mod 8;
            A : constant D.Box := (X, Y, X + 1 + (Step / 64) mod 8, Y + 1 + (Step / 512) mod 8);
            U : constant D.Box := (2, 2, 12, 12);
            C : constant D.Box := ((Step * 3) mod 14, (Step * 5) mod 14,
                                  (Step * 3) mod 14 + 2, (Step * 5) mod 14 + 2);
         begin
            for Pending_Work in Boolean loop
               declare R : constant Repair_Plan := Before_Draw (A, U, C, Pending_Work); begin
                  for PY in 0 .. 15 loop
                     for PX in 0 .. 15 loop
                        if Inside (A, PX, PY) and then
                          (not Pending_Work or else not Inside (U, PX, PY) or else Inside (C, PX, PY))
                        then pragma Assert (Covers_Point (R, PX, PY)); end if;
                        pragma Assert (not Covers_Point (R, PX, PY) or else Inside (A, PX, PY));
                        -- Exact independent bitmap oracle, including elision inside Upcoming.
                        pragma Assert (Covers_Point (R, PX, PY) =
                          (Inside (A, PX, PY) and (not Pending_Work or not Inside (U, PX, PY) or Inside (C, PX, PY))));
                     end loop;
                  end loop;
                  Checks := Checks + 1;
               end;
            end loop;
         end;
      end loop;
      declare
         R : constant Repair_Plan := Before_Draw
           ((Natural'Last - 4, Natural'Last - 4, Natural'Last, Natural'Last),
            (0, 0, Natural'Last, Natural'Last),
            (Natural'Last - 2, Natural'Last - 2, Natural'Last, Natural'Last), True);
      begin
         pragma Assert (R (5) = (Natural'Last - 2, Natural'Last - 2, Natural'Last, Natural'Last));
         for I in 1 .. 4 loop pragma Assert (not D.Valid (R (I))); end loop;
      end;
      Ada.Text_IO.Put_Line ("repaint: PASS" & Checks'Image & " independent repair/cursor coverage grids and extreme edges");
   end;
   declare
      type Image is array (0 .. 31, 0 .. 31) of Natural;
      Scene : Image := (others => (others => 0));
      type Targets is array (Slot) of Image;
      Backing : Targets := (others => (others => (others => Natural'Last)));
      History : State := Open ((0, 0, 32, 32));
      Writer : Slot := 1;
      Changed : constant D.Box := (4, 4, 28, 28);
      Old_Cursor : D.Box := (0, 0, 0, 0);
      Saved : Image := Scene;
      Pixels, Baseline : Natural := 0;
      procedure Paint (Area : D.Box) is
      begin
         if not D.Valid (Area) then return; end if;
         for Y in Area.Top .. Area.Bottom - 1 loop
            for X in Area.Left .. Area.Right - 1 loop
               Backing (Writer) (Y, X) := Scene (Y, X);
               Pixels := Pixels + 1;
            end loop;
         end loop;
      end Paint;
   begin
      for Step in 1 .. 600 loop
         declare
            X : constant Natural := (Step * 7) mod 30;
            Y : constant Natural := (Step * 11) mod 30;
            Cursor : constant D.Box := (X, Y, X + 2, Y + 2);
            Before : constant Targets := Backing;
            Repair_Pixels : Natural;
         begin
            for PY in Changed.Top .. Changed.Bottom - 1 loop
               for PX in Changed.Left .. Changed.Right - 1 loop Scene (PY, PX) := Step; end loop;
            end loop;
            if D.Valid (Old_Cursor) then Invalidate (History, Old_Cursor); end if;
            Take (History, Writer, Areas);
            D.Add (Areas, Cursor);
            Repair_Pixels := Pixels;
            for I in 1 .. D.Count (Areas) loop
               declare A : constant D.Box := D.Item (Areas, I); begin
                  Baseline := Baseline + (A.Right - A.Left) * (A.Bottom - A.Top);
                  for Region of Before_Draw (A, Changed, Cursor, True) loop Paint (Region); end loop;
               end;
            end loop;
            -- Save/draw/restore the transient cursor between repair and redraw.
            for PY in Cursor.Top .. Cursor.Bottom - 1 loop
               for PX in Cursor.Left .. Cursor.Right - 1 loop
                  pragma Assert (Backing (Writer) (PY, PX) = Scene (PY, PX));
                  Saved (PY, PX) := Backing (Writer) (PY, PX);
                  Backing (Writer) (PY, PX) := Natural'Last;
                  Backing (Writer) (PY, PX) := Saved (PY, PX);
               end loop;
            end loop;
            Repair_Pixels := Pixels - Repair_Pixels;
            Paint (Changed);
            pragma Assert (Backing (Writer) = Scene);
            for B in Slot loop pragma Assert (B = Writer or else Backing (B) = Before (B)); end loop;
            Invalidate (History, Changed);
            Invalidate (History, Cursor);
            -- Final displayed cursor is deliberately left in this retained slot.
            for PY in Cursor.Top .. Cursor.Bottom - 1 loop
               for PX in Cursor.Left .. Cursor.Right - 1 loop Backing (Writer) (PY, PX) := Natural'Last; end loop;
            end loop;
            Old_Cursor := Cursor;
            Writer := (if Writer = Slot'Last then Slot'First else Writer + 1);
         end;
      end loop;
      -- Compare actual repair pixels against the same region list without elision.
      pragma Assert (Pixels - 600 * 24 * 24 < Baseline / 2);
      Ada.Text_IO.Put_Line ("repaint: PASS 600 exact retained/cursor frames; repair pixels" &
        Natural'Image (Pixels - 600 * 24 * 24) & " versus" & Baseline'Image);
   end;
end Repaint_Tests;
