with Interfaces; use Interfaces;
with System;
with Mesa_FFI; use Mesa_FFI;
with Compositor_Pool;
with Compositor_Repaint;
with Compositor_Damage;
function Native_Pool_Test return Boolean is
   package P renames Compositor_Pool;
   package R renames Compositor_Repaint;
   package D renames Compositor_Damage;
   use type System.Address, P.Ticket;
   type Pixels is array (Natural range 0 .. 1_023) of Unsigned_32;
   type Buffers is array (P.Live_Slot) of aliased Pixels;
   Storage : Buffers := (others => (others => 16#DEAD_BEEF#));
   Held : Pixels := (others => 0);
   Handles : array (P.Live_Slot) of System.Address;
   Background : aliased Unsigned_32 := 16#FF10_1820#;
   Foreground : aliased Unsigned_32 := 16#FF00_FF00#;
   BI : aliased Image := (Background'Address, 1, 1, 4, 0);
   FI : aliased Image := (Foreground'Address, 1, 1, 4, 0);
   TI : aliased Image;
   Ctx, BG, FG : System.Address;
   Pool : P.State := P.Open (71);
   Repaint : R.State := R.Open ((0, 0, 32, 32));
   Work : D.State;
   T, Sent : P.Ticket;
   Window : D.Box := (1, 2, 7, 7);
   Held_Buffer : P.Slot := 0;
   Repainted : Natural := 0;
   function Draw_Box (Target, Source : System.Address; Area : D.Box) return Boolean is
      Cmd : aliased Draw := (0, 0, 1, 1, 0, 0, 32, 32,
        Word (Area.Left), Word (Area.Top), Word (Area.Right - Area.Left),
        Word (Area.Bottom - Area.Top), 0);
   begin
      return Render (Ctx, Target, Source, Cmd'Access) = 0;
   end Draw_Box;
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
   procedure Report (Pixels_Repainted : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_pool_report";
begin
   Ctx := Create;
   if Ctx = System.Null_Address then return False; end if;
   BG := Import_Image (Ctx, BI'Access); FG := Import_Image (Ctx, FI'Access);
   if BG = System.Null_Address or FG = System.Null_Address then return False; end if;
   for B in P.Live_Slot loop
      TI := (Storage (B)'Address, 32, 32, 128, 1);
      Handles (B) := Import_Image (Ctx, TI'Access);
      if Handles (B) = System.Null_Address then return False; end if;
   end loop;
   for Frame in 1 .. 96 loop
      if Frame mod 5 = 0 and Held_Buffer /= 0 then
         P.Retire_Display (Pool, P.Displayed (Pool), True);
         Held_Buffer := 0;
      end if;
      R.Invalidate (Repaint, Window);
      Window := ((Frame * 3) mod 26, (Frame * 7) mod 27,
                 (Frame * 3) mod 26 + 6, (Frame * 7) mod 27 + 5);
      R.Invalidate (Repaint, Window);
      Foreground := 16#FF00_4000# or Unsigned_32 (Frame);
      P.Acquire (Pool, T);
      if T = P.None or else not P.Writable (Pool, T) then return False; end if;
      R.Take (Repaint, T.Buffer, Work);
      P.Start_Render (Pool, T);
      for I in 1 .. D.Count (Work) loop
         declare
            Area : constant D.Box := D.Item (Work, I);
            Intersection : constant D.Box :=
              (Natural'Max (Area.Left, Window.Left), Natural'Max (Area.Top, Window.Top),
               Natural'Min (Area.Right, Window.Right), Natural'Min (Area.Bottom, Window.Bottom));
         begin
            if not Draw_Box (Handles (T.Buffer), BG, Area) then return False; end if;
            Repainted := Repainted + (Area.Right - Area.Left) * (Area.Bottom - Area.Top);
            -- Abort after a real completed background draw: target is partial,
            -- but the synchronous Mesa adapter is quiescent. Force repair later.
            exit when Frame mod 17 = 0;
            if D.Valid (Intersection) and then not Draw_Box (Handles (T.Buffer), FG, Intersection)
            then return False; end if;
         end;
      end loop;
      if Frame mod 17 = 0 then
         R.Failed_Render (Repaint, T.Buffer);
         P.Finish_Render (Pool, T, P.Failed_Quiescent);
      else
         for Y in 0 .. 31 loop
            for X in 0 .. 31 loop
               declare
                  Expected : constant Unsigned_32 :=
                    (if X >= Window.Left and X < Window.Right and
                        Y >= Window.Top and Y < Window.Bottom then Foreground else Background);
               begin
                  if Storage (T.Buffer) (Y * 32 + X) /= Expected then
                     Mismatch (Unsigned_32 (Frame), Unsigned_32 (Y * 32 + X),
                       Storage (T.Buffer) (Y * 32 + X), Expected);
                     return False;
                  end if;
               end;
            end loop;
         end loop;
         P.Finish_Render (Pool, T, P.Completed);
      end if;
      if Held_Buffer /= 0 and then Storage (Held_Buffer) /= Held then return False; end if;
      P.Present (Pool, Sent);
      if Sent /= P.None then
         Held_Buffer := Sent.Buffer;
         Held := Storage (Sent.Buffer);
      end if;
      if P.Faulted (Pool) then return False; end if;
   end loop;
   if Repainted >= 96 * 1_024 then return False; end if;
   if Held_Buffer /= 0 then P.Retire_Display (Pool, P.Displayed (Pool), True); end if;
   -- Direct-front ownership simulation using real native Mesa target writes.
   -- Latch/retirement signals are test evidence, not physical scanout fences.
   Pool := P.Open (72);
   declare
      Previous, Busy : P.Ticket;
      procedure Front_Report
        with Import, Convention => C, External_Name => "compositor_front_report";
   begin
      for Frame in 1 .. 64 loop
         Previous := P.Front (Pool);
         P.Acquire (Pool, T);
         if T = P.None or else T.Buffer = Previous.Buffer then return False; end if;
         Foreground := 16#FF20_4000# or Unsigned_32 (Frame);
         P.Start_Render (Pool, T);
         if not Draw_Box (Handles (T.Buffer), FG, (0, 0, 32, 32)) then return False; end if;
         P.Finish_Render (Pool, T, P.Completed);
         for Pixel of Storage (T.Buffer) loop
            if Pixel /= Foreground then return False; end if;
         end loop;
         P.Present (Pool, Sent);
         if Sent /= T then return False; end if;
         -- Render the third allocation while both front and pending stay held.
         P.Acquire (Pool, T);
         if T = P.None or else T.Buffer = Previous.Buffer or else T.Buffer = Sent.Buffer then return False; end if;
         P.Start_Render (Pool, T);
         if not Draw_Box (Handles (T.Buffer), BG, (0, 0, 32, 32)) then return False; end if;
         P.Finish_Render (Pool, T, P.Completed);
         if Previous /= P.None then
            if Storage (Previous.Buffer) /= Held then return False; end if;
            P.Acquire (Pool, Busy);
            if Busy /= P.None or else P.Faulted (Pool) then return False; end if;
            -- New scene revisions replace only the completed ready allocation.
            for Revision in 1 .. 4 loop
               Busy := P.Ready (Pool);
               P.Acquire (Pool, T, Replace_Ready => True);
               if T = P.None or else T.Buffer /= Busy.Buffer or else
                 T.Serial <= Busy.Serial or else T.Buffer = Previous.Buffer or else
                 T.Buffer = Sent.Buffer
               then return False; end if;
               Foreground := 16#FF40_0000# or Shift_Left (Unsigned_32 (Frame), 8) or Unsigned_32 (Revision);
               P.Start_Render (Pool, T);
               if not Draw_Box (Handles (T.Buffer), FG, (0, 0, 32, 32)) then return False; end if;
               P.Finish_Render (Pool, T, P.Completed);
               for Pixel of Storage (T.Buffer) loop
                  if Pixel /= Foreground then return False; end if;
               end loop;
               if Storage (Previous.Buffer) /= Held then return False; end if;
            end loop;
            Foreground := 16#FF20_4000# or Unsigned_32 (Frame);
         end if;
         for Pixel of Storage (Sent.Buffer) loop
            if Pixel /= Foreground then return False; end if;
         end loop;
         P.Latch_Display (Pool, Sent, Previous, True);
         if P.Faulted (Pool) or else P.Front (Pool) /= Sent then return False; end if;
         Held := Storage (Sent.Buffer);
      end loop;
      P.Retire_Front (Pool, P.Front (Pool), True);
      if P.Faulted (Pool) or else P.Front (Pool) /= P.None then return False; end if;
      Front_Report;
   end;
   for B in P.Live_Slot loop
      if Release (Ctx, Handles (B)) /= 0 then return False; end if;
   end loop;
   if Release (Ctx, BG) /= 0 or else Release (Ctx, FG) /= 0 then return False; end if;
   Destroy (Ctx);
   Report (Unsigned_32 (Repainted));
   return True;
end Native_Pool_Test;
