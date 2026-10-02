with System; use System;
with System.Storage_Elements;
with Compositor_Cache;
with Compositor_Formats; use Compositor_Formats;
with Compositor_Policy; use Compositor_Policy;
with Ada.Text_IO;
procedure Cache_Tests is
   use type Word, Byte_Count;
   type Library is record Next : Natural := 1; Live : Natural := 0; end record;
   Imports, Releases, Stops, Draws : Natural := 0;
   Init_OK, Release_OK : Boolean := True;
   Mask_OK : Boolean := True;
   Mask_Imports : Natural := 0;
   Fail_Handle : Natural := 0;
   Outcome : Completion := Rendered;
   procedure Start (L : out Library; OK : out Boolean) is
   begin L := (Next => 1, Live => 0); OK := Init_OK; end Start;
   procedure Import_View (L : in out Library; D : Image; H : out Natural) is
      pragma Unreferenced (D);
   begin H := L.Next; L.Next := L.Next + 1; L.Live := L.Live + 1; Imports := Imports + 1; end Import_View;
   procedure Render_View (L : in out Library; T, S : Natural; D : Draw; R : out Completion) is
      pragma Unreferenced (L, T, S, D);
   begin Draws := Draws + 1; R := Outcome; end Render_View;
   procedure Release_View (L : in out Library; H : Natural; Safe : out Boolean) is
   begin Safe := Release_OK and H /= Fail_Handle;
      if Safe then Releases := Releases + 1; L.Live := L.Live - 1; end if;
   end Release_View;
   procedure Stop (L : in out Library) is
   begin pragma Assert (L.Live = 0); Stops := Stops + 1; end Stop;
   package Cache is new Compositor_Cache
     (Library, Natural, 0, Start, Import_View, Render_View, Release_View, Stop);
   use type Cache.Slot;
   function Valid_Mask (D : Image; Capacity : Byte_Count) return Boolean is
     (D.Pixels /= Null_Address and D.Width = 4 and D.Height = 4 and D.Pitch = 16 and
      D.Writable = 0 and Capacity >= 64);
   procedure Import_Mask (L : in out Library; D : Image; H : out Natural) is
   begin
      if Mask_OK then Import_View (L, D, H); Mask_Imports := Mask_Imports + 1;
      else H := 0; end if;
   end Import_Mask;
   procedure Ensure_Mask is new Cache.Ensure_Mask (Valid_Mask, Import_Mask);
   Batch_Calls : Natural := 0;
   Bad_Source : Natural := 0;
   function Fits_Batch (I : Cache.Batch_Index; S, T : Image) return Boolean is
     (I /= Bad_Source and S.Writable = 0 and T.Writable = 1);
   procedure Draw_Batch (L : in out Library; T : Natural;
                         Sources : Cache.Handle_Batch; Length : Cache.Batch_Count;
                         Result : out Completion) is
      pragma Unreferenced (L);
   begin
      pragma Assert (T = 1 and Length > 0);
      for I in 1 .. Length loop pragma Assert (Sources (I) = I + 1); end loop;
      Batch_Calls := Batch_Calls + 1;
      Result := Outcome;
   end Draw_Batch;
   procedure Render_Batch is new Cache.Render_Masks (Fits_Batch, Draw_Batch);
   function Addr (N : Natural) return Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Source : constant Image := (Addr (4096), 4, 4, 16, 0);
   Target : constant Image := (Addr (8192), 4, 4, 16, 1);
   D : constant Draw := (0,0,4,4,0,0,4,4,0,0,4,4,0);
   OK : Boolean;
   Index : Cache.Source_Slot;
begin
   for Enabled in Boolean loop
      for Works in Boolean loop
         declare S : Cache.State; begin
            Init_OK := Works;
            Cache.Initialize (S, Enabled);
            pragma Assert ((Cache.Mode (S) = Ready) = (Enabled and Works));
            Cache.Shutdown (S);
         end;
      end loop;
   end loop;
   Init_OK := True; Imports := 0; Releases := 0; Stops := 0;
   for Result in Completion loop
      for Length in Cache.Batch_Count loop
         declare
            S : Cache.State;
            Sources : Cache.Mask_Indices := (others => Cache.Mask_Slot'First);
            Before : constant Natural := Batch_Calls;
         begin
            Cache.Initialize (S, True);
            -- An empty packet neither looks up a missing target nor calls FFI.
            Render_Batch (S, 0, Sources, 0, OK);
            pragma Assert (OK and Batch_Calls = Before);
            Cache.Ensure (S, 0, Target, 64, OK); pragma Assert (OK);
            for I in 1 .. Length loop
               Sources (I) := Cache.Mask_Slot'First + Cache.Slot (I - 1);
               Ensure_Mask (S, Sources (I), Source, 64, OK); pragma Assert (OK);
            end loop;
            Outcome := Result;
            Render_Batch (S, 0, Sources, Length, OK);
            pragma Assert (OK = (Length = 0 or Result = Rendered));
            pragma Assert (Batch_Calls = Before + (if Length = 0 then 0 else 1));
            pragma Assert (Cache.Can_Retire (S) = (Length = 0 or Result /= Access_Unknown));
            if Cache.Can_Retire (S) then Cache.Shutdown (S); end if;
         end;
      end loop;
   end loop;
   for Missing in Boolean loop
      for Bad in Cache.Batch_Index loop
         declare
            S : Cache.State;
            Sources : Cache.Mask_Indices;
            Before : constant Natural := Batch_Calls;
         begin
            Cache.Initialize (S, True);
            Cache.Ensure (S, 0, Target, 64, OK); pragma Assert (OK);
            for I in Cache.Batch_Index loop
               Sources (I) := Cache.Mask_Slot'First + Cache.Slot (I - 1);
               if not Missing or I /= Bad then
                  Ensure_Mask (S, Sources (I), Source, 64, OK); pragma Assert (OK);
               end if;
            end loop;
            Bad_Source := (if Missing then 0 else Bad);
            Render_Batch (S, 0, Sources, 32, OK);
            pragma Assert (not OK and Batch_Calls = Before and Cache.Can_Retire (S));
            Cache.Shutdown (S);
         end;
      end loop;
   end loop;
   Bad_Source := 0; Outcome := Rendered;
   Imports := 0; Releases := 0; Stops := 0;
   for Cycle in 1 .. 100 loop
      declare S : Cache.State; begin
         Cache.Initialize (S, True);
         Cache.Ensure (S, 0, Target, 64, OK); pragma Assert (OK);
         Cache.Ensure_Source (S, Source, 64, Index, OK); pragma Assert (OK);
         Cache.Ensure_Source (S, Source, 64, Index, OK); pragma Assert (OK);
         pragma Assert (Imports = Cycle * 2);
         Cache.Render (S, 0, Index, D, OK); pragma Assert (OK);
         Cache.Forget_Source (S, Source.Pixels);
         Cache.Shutdown (S);
         pragma Assert (Imports = Releases and Stops = Cycle);
      end;
   end loop;
   for Result in Completion loop
      declare S : Cache.State; Before : Natural; begin
         Cache.Initialize (S, True);
         Cache.Ensure (S, 0, Target, 64, OK);
         Cache.Ensure_Source (S, Source, 64, Index, OK);
         Outcome := Result;
         Cache.Render (S, 0, Index, D, OK);
         pragma Assert (OK = (Result = Rendered));
         pragma Assert (Cache.Can_Retire (S) = (Result /= Access_Unknown));
         Before := Releases;
         if Cache.Can_Retire (S) then
            Cache.Shutdown (S); pragma Assert (Releases = Before + 2);
         end if;
      end;
   end loop;
   declare S : Cache.State; Before : Natural; begin
      Cache.Initialize (S, True);
      Cache.Ensure_Source (S, Source, 64, Index, OK);
      Before := Releases;
      Release_OK := False;
      Cache.Forget_Source (S, Source.Pixels);
      pragma Assert (not Cache.Can_Retire (S));
      pragma Assert (Cache.Mode (S) = Restart_Required and Releases = Before);
   end;
   -- Populate the complete mask table in the SAME context as color views.
   -- Repeated hits do not reimport; target retirement preserves every mask.
   Release_OK := True;
   declare S : Cache.State; Before : Natural; begin
      Cache.Initialize (S, True);
      Cache.Ensure (S, 0, Target, 64, OK); pragma Assert (OK);
      Cache.Ensure_Source (S, Source, 64, Index, OK); pragma Assert (OK);
      Before := Mask_Imports;
      for I in Cache.Mask_Slot loop
         Ensure_Mask (S, I, (Addr (Natural (I) * 4096), 4, 4, 16, 0), 64, OK); pragma Assert (OK);
         Ensure_Mask (S, I, (Addr (Natural (I) * 4096), 4, 4, 16, 0), 64, OK); pragma Assert (OK);
      end loop;
      pragma Assert (Mask_Imports = Before + 128);
      Cache.Forget_Targets (S);
      for I in Cache.Mask_Slot loop pragma Assert (not Cache.Empty (S, I)); end loop;
      pragma Assert (not Cache.Empty (S, Index));
      Cache.Shutdown (S); pragma Assert (Cache.Views_Clear (S));
   end;
   -- Every mask position can stop shutdown: never destroy the context after
   -- an uncertain release, including after a long successfully retired prefix.
   for Failed in Cache.Mask_Slot loop
      declare S : Cache.State; Before_Stops : constant Natural := Stops; begin
         Cache.Initialize (S, True);
         for I in Cache.Mask_Slot loop Ensure_Mask (S, I, Source, 64, OK); pragma Assert (OK); end loop;
         Fail_Handle := Natural (Failed - Cache.Mask_Slot'First + 1);
         Cache.Shutdown (S);
         pragma Assert (not Cache.Can_Retire (S) and Stops = Before_Stops);
         for I in Cache.Mask_Slot loop pragma Assert (Cache.Empty (S, I) = (I < Failed)); end loop;
         Fail_Handle := 0;
      end;
   end loop;
   -- Replacement must retire before reimport; malformed and failed imports
   -- cannot clear existing views or poison the client's independent slots.
   for Fault in 1 .. 3 loop
      declare S : Cache.State; Before : Natural; begin
         Cache.Initialize (S, True);
         Cache.Ensure_Source (S, Source, 64, Index, OK); pragma Assert (OK);
         Ensure_Mask (S, Cache.Mask_Slot'First, Source, 64, OK); pragma Assert (OK);
         Before := Mask_Imports;
         if Fault = 1 then
            Ensure_Mask (S, Cache.Mask_Slot'First, Source, 63, OK);
            pragma Assert (not OK and not Cache.Empty (S, Cache.Mask_Slot'First));
         else
            if Fault = 2 then Release_OK := False; else Mask_OK := False; end if;
            Ensure_Mask (S, Cache.Mask_Slot'First, (Addr (16384), 4, 4, 16, 0), 64, OK);
            pragma Assert (not OK and Cache.Can_Retire (S) = (Fault = 3));
            Release_OK := True; Mask_OK := True;
         end if;
         pragma Assert (not Cache.Empty (S, Index) and Mask_Imports = Before);
         if Cache.Can_Retire (S) then Cache.Shutdown (S); end if;
      end;
   end loop;
   Release_OK := True;
   Outcome := Rendered;
   for Failure in 0 .. 2 loop
      declare
         S : Cache.State;
         Before_Releases : Natural;
         Before_Imports : Natural;
         Before_Stops : Natural;
      begin
         Cache.Initialize (S, True);
         Cache.Ensure (S, 0, Target, 64, OK); pragma Assert (OK);
         Cache.Ensure (S, 1, (Addr (12288), 4, 4, 16, 1), 64, OK); pragma Assert (OK);
         Cache.Ensure_Source (S, Source, 64, Index, OK); pragma Assert (OK);
         Before_Releases := Releases; Before_Imports := Imports; Before_Stops := Stops;
         Fail_Handle := Failure;
         Cache.Forget_Targets (S);
         pragma Assert (not Cache.Empty (S, Index) and Stops = Before_Stops);
         if Failure = 0 then
            pragma Assert (Cache.Can_Retire (S) and Cache.Targets_Clear (S));
            pragma Assert (Releases = Before_Releases + 2);
            Cache.Forget_Targets (S); -- Duplicate retirement must not release twice.
            pragma Assert (Releases = Before_Releases + 2);
            Cache.Ensure_Source (S, Source, 64, Index, OK);
            pragma Assert (OK and Imports = Before_Imports);
            Cache.Ensure (S, 0, Target, 64, OK);
            Cache.Render (S, 0, Index, D, OK); pragma Assert (OK);
            Cache.Shutdown (S);
         else
            pragma Assert (not Cache.Can_Retire (S) and Cache.Mode (S) = Restart_Required);
            pragma Assert (Releases = Before_Releases + Failure - 1);
            pragma Assert (not Cache.Empty (S, 1));
            pragma Assert (Cache.Empty (S, 0) = (Failure = 2));
         end if;
         Fail_Handle := 0;
      end;
   end loop;
   declare S : Cache.State; I : Image := Source; begin
      Cache.Initialize (S, True);
      for N in 1 .. 8 loop
         I.Pixels := Addr (N * 4096);
         Cache.Ensure_Source (S, I, 64, Index, OK); pragma Assert (OK);
      end loop;
      I.Pixels := Addr (9 * 4096);
      Cache.Ensure_Source (S, I, 64, Index, OK);
      pragma Assert (not OK and Cache.Mode (S) = Disabled);
      Cache.Shutdown (S);
   end;
   Ada.Text_IO.Put_Line ("COMPOSITOR-CACHE: PASS init/failure/retirement/bounds and 100 reuse cycles");
   Ada.Text_IO.Put_Line ("COMPOSITOR-CACHE: PASS target-only retirement, idempotence, preserved sources and first/second-release faults");
   Ada.Text_IO.Put_Line ("COMPOSITOR-MASK-CACHE: PASS 128 retained masks, every shutdown failure position, duplicate imports and replacement faults");
   Ada.Text_IO.Put_Line ("COMPOSITOR-MASK-BATCH: PASS 0..32 lengths/all completions, ordered handles, every missing/invalid source before FFI");
end Cache_Tests;
