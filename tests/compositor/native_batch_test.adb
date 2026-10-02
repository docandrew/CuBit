with Interfaces; use Interfaces;
with System;
with Mesa_Cache;
with Mesa_Masks;
with Compositor_Mask_Batch;
with Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
with Compositor_Glyph_Cache;
with Compositor_Glyph_Memory;
with Compositor_Transform;
function Native_Batch_Test return Boolean is
   package B renames Compositor_Mask_Batch;
   package V renames Mesa_Cache;
   package L renames Compositor_Glyph_Layout;
   package M renames Compositor_Glyph_Memory;
   package C is new Compositor_Glyph_Cache;
   use type C.Token, C.Lease, M.Arena.Token;
   use type System.Address;
   use type V.Slot;
   type Pixels is array (Natural range <>) of Unsigned_32;
   type Bytes is array (Natural range <>) of Unsigned_8;
   Target, Serial : aliased Pixels (0 .. 84 * 72 - 1) with Alignment => 64;
   Masks : M.State;
   Backing : array (0 .. 2) of M.Arena.Token;
   Tickets : array (0 .. 2) of C.Token;
   Readers : array (B.Index) of C.Lease;
   Original : array (0 .. 2) of Bytes (0 .. 2047);
   Capacity : array (0 .. 2) of Natural;
   Addresses : array (0 .. 2) of System.Address;
   Cache : C.State := C.Open (8192);
   Views : V.State;
   Scales : constant array (0 .. 2) of L.G.UI_Scale := ((1, 1), (5, 4), (3, 2));
   Tints : constant array (0 .. 3) of Unsigned_32 :=
     (16#FF27_91D3#, 16#8027_91D3#, 16#00FF_FFFF#, 16#FFFF_FFFF#);
   Packet : B.Packet := (Width => 80, Height => 72, others => <>);
   Cmd : B.Command;
   OK : Boolean;
   Advance : Natural;
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   procedure Report with Import, Convention => C, External_Name => "compositor_batch_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
   function Channel (P : Unsigned_32; I : Natural) return Integer_64 is
     (Integer_64 (Shift_Right (P, I * 8) and 255));
   -- Fixed-point independent source-over arithmetic retains 16 fractional
   -- bits until final storage. Serial draws quantize the target after EACH
   -- flush; softpipe's batched float tiles quantize at the final flush.
   type Channels is array (0 .. 3) of Integer_64;
   Oracle : array (Target'Range) of Channels;
   procedure Accumulate (Command : B.Command) is
      D : B.A.Draw renames Command.Description;
      Q : constant Compositor_Transform.Quad := Compositor_Transform.Vertices (D, 80, 72);
      Layout : constant L.Layout := L.Plan (Scales (Command.Mask));
      function Sample_Axis (A, X, Y, Divisor : Integer_64; PX, PY, Size : Natural) return Natural is
         N : constant Integer_64 := A * 160 * 144 +
           (X - A) * Integer_64 (2 * PX + 1) * 144 +
           (Y - A) * Integer_64 (2 * PY + 1) * 160;
         Den : constant Integer_64 := Divisor * 160 * 144;
      begin
         if N <= 0 then return 0; end if;
         if N >= Den then return Size - 1; end if;
         return Natural (N * Integer_64 (Size) / Den);
      end Sample_Axis;
   begin
      for Y in Natural (D.Clip_Y) .. Natural (D.Clip_Y + D.Clip_H - 1) loop
         for X in Natural (D.Clip_X) .. Natural (D.Clip_X + D.Clip_W - 1) loop
            declare
               SX : constant Natural := Sample_Axis (Q.Corners (0).U, Q.Corners (1).U,
                 Q.Corners (3).U, Q.UD, X, Y, Layout.Width);
               SY : constant Natural := Sample_Axis (Q.Corners (0).V, Q.Corners (1).V,
                 Q.Corners (3).V, Q.VD, X, Y, Layout.Height);
               Alpha : constant Integer_64 := Integer_64
                 (Original (Command.Mask) (SY * Layout.Pitch + SX)) * Channel (Command.Tint, 3);
               Index : constant Natural := Y * 84 + X;
            begin
               for K in 0 .. 3 loop
                  Oracle (Index) (K) := ((if K = 3 then 255 else Channel (Command.Tint, K)) *
                    Alpha * 65536 + Oracle (Index) (K) * (65025 - Alpha) + 32512) / 65025;
               end loop;
            end;
         end loop;
      end loop;
   end Accumulate;
begin
   V.Initialize (Views, True);
   V.Ensure (Views, 0, (Target'Address, 80, 72, 84 * 4, 1), Target'Length * 4, OK);
   if not OK then return False; end if;
   V.Ensure (Views, 1, (Serial'Address, 80, 72, 84 * 4, 1), Serial'Length * 4, OK);
   if not OK then return False; end if;
   for I in 0 .. 2 loop
      declare Layout : constant L.Layout := L.Plan (Scales (I)); begin
         C.Reserve (Cache, (I mod 2, 65 + I, Scales (I)), Tickets (I));
         if Tickets (I) = C.No_Token then return False; end if;
         M.Reserve (Masks, Layout.Bytes, Backing (I), Addresses (I), Capacity (I));
         if Backing (I) = M.Arena.No_Token or Capacity (I) > 2048 then return False; end if;
         declare Mask : Bytes (0 .. Capacity (I) - 1) with Address => Addresses (I); begin
            Mask := (others => 16#A5#);
            Compositor_Glyph_FFI.Rasterize (Unsigned_32 (I mod 2), Unsigned_32 (65 + I),
               Layout, Mask'Address, Mask'Length, Advance, OK);
            if not OK then return False; end if;
            Original (I) (Mask'Range) := Mask;
         end;
         Mesa_Masks.Ensure (Views, V.Mask_Slot'First + V.Slot (I), Addresses (I), Layout,
                           Unsigned_64 (Capacity (I)), OK);
         if not OK then return False; end if;
         C.Publish (Cache, Tickets (I), True);
      end;
   end loop;
   for Rotation in 0 .. 3 loop
      for Length in B.Count loop
         Target := (others => Sentinel); Serial := Target;
         for P of Oracle loop
            for K in 0 .. 3 loop P (K) := Channel (Sentinel, K) * 65536; end loop;
         end loop;
         Packet.Length := 0;
         for I in 1 .. Length loop
            Cmd := (Mask => (I - 1) mod 3,
              Description => (Integer_64 (I mod 11) - 4, Integer_64 (I mod 13) - 2,
                -- Power-of-two numerator and odd denominator keep these
                -- source dimensions away from exact nearest-texel ties.
                32, 17, 4, Unsigned_32 (if I mod 2 = 0 then 3 else 1), Unsigned_32 (Rotation),
                Unsigned_32 (I mod 3), Unsigned_32 (I mod 5), 76, 66, 1),
              Tint => Tints (I mod 4));
            C.Acquire (Cache, Tickets (Cmd.Mask), Readers (I));
            if Readers (I) = C.No_Lease then return False; end if;
            B.Append (Packet, Cmd, OK); if not OK then return False; end if;
            Accumulate (Cmd);
         end loop;
         if C.Reader_Count (Cache) /= Length then return False; end if;
         Mesa_Masks.Render_Batch (Views, 0, Packet, OK);
         if not OK then return False; end if;
         -- Deliberately withhold completion: no pinned glyph may retire.
         for I in 1 .. Length loop
            C.Complete (Cache, Readers (I), False);
            C.Begin_Retirement (Cache, Tickets (Packet.Items (I).Mask), OK);
            if OK or not C.Active (Cache, Readers (I)) then return False; end if;
            Cmd := Packet.Items (I);
            Mesa_Masks.Render (Views, 1, V.Mask_Slot'First + V.Slot (Cmd.Mask),
              Cmd.Description, 80, 72, Cmd.Tint, OK);
            if not OK then return False; end if;
         end loop;
         for I in Target'Range loop
            for K in 0 .. 3 loop
               declare Expected : constant Integer_64 := (Oracle (I) (K) + 32768) / 65536; begin
                  -- One byte level covers float/UNORM conversion. The serial
                  -- comparison additionally allows accumulated 8-bit rounding.
                  if abs (Channel (Target (I), K) - Expected) > 1 or else
                    abs (Channel (Target (I), K) - Channel (Serial (I), K)) > Integer_64 ((Length + 1) / 2 + 1) or else
                    ((Length = 0 or I mod 84 >= 80) and (Target (I) /= Sentinel or Serial (I) /= Sentinel))
                  then
                     Mismatch (Unsigned_32 (Rotation * 33 + Length), Unsigned_32 (I),
                       Unsigned_32 (Channel (Target (I), K)), Unsigned_32 (Expected));
                     return False;
                  end if;
               end;
            end loop;
         end loop;
         for I in 1 .. Length loop C.Complete (Cache, Readers (I), True); end loop;
         if C.Reader_Count (Cache) /= 0 then return False; end if;
      end loop;
   end loop;
   -- Reject a malformed packet without writing or poisoning the ready context.
   Target := (others => Sentinel);
   Packet.Items (Packet.Length).Description.Clip_W := 81;
   Mesa_Masks.Render_Batch (Views, 0, Packet, OK);
   if OK or not V.Can_Retire (Views) then return False; end if;
   for P of Target loop if P /= Sentinel then return False; end if; end loop;
   Packet.Items (Packet.Length).Description.Clip_W := 76;
   -- A missing final import must reject the entire packet before any draw.
   Packet.Items (Packet.Length).Mask := 127;
   Mesa_Masks.Render_Batch (Views, 0, Packet, OK);
   if OK or not V.Can_Retire (Views) then return False; end if;
   for P of Target loop if P /= Sentinel then return False; end if; end loop;
   V.Shutdown (Views);
   if not V.Can_Retire (Views) or not V.Views_Clear (Views) then return False; end if;
   for I in 0 .. 2 loop
      declare Mask : Bytes (0 .. Capacity (I) - 1) with Address => Addresses (I); begin
         if Mask /= Original (I) (Mask'Range) then return False; end if;
      end;
      C.Begin_Retirement (Cache, Tickets (I), OK); if not OK then return False; end if;
      M.Release (Masks, Backing (I), True, OK); if not OK then return False; end if;
      C.Retired (Cache, Tickets (I), True);
   end loop;
   if C.Charged (Cache) /= 0 then return False; end if;
   Report;
   return True;
end Native_Batch_Test;
