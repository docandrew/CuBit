with Ada.Command_Line;
with Ada.Text_IO;
with Boot_Diagnostics;
with Boot_Output;
with Boot_Framebuffer;
with Interfaces; use Interfaces;
with Spinlocks;
with System.Storage_Elements; use System.Storage_Elements;
procedure Main is
   use type Boot_Framebuffer.Status;
   use type Boot_Framebuffer.Address;
   W : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   H : constant Positive := Positive'Value (Ada.Command_Line.Argument (2));
   Stride : constant Positive := W + 8;
   type Pixels is array (Natural range <>) of aliased Unsigned_32;
   type Pixel_Buffer is access Pixels;
   Sentinel : constant Unsigned_32 := 16#BADCAFE#;
   -- Large test framebuffers live on the Linux heap, not the test stack.
   Storage : constant Pixel_Buffer := new Pixels'(0 .. Stride * H + 1 => Sentinel);
   Saved : constant Pixel_Buffer := new Pixels (Storage'Range);
   Memory : Pixels renames Storage.all;
   Snapshot : Pixels renames Saved.all;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory (1)'Address));
   Decoded : constant Boot_Framebuffer.Result := Boot_Framebuffer.Decode
     ((Base, Unsigned_32 (W), Unsigned_32 (H), Unsigned_32 (Stride * 4),
       1, 32, 16, 8, 8, 8, 0, 8), 2 ** 48, 128 * 1024 * 1024);
   Message : aliased constant String := "test failure" & ASCII.NUL;
   Second : aliased constant String := "must not replace first failure" & ASCII.NUL;
   procedure Check_Guards is
   begin
      pragma Assert (Memory (0) = Sentinel and Memory (Memory'Last) = Sentinel);
      for Y in 0 .. H - 1 loop
         for X in W .. Stride - 1 loop
            pragma Assert (Memory (1 + Y * Stride + X) = Sentinel);
         end loop;
      end loop;
   end;
begin
   pragma Assert (Decoded.State = Boot_Framebuffer.Success);
   if Ada.Command_Line.Argument_Count = 3 then
      Boot_Output.Retire;
      Snapshot := Memory;
      Boot_Diagnostics.Setup (Decoded.Value);
      Boot_Output.Panic (Message'Address);
      pragma Assert (Memory = Snapshot);
      Ada.Text_IO.Put_Line ("PASS retirement before renderer setup");
      return;
   end if;
   Boot_Diagnostics.Setup (Decoded.Value);
   Check_Guards;
   Snapshot := Memory;
   Boot_Diagnostics.Begin_Step ("Testing bounded framebuffer writes");
   -- Stage updates may only change the current-step glyph row.
   declare
      Scale : constant Positive := (if W >= 1024 and H >= 600 then 2 else 1);
      Top : constant Natural := 16 + 3 * 16 * Scale;
   begin
      for Y in 0 .. H - 1 loop
         if Y < Top or Y >= Top + 13 * Scale then
            for X in 0 .. Stride - 1 loop
               pragma Assert (Memory (1 + Y * Stride + X) = Snapshot (1 + Y * Stride + X));
            end loop;
         end if;
      end loop;
   end;
   Boot_Diagnostics.Complete_Step ("Completed");
   for I in 1 .. 1000 loop Boot_Diagnostics.Append ('x'); end loop;
   Boot_Diagnostics.Append (ASCII.LF);
   Check_Guards;
   Snapshot := Memory;
   Spinlocks.Busy := True;
   Boot_Diagnostics.Panic (Message'Address);
   Boot_Diagnostics.Append (ASCII.LF);
   Boot_Diagnostics.Begin_Step ("busy");
   pragma Assert (Memory = Snapshot);
   Spinlocks.Busy := False;
   Boot_Diagnostics.Panic (Message'Address);
   Snapshot := Memory;
   Boot_Diagnostics.Panic (Second'Address);
   pragma Assert (Memory = Snapshot);
   Boot_Output.Retire;
   Boot_Output.Retire;
   Boot_Diagnostics.Setup (Decoded.Value);
   Boot_Diagnostics.Begin_Step ("must not revive");
   Boot_Diagnostics.Complete_Step ("must not revive");
   Boot_Diagnostics.Panic (Second'Address);
   Boot_Diagnostics.Append ('x');
   Boot_Diagnostics.Append (ASCII.LF);
   Boot_Output.Append (ASCII.LF);
   Boot_Output.Panic (Second'Address);
   pragma Assert (Memory = Snapshot);
   Check_Guards;
   Ada.Text_IO.Put_Line ("PASS hosted renderer bounds, row damage, busy panic and retirement" & W'Image & " x" & H'Image);
end Main;
