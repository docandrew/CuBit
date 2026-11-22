with Native_Pool_Test;
with Native_Target_Retirement_Test;
with Native_Affine_Test;
with Native_Output_Test;
with Native_Glyph_Test;
with Native_Mask_Test;
with Native_Batch_Test;
with Native_Placement_Test;
with Native_Glyph_Owner_Test;
with Interfaces; use Interfaces;
with System;
with Mesa_FFI; use Mesa_FFI;
with Compositor_Policy;
procedure Native_Main is
   use type System.Address;
   package Policy renames Compositor_Policy;
   use type Policy.State;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Source : aliased Pixels (0 .. 15) with Alignment => 64;
   Target : aliased Pixels (0 .. 32 * 32 - 1) with Alignment => 64;
   Expected : Pixels (Target'Range);
   Ctx, Src, Dst : System.Address;
   SI : aliased Image := (Source'Address, 4, 4, 16, 0);
   TI : aliased Image := (Target'Address, 32, 32, 128, 1);
   D : aliased Draw := (0, 0, 4, 4, 3, 5, 16, 12, 5, 6, 9, 9, 0);
   Status : Word;
   S : Policy.State;
   procedure Report (Result : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
   function Byte (P : Unsigned_32; C : Natural) return Natural is
     (Natural (Shift_Right (P, C * 8) and 255));
   function Blend (A, B : Unsigned_32) return Unsigned_32 is
      R : Unsigned_32 := 0;
   begin
      for C in 0 .. 3 loop
         R := R or Shift_Left (Unsigned_32
           (Byte (A, C) + (Byte (B, C) * (255 - Byte (A, 3)) + 127) / 255), C * 8);
      end loop;
      return R;
   end Blend;
begin
   --  The same physical storage changes between draws without reimporting.
   --  This catches stale sampler caches and hidden snapshot/upload copies.
   for Cycle in 1 .. 3 loop
      Ctx := Create;
      if Ctx = System.Null_Address then Report (1); return; end if;
      Src := Import_Image (Ctx, SI'Access);
      Dst := Import_Image (Ctx, TI'Access);
      if Src = System.Null_Address or Dst = System.Null_Address then Report (2); return; end if;
      S := Policy.Initial (True, True);
      for Frame in 0 .. 63 loop
         Target := (others => 16#FF10_2020#);
         Expected := Target;
         for I in Source'Range loop
            Source (I) := Shift_Left ((if Frame mod 2 = 0 then 255 else 128), 24) or
              Shift_Left (Unsigned_32 (Frame), 16) or
              Shift_Left (Unsigned_32 ((I / 4) * 24), 8) or Unsigned_32 ((I mod 4) * 24);
         end loop;
         D.Over := Word (Frame mod 2);
         --  Noninteger 3:14 / 3:10 scaling avoids exact nearest-texel
         --  boundaries, whose floating-point tie direction is unspecified.
         D.SW := (if Frame mod 3 = 0 then 3 else 4);
         D.SH := (if Frame mod 3 = 0 then 3 else 4);
         D.DW := (if Frame mod 3 = 0 then 14 else 16);
         D.DH := (if Frame mod 3 = 0 then 10 else 12);
         for Y in 6 .. 14 loop
            for X in 5 .. 13 loop
               declare
                  SX : constant Natural := ((2 * (X - 3) + 1) * Natural (D.SW)) / (2 * Natural (D.DW));
                  SY : constant Natural := ((2 * (Y - 5) + 1) * Natural (D.SH)) / (2 * Natural (D.DH));
                  P : constant Unsigned_32 := Source (SY * 4 + SX);
               begin
                  Expected (Y * 32 + X) :=
                    (if D.Over = 0 then P else Blend (P, Target (Y * 32 + X)));
               end;
            end loop;
         end loop;
         Policy.Begin_Draw (S);
         Status := Render (Ctx, Dst, Src, D'Access);
         Policy.Finish_Draw (S, (if Status = 0 then Policy.Rendered
                                elsif Status = 1 then Policy.Rejected
                                elsif Status = 2 then Policy.Failed_Quiescent
                                else Policy.Access_Unknown));
         if S /= Policy.Ready then Report (10 + Status); return; end if;
         for I in Target'Range loop
            for C in 0 .. 3 loop
               if abs (Byte (Target (I), C) - Byte (Expected (I), C)) >
                 (if D.Over = 0 then 0 else 1)
               then
                  Mismatch (Unsigned_32 (Frame), Unsigned_32 (I), Target (I), Expected (I));
                  Report (3); return;
               end if;
            end loop;
         end loop;
      end loop;
      if Release (Ctx, Src) /= 0 or else Release (Ctx, Dst) /= 0 then Report (4); return; end if;
      Destroy (Ctx);
   end loop;
   if not Native_Pool_Test then Report (50); return; end if;
   if not Native_Target_Retirement_Test then Report (51); return; end if;
   if not Native_Affine_Test then Report (52); return; end if;
   if not Native_Output_Test then Report (53); return; end if;
   if not Native_Glyph_Test then Report (54); return; end if;
   if not Native_Mask_Test then Report (55); return; end if;
   if not Native_Mask_Test (True) then Report (56); return; end if;
   if not Native_Batch_Test then Report (57); return; end if;
   if not Native_Placement_Test then Report (58); return; end if;
   if not Native_Glyph_Owner_Test then Report (59); return; end if;
   Report (0);
end Native_Main;
