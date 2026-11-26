--  SPDX-License-Identifier: GPL-3.0-or-later
--  KERN-006 layer 1: the share rules (kernel/src/image_share_policy), on the
--  host. Every flag combination: only R and R+X segments on a page with
--  memory may be shared, never a writable one. Same: equal keys match, and
--  a change to any one field of any segment or to the entry point does
--  not. Pages: each segment its rounded-up pages. Users: join to
--  the limit, then refused; the last leave says so.
pragma Ada_2022;
with Interfaces; use Interfaces;
with Image_Share_Policy; use Image_Share_Policy;
with Ada.Text_IO;
procedure Policy_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   Base : constant Unsigned_64 := 16#40_0000#;
   function Key_Of (Count : Segment_Count) return Key is
      K : Key;
   begin
      K.Entry_Point := Base + 16;
      K.Count := Count;
      for I in 1 .. Count loop
         K.Segments (I) := (Address => Base + Unsigned_64 (I) * 16 * Page_Bytes,
                            Offset => Unsigned_64 (I) * Page_Bytes,
                            File_Bytes => Unsigned_64 (I) * 1000,
                            Memory_Bytes => Unsigned_64 (I) * 5000,
                            Executable => I = 1);
      end loop;
      return K;
   end Key_Of;
   Pages : Page_Count;
   Fits, Joined, Last : Boolean;
   Users : User_Count := 0;
begin
   for Flags in Unsigned_32'(0) .. 7 loop
      Check (Shareable (Flags, Base, 10, 20) = (Flags = Flag_Read or else Flags = (Flag_Read or Flag_Execute)),
             "shareable only read-only flags" & Flags'Image);
      Check (not Shareable (Flags or Flag_Write, Base, 10, 20), "never writable");
   end loop;
   Check (not Shareable (Flag_Read, Base + 1, 10, 20), "on a page");
   Check (not Shareable (Flag_Read, Base, 0, 0), "not empty");
   Check (not Shareable (Flag_Read, Base, 21, 20), "file part within memory part");
   Check (Shareable (Flag_Read, Base, 0, 1), "bss-only read-only segment");

   for Count in Segment_Count loop
      declare
         A : constant Key := Key_Of (Count);
         B : Key;
      begin
         Check (Valid (A) and then Same (A, A), "same as itself");
         B := A; B.Entry_Point := B.Entry_Point + 1;
         Check (not Same (A, B), "entry differs");
         if Count < Segment_Count'Last then
            Check (not Same (A, Key_Of (Count + 1)), "segment count differs");
         end if;
         for I in 1 .. Count loop
            B := A; B.Segments (I).Address := B.Segments (I).Address + Page_Bytes;
            Check (not Same (A, B), "address differs");
            B := A; B.Segments (I).Offset := B.Segments (I).Offset + 1;
            Check (not Same (A, B), "offset differs");
            B := A; B.Segments (I).File_Bytes := B.Segments (I).File_Bytes + 1;
            Check (not Same (A, B), "file size differs");
            B := A; B.Segments (I).Memory_Bytes := B.Segments (I).Memory_Bytes + 1;
            Check (not Same (A, B), "memory size differs");
            B := A; B.Segments (I).Executable := not B.Segments (I).Executable;
            Check (not Same (A, B), "executable differs");
         end loop;
         --  Unused segment slots do not matter.
         B := A;
         if Count < Segment_Count'Last then
            B.Segments (Count + 1).Address := 12345 * Page_Bytes;
            Check (Same (A, B), "unused slots ignored");
         end if;
         Total_Pages (A, Pages, Fits);
         declare
            Want : Unsigned_64 := 0;
         begin
            for I in 1 .. Count loop
               Want := Want + (Unsigned_64 (I) * 5000 + Page_Bytes - 1) / Page_Bytes;
            end loop;
            Check (Fits and then Pages = Want, "pages");
         end;
      end;
   end loop;

   for I in 1 .. User_Count'Last loop
      Join (Users, Joined);
      Check (Joined, "join");
   end loop;
   Join (Users, Joined);
   Check (not Joined and then Users = User_Count'Last, "full");
   while Users > 1 loop
      Leave (Users, Last);
      Check (not Last, "not last");
   end loop;
   Leave (Users, Last);
   Check (Last and then Users = 0, "last");
   Ada.Text_IO.Put_Line ("image share policy: PASS checks=" & Checks'Image);
end Policy_Tests;
