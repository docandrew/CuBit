------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Hosted checks of CuBit.Secondary_Stack_Layout: the per-thread secondary
--  stack's region layout and commit plan (run.sh; --prove for gnatprove).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Secondary_Stack_Layout; use CuBit.Secondary_Stack_Layout;

procedure Main is
   --  The region and its guard.
   pragma Compile_Time_Error (Usable_Bytes /= Reservation_Bytes - Page_Bytes,
                              "one guard page is never committed");
   pragma Compile_Time_Error (Reservation_Bytes mod Page_Bytes /= 0,
                              "the reservation is whole pages");

   Checks, Failed : Natural := 0;
   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   --  Commit from First_Commit (Prefix) until Needed is backed, as
   --  System.Secondary_Stack.Grow does; how many commits that took.
   function Commits_To (Prefix : Prefix_Bytes; Needed : Region_Bytes) return Natural is
      Committed : Region_Bytes := First_Commit (Prefix);
      Count : Natural := 1;
      Step : Commit_Bytes;
   begin
      while Committed < Needed loop
         Step := Next_Commit (Committed, Needed);
         if Step = 0 or else Step mod Page_Bytes /= 0
           or else Step > Usable_Bytes - Committed
         then
            return Natural'Last;
         end if;
         Committed := Committed + Step;
         Count := Count + 1;
      end loop;
      return Count;
   end Commits_To;

   --  Commits when the need grows a page at a time (many small results).
   function Grown_In_Steps (Needed : Region_Bytes) return Natural is
      Committed : Region_Bytes := First_Commit (0);
      Count : Natural := 1;
      Want : Region_Bytes := Committed;
   begin
      while Want < Needed loop
         Want := Want + Page_Bytes;
         if Want > Committed then
            Committed := Committed + Next_Commit (Committed, Want);
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Grown_In_Steps;

   Committed : Region_Bytes;
begin
   --  A thread without a prefix (musl, Rust): header first, then data.
   Check (Header_Offset (0) = 0, "no prefix: header at the base");
   Check (Data_Offset (0) = Header_Bytes, "no prefix: data after the header");
   Check (First_Commit (0) = 68 * KiB, "no prefix: 64 KiB of data and the header page-rounded");
   Check (Data_Capacity (0) = Usable_Bytes - Header_Bytes, "no prefix: reach");

   --  A thread area: an 8-byte TLS segment aligned to 8.
   Check (TLS_Block_Bytes (8, 8) = 8, "TLS block: 8 bytes");
   Check (Thread_Pointer_Offset (8, 8) = 64, "thread pointer after the TLS block, 64-aligned");
   Check (Area_Prefix (8, 8) = 128, "prefix: TLS block and control block");
   Check (Header_Offset (Area_Prefix (8, 8)) = 128, "header after the prefix");
   Check (TLS_Block_Bytes (0, 1) = 0 and then Area_Prefix (0, 1) = Control_Block_Bytes,
          "no TLS: the control block alone");
   Check (TLS_Block_Bytes (24, 16) = 32 and then Thread_Pointer_Offset (24, 16) = 64,
          "TLS block rounded to its alignment, as the linker lays it out");
   Check (TLS_Block_Bytes (100, 64) = 128 and then Thread_Pointer_Offset (100, 64) = 128,
          "64-aligned TLS: thread pointer aligned");
   Check (Valid_TLS_Alignment (1) and then Valid_TLS_Alignment (8)
            and then Valid_TLS_Alignment (64),
          "power-of-two alignments up to the control block accepted");
   Check (not Valid_TLS_Alignment (0) and then not Valid_TLS_Alignment (24)
            and then not Valid_TLS_Alignment (128),
          "zero, non-power-of-two and over-large alignments refused");
   Check (Area_Prefix (Maximum_TLS_Bytes, Maximum_TLS_Alignment) <= Maximum_Prefix_Bytes,
          "the largest TLS segment fits the prefix");
   Check (First_Commit (Maximum_Prefix_Bytes) <= Maximum_Commit_Bytes,
          "the largest first commit is one call");

   --  Growth: at least 1 MiB, then doubling, at most 16 MiB a call.
   Check (Next_Commit (64 * KiB, 64 * KiB + 16) = Minimum_Growth_Bytes,
          "a small overflow commits the minimum growth");
   Check (Next_Commit (4 * MiB, 4 * MiB + 1) = 4 * MiB, "doubling");
   Check (Next_Commit (32 * MiB, 32 * MiB + 1) = Maximum_Commit_Bytes, "at most one call's bound");
   Check (Next_Commit (1 * MiB, 9 * MiB) = 8 * MiB, "a large request in one call when it fits");
   Check (Next_Commit (Usable_Bytes - Page_Bytes, Usable_Bytes) = Page_Bytes,
          "never past the guard");
   Check (Commits_To (0, 68 * KiB) = 1, "within the first commit: one call");
   Check (Commits_To (0, 9 * MiB) = 2, "one 9 MiB request: one more commit");
   Check (Grown_In_Steps (9 * MiB) = 6, "9 MiB in small steps: 64 KiB, 1, 2, 4, 8, 17 MiB");
   Check (Commits_To (0, Usable_Bytes) /= Natural'Last
            and then Commits_To (0, Usable_Bytes) <= 24,
          "the whole reach: a bounded number of commits");

   --  Every plan step from every page-aligned start keeps its promises.
   Committed := First_Commit (0);
   while Committed < Usable_Bytes loop
      declare
         Step : constant Commit_Bytes := Next_Commit (Committed, Committed + 1);
      begin
         Check (Step > 0 and then Step mod Page_Bytes = 0
                  and then Step <= Usable_Bytes - Committed,
                "a commit is whole pages within the reach");
         Committed := Committed + Step;
      end;
   end loop;
   Check (Committed = Usable_Bytes, "growth reaches exactly the usable reach");

   Put_Line ("secondary-stack-layout:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
