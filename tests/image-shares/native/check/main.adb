--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  KERN-006 layer 1, on the kernel: ten copies of one image (an 8 MiB
--  read-only blob, image-share-child.app) are launched together, and the
--  free memory they took is measured while every copy holds its blob.
--  With the read-only segments shared, a copy costs its private pages
--  only, less than its blob; with each copy loaded privately it costs the
--  whole blob and more. Then a twin (image-share-twin.app: the same
--  segments, another fill) runs beside them: the kernel compares bytes,
--  so it must read its own fill, not the copies'. Every copy and the twin
--  check each byte of their blob and exit 0. Results go to the kernel
--  console.
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launching;
with CuBit.Monotonic;
with CuBit.System_Facts;
with Startup;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Copies : constant := 10;
   --  The child's blob (child.c BLOB_BYTES).
   Blob_Bytes : constant := 8 * 2**20;
   Kibibyte : constant := 1_024;
   --  Long enough for the launches to settle and free memory to be
   --  published (System_Facts: at most about 10 ms old); far under the
   --  copies' hold (child.c HOLD_SECONDS, 5 s).
   Settle_Milliseconds : constant := 300;
   Not_Exited : constant Integer := -1;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("image-share: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   procedure Sleep_Milliseconds (Milliseconds : Natural) is
      Microseconds_Per_Millisecond : constant := 1_000;
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
         CuBit.Monotonic.Microseconds
           + Unsigned_64 (Milliseconds) * Microseconds_Per_Millisecond);
   end Sleep_Milliseconds;

   function Image (N : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   type Started is record
      Running : Boolean := False;
      Child : CuBit.Launching.Child;
   end record;

   function Start (Program : String) return Started is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added : Boolean;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      S : Started;
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, Program, Added);
      LA.Finish (Block, Length, Added);
      LG.Start (Places);
      LG.Finish (Places, Region, Region_Length);
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), S.Child, Result,
         Failure);
      S.Running := Result = CuBit.Launching.Launched;
      return S;
   end Start;

   --  Its exit code, once it has ended (Not_Exited if it never ran).
   function Finish (S : Started) return Integer is
      Has_Ended : Boolean := False;
      Ended : CuBit.Child_Exits.Report;
   begin
      if not S.Running then
         return Not_Exited;
      end if;
      loop
         CuBit.Launching.Poll_Exit (S.Child, Has_Ended, Ended);
         exit when Has_Ended;
         Sleep_Milliseconds (1);
      end loop;
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Finish;

   Children : array (1 .. Copies) of Started;
   Twin : Started;
   Before, After, Each : Unsigned_64;
   All_Read : Boolean := True;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Sleep_Milliseconds (Settle_Milliseconds);
   Before := CuBit.System_Facts.Free_Memory_Bytes;
   for C of Children loop
      C := Start ("image-share-child.app");
   end loop;
   Sleep_Milliseconds (Settle_Milliseconds);
   After := CuBit.System_Facts.Free_Memory_Bytes;
   Twin := Start ("image-share-twin.app");
   Say ("ten copies launched", (for all C of Children => C.Running));
   Each := (if Before > After then (Before - After) / Copies else 0);
   debugPrint ("IMAGE-SHARE-COST: copies=10 blob_kib=" & Image (Blob_Bytes / Kibibyte) &
               " each_kib=" & Image (Each / Kibibyte) & " before_kib=" & Image (Before / Kibibyte) &
               " after_kib=" & Image (After / Kibibyte) & ASCII.LF);
   --  A copy that loads its blob privately costs the whole blob and its
   --  private pages besides (10 MiB each before KERN-006); a copy that
   --  shares it costs only its private pages (writable data, stack, heap:
   --  about 4.7 MiB here), less than the blob itself.
   Say ("a copy of an 8 MiB read-only image costs less than the image",
        Before /= 0 and then Each < Blob_Bytes);
   for C of Children loop
      All_Read := All_Read and then Finish (C) = 0;
   end loop;
   Say ("every copy read its own image's bytes", All_Read);
   Say ("the twin (same segments, another fill) read its own bytes", Finish (Twin) = 0);
   debugPrint ((if Failed then "IMAGE-SHARE: FAIL" else "IMAGE-SHARE: PASS") & ASCII.LF);
end Main;
