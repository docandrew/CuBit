--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The unix.stdin inlet (CuBit.Inlet_Rings): lends stdin-check.app a ring
--  for its declared inlet, then writes 3000 lines ("line 0" ..) into it,
--  each only once the reader has freed room (its ring is 4 pages, the
--  lines about 30 KiB), and finishes the stream; stdin-check reads
--  descriptor 0 to its end and checks every line. stdin-none.app declares
--  no inlet and must find descriptor 0 not open. Results go to the kernel
--  console.
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Inlet_Rings;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launching;
with Startup;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Program_Descriptions;
with CuBit.Streams;
with CuBit.Monotonic;

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;
   use type CuBit.Inlet_Rings.Put_Result;

   --  stdin-check's manifest declares 4 pages for unix.stdin.
   Inlet_Pages : constant := 4;
   Lines : constant := 3_000;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("stdin-inlet: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
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

   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   Not_Exited : constant Integer := -1;
   Waits : Natural := 0;

   --  Program, with a lent ring for its unix.stdin inlet when Feed: the
   --  lines are written as the reader makes room, then the stream is
   --  finished. Its exit code, or Not_Exited.
   function Run (Program : String; Feed : Boolean) return Integer is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added : Boolean;
      Child : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean := False;
      Rings : CuBit.Outlet_Rings.Table;
      Base : Unsigned_64 := 0;
      Grant : Unsigned_64;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Revoked, Ok, Found : Boolean;
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Descriptor_Length : PP.Descriptor_Length;
      S : PP.Signature;
      Index : PP.Connector_Index;
      Next : Natural := 0;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
   begin
      Rings.Count := 0;
      if Feed then
         CuBit.Launching.Describe (Launcher, Program, Descriptor, Descriptor_Length, Result, Failure);
         if Result /= CuBit.Launching.Launched or else Descriptor_Length = 0 then
            return Not_Exited;
         end if;
         PP.Decode (Descriptor (1 .. Descriptor_Length), S, Ok);
         if Ok then
            PP.Find_Connector (S, "unix.stdin", Index, Found);
            Ok := Found;
         end if;
         if Ok then
            CuBit.Launching.Lend_Ring
              (Launcher, Inlet_Pages, CuBit.Streams.TYPE_RAW_BYTES, Base, Grant, Reference, Ok);
         end if;
         if not Ok then
            return Not_Exited;
         end if;
         Rings.Count := 1;
         Rings.Entries (1) := (Outlet => Index, Grant => Grant);
      end if;
      LA.Start (Block);
      LA.Add_Argument (Block, Program, Added);
      LA.Finish (Block, Length, Added);
      LG.Start (Places);
      LG.Finish (Places, Region, Region_Length);
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result,
         Failure, Rings);
      if Result /= CuBit.Launching.Launched then
         return Not_Exited;
      end if;
      loop
         --  Write what fits, a line per record.
         while Feed and then Next < Lines loop
            declare
               Text : constant String := "line " & Image (Next) & ASCII.LF;
            begin
               exit when CuBit.Inlet_Rings.Write (Base, Inlet_Pages, Text'Address, Text'Length)
                 /= CuBit.Inlet_Rings.Put;
               Next := Next + 1;
               if Next = Lines then
                  CuBit.Inlet_Rings.Finish (Base);
               end if;
            end;
         end loop;
         CuBit.Launching.Poll_Exit (Child, Has_Ended, Ended);
         exit when Has_Ended;
         Waits := Waits + 1;
         Sleep_Milliseconds (1);
      end loop;
      if Feed then
         CuBit.Memory_Grants.Revoke (Reference, Revoked);
         Say ("all" & Natural'Image (Lines) & " lines written, waiting on the reader" &
              Natural'Image (Waits) & " times", Next = Lines);
      end if;
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Say ("stdin-check reads the inlet's stream and exits with 0", Run ("stdin-check.app", Feed => True) = 0);
   Say ("stdin-none finds no descriptor 0 and exits with 0", Run ("stdin-none.app", Feed => False) = 0);
   debugPrint ((if Failed then "STDIN-INLET: FAIL" else "STDIN-INLET: PASS") & ASCII.LF);
end Main;
