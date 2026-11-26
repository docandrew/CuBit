--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Ladybird's js on CuBit (WEB-006 phase a): runs ladybird-js.app, the
--  unmodified Linux/musl js shell, on check.js, with @nvme:0/ladybird
--  delegated read-only (the script and ICU's data file), and shows each
--  line it writes to its unix.stdout and unix.stderr outlets, through rings
--  lent for the run. js finds ICU's data by LADYBIRD_ICU_DATA, an
--  environment string of its launch block (Ladybird's own name for it).
--  Results go to the kernel console.
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
with Startup;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Streams;
with CuBit.Monotonic;

procedure Main is
   Launcher : constant CuBit.Launching.Launcher := CuBit.Launching.Using (Startup.Process_Manager);
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PP renames CuBit.Program_Descriptions;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Program : constant String := "ladybird-js.app";
   Place   : constant String := "@nvme:0/ladybird";
   Script  : constant String := Place & "/check.js";
   ICU_Data : constant String := "LADYBIRD_ICU_DATA=" & Place & "/icudt78l.dat";
   Pass_Line : constant String := "LADYBIRD-JS: PASS";

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("ladybird-js-check: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  One lent ring per outlet the program declares (unix.stdout,
   --  unix.stderr), in this program's memory; the pages are its own.
   Outlet_Names : constant array (1 .. 2) of String (1 .. 11) := ["unix.stdout", "unix.stderr"];
   Ring_Pages : constant := 4;
   Rings : CuBit.Outlet_Rings.Table;
   Ring_Bases : array (1 .. 2) of Unsigned_64 := [others => 0];
   Ring_References : array (1 .. 2) of CuBit.Memory_Grants.Grant_Reference;
   Ring_Of_Entry : array (1 .. 2) of Positive := [others => 1];
   Saw_Pass : Boolean := False;
   Lines : Natural := 0;

   procedure Lend_Rings is
      Descriptor : PP.Bytes (1 .. PP.Maximum_Descriptor_Bytes);
      Length : PP.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PP.Signature;
      Index : PP.Connector_Index;
      Ok, Found : Boolean;
      Grant : Unsigned_64;
   begin
      Rings.Count := 0;
      CuBit.Launching.Describe (Launcher, Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return;
      end if;
      PP.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return;
      end if;
      for K in Outlet_Names'Range loop
         PP.Find_Connector (S, Outlet_Names (K), Index, Found);
         if Found then
            CuBit.Launching.Lend_Ring
              (Launcher, Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Ring_Bases (K), Grant,
               Ring_References (K), Ok);
            if Ok then
               Rings.Count := Rings.Count + 1;
               Rings.Entries (Rings.Count) := (Outlet => Index, Grant => Grant);
               Ring_Of_Entry (Rings.Count) := K;
            end if;
         end if;
      end loop;
   end Lend_Rings;

   --  Show what the rings hold now; the run goes on (a full ring would
   --  hold up the program writing to it).
   --  A record may hold several lines (one write of js's output): each is
   --  shown on its own.
   Buffer : String (1 .. Ring_Pages * 4096);

   procedure Show_Line (Outlet : String; Text : String) is
   begin
      Lines := Lines + 1;
      debugPrint ("ladybird-js-check: js " & Outlet & ": " & Text & ASCII.LF);
      for First in Text'First .. Text'Last - Pass_Line'Length + 1 loop
         if Text (First .. First + Pass_Line'Length - 1) = Pass_Line then
            Saw_Pass := True;
         end if;
      end loop;
   end Show_Line;

   procedure Drain is
      Read : Natural;
      First : Positive;
   begin
      for E in 1 .. Rings.Count loop
         declare
            K : constant Positive := Ring_Of_Entry (E);
         begin
            loop
               Read := CuBit.Stream_Regions.Read_Owned
                 (Ring_Bases (K), Ring_Pages, Buffer'Address, Buffer'Length);
               exit when Read = 0;
               First := 1;
               for Last in 1 .. Read loop
                  if Buffer (Last) = ASCII.LF or else Last = Read then
                     declare
                        Ends : constant Natural := (if Buffer (Last) = ASCII.LF then Last - 1 else Last);
                     begin
                        if Ends >= First then
                           Show_Line (Outlet_Names (K), Buffer (First .. Ends));
                        end if;
                     end;
                     First := Last + 1;
                  end if;
               end loop;
            end loop;
         end;
      end loop;
   end Drain;

   procedure Return_Rings is
      Revoked : Boolean;
   begin
      Drain;
      for E in 1 .. Rings.Count loop
         CuBit.Memory_Grants.Revoke (Ring_References (Ring_Of_Entry (E)), Revoked);
      end loop;
      Rings.Count := 0;
   end Return_Rings;

   procedure Sleep_Milliseconds (Milliseconds : Natural) is
      Microseconds_Per_Millisecond : constant := 1_000;
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
         CuBit.Monotonic.Microseconds
           + Unsigned_64 (Milliseconds) * Microseconds_Per_Millisecond);
   end Sleep_Milliseconds;

   Not_Exited : constant Integer := -1;
   Poll_Milliseconds : constant := 1;

   --  js with Script as its one argument: its exit code, or Not_Exited.
   function Run return Integer is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Added, Accepted : Boolean;
      Child : CuBit.Launching.Child;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, "js", Added);
      Accepted := Added;
      LA.Add_Argument (Block, Script, Added);
      Accepted := Accepted and Added;
      LA.Add_Environment (Block, ICU_Data, Added);
      Accepted := Accepted and Added;
      LA.Finish (Block, Length, Added);
      Accepted := Accepted and Added;
      LG.Start (Places);
      LG.Add (Places, LG.Read_Right, Place, Added);
      Accepted := Accepted and Added;
      LG.Finish (Places, Region, Region_Length);
      if not Accepted then
         return Not_Exited;
      end if;
      Lend_Rings;
      CuBit.Launching.Launch
        (Launcher, Program, Block.Data (1 .. Length), Region (1 .. Region_Length), Child, Result, Failure,
         Rings);
      if Result /= CuBit.Launching.Launched then
         Rings.Count := 0;
         return Not_Exited;
      end if;
      loop
         CuBit.Launching.Poll_Exit (Child, Has_Ended, Ended);
         exit when Has_Ended;
         Drain;
         Sleep_Milliseconds (Poll_Milliseconds);
      end loop;
      Return_Rings;
      return (if Ended.Kind = CuBit.Child_Exits.Exited then Integer (Ended.Code) else Not_Exited);
   end Run;

   Code : Integer;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Code := Run;
   Say ("js runs check.js and exits with 0", Code = 0);
   Say ("js wrote its lines through the lent rings", Lines > 0);
   Say ("every check in check.js passes", Saw_Pass);
   debugPrint ((if Failed then "LADYBIRD-JS-CHECK: FAIL" else "LADYBIRD-JS-CHECK: PASS") & ASCII.LF);
end Main;
