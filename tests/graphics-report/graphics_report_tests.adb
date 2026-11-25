------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted checks of the renderer report (GPU-014): the entry wire's
--  round trip, devmgr's copy (posts replace their subject, a gone poster's
--  entries turn stale, a full report counts what it drops), the reader's
--  pages and refusals, and the Deskbar's three cases. Not a CuBit result.
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Graphics_Report;
with Devmgr_Graphics_Report;

procedure Graphics_Report_Tests is
   package GR renames CuBit.Graphics_Report;
   package DR renames Devmgr_Graphics_Report;
   use type GR.Report_Entry, GR.Indicator, GR.Generation, GR.Path_Kind;
   Failed : Boolean := False;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Put_Line ("GRAPHICS-REPORT: " & (if Ok then "PASS " else "FAIL ") & What);
      Failed := Failed or else not Ok;
   end Check;

   Desktop_Out : constant GR.Report_Entry :=
     (Kind => GR.Output, Id => 0, Subject => GR.To_Name ("output-0"), Path => GR.Software,
      Why => GR.Policy, Detail => GR.Policy_Setting'Pos (GR.Composition_CPU),
      Device => GR.To_Name ("cpu"), Since => 1234, Stale => False);
   Game : constant GR.Report_Entry :=
     (Kind => GR.Program, Id => 77, Subject => GR.To_Name ("com.cubit.neon-blast.longer-than-16"),
      Path => GR.Software, Why => GR.ICD_Init_Failed, Detail => -3,
      Device => GR.To_Name ("neon-soft"), Since => 2**40, Stale => False);
   Intel : constant GR.Report_Entry :=
     (Kind => GR.GPU, Id => 16#0200#, Subject => GR.To_Name ("8086:46d0"), Path => GR.Hardware,
      Why => GR.None, Detail => 0, Device => GR.To_Name ("intel-gpu"), Since => 9, Stale => False);

   procedure Wire_Checks is
      type Entries is array (1 .. 3) of GR.Report_Entry;
      All_Three : constant Entries := [Desktop_Out, Game, Intel];
   begin
      for E of All_Three loop
         Check (GR.Valid (GR.Encode (E)) and then GR.Decode (GR.Encode (E)) = E,
                "round trip " & GR.Image (E.Subject));
      end loop;
      Check (GR.Image (Game.Subject) = "com.cubit.neon-b", "a long name is cut to 16 characters");
      declare
         Bad : GR.Entry_Wire := GR.Encode (Intel);
      begin
         Bad (0) := Bad (0) or 16#30#;   --  Why = 3 with Path = Hardware
         Check (not GR.Valid (Bad), "a hardware path with a reason is refused");
         Bad := GR.Encode (Intel);
         Bad (2) := Bad (2) or 16#80#;   --  an unprintable character
         Check (not GR.Valid (Bad), "an unprintable name is refused");
      end;
      Check (GR.Indicator_Of (Desktop_Out) = GR.Quiet, "Desktop's CPU composition is no warning");
      Check (GR.Indicator_Of (Game) = GR.Amber, "a failed Vulkan start is amber");
      Check (GR.Indicator_Of ((Game with delta Why => GR.Policy,
                                Detail => GR.Policy_Setting'Pos (GR.Render_GPU_Off))) = GR.Dim,
             "a program on software by policy is dim");
      Check (GR.Indicator_Of (Intel) = GR.Quiet, "a hardware path is quiet");
   end Wire_Checks;

   R : DR.Report := DR.Empty;
   Halves : DR.Pending := DR.No_Pending;
   A : DR.Answer;
   Handled : Boolean;
   Desktop_PID : constant DR.Poster_Id := 40;
   Procmgr_PID : constant DR.Poster_Id := 3;

   procedure Send (Item : GR.Report_Entry; Poster : DR.Poster_Id; Badge : Unsigned_64 := GR.Poster_Tag) is
      W : constant GR.Entry_Wire := GR.Encode (Item);
   begin
      DR.Handle (R, Halves, GR.Post_Label, GR.Post_Words, [W (0), W (1), W (2), W (3)], Badge, Poster, False, A, Handled);
      Check (Handled and then A.Reply and then A.Label = GR.Post_Label and then A.Length = 0,
             "a post's first half is acknowledged with an empty reply");
      DR.Handle (R, Halves, GR.Post_End_Label, GR.Post_Words, [W (4), W (5), W (6), W (7)], Badge, Poster,
                 False, A, Handled);
   end Send;

   --  Read entry Index through the wire, as an observer.
   function Read (Index : Positive) return GR.Report_Entry is
      Wire : GR.Entry_Wire := [others => 0];
      Gen : Unsigned_64;
   begin
      DR.Handle (R, Halves, GR.Summary_Label, 1, [5, 0, 0, 0], GR.Observer_Tag, 9, False, A, Handled);
      Gen := A.Values (1);
      for Page in GR.Page_Index loop
         DR.Handle (R, Halves, GR.Entry_Label, 4, [5, Gen, Unsigned_64 (Index), Unsigned_64 (Page)],
                    GR.Observer_Tag, 9, False, A, Handled);
         for O in 0 .. GR.Page_Words - 1 loop
            Wire (GR.Page_Words * Page + O) := A.Values (2 + O);
         end loop;
      end loop;
      return GR.Decode (Wire);
   end Read;

   procedure Store_Checks is
      G0 : GR.Generation;
   begin
      Send (Desktop_Out, Desktop_PID);
      Send (Intel, Procmgr_PID);
      Check (DR.Count (R) = 2, "two subjects, two entries");
      Send ((Desktop_Out with delta Path => GR.Hardware, Why => GR.None, Detail => 0), Desktop_PID);
      Check (DR.Count (R) = 2 and then DR.Item (R, 1).Path = GR.Hardware, "a newer post replaces its subject");
      Check (Read (2) = Intel, "an observer reads an entry back page by page");
      G0 := DR.Generation (R);
      DR.Poster_Gone (R, Desktop_PID);
      Check (DR.Item (R, 1).Stale and then not DR.Item (R, 2).Stale and then DR.Generation (R) /= G0,
             "a gone poster's entries stay, stale; the others do not");
      Check (Read (1).Stale and then Read (1).Path = GR.Hardware, "and the reader sees the last value, stale");
      Send (Game, Desktop_PID, Badge => GR.Observer_Tag);
      Check (DR.Count (R) = 2, "a post on the observer's badge is ignored");
      DR.Handle (R, Halves, GR.Post_End_Label, GR.Post_Words, [1, 2, 3, 4], GR.Poster_Tag, 55, False, A, Handled);
      Check (DR.Count (R) = 2, "an end without its start is ignored");
      DR.Handle (R, Halves, GR.Summary_Label, 1, [5, 0, 0, 0], GR.Poster_Tag, 9, False, A, Handled);
      Check (A.Reply and then A.Label = GR.Refused_Label and then A.Values (1) = GR.Refusal'Enum_Rep (GR.Not_Approved),
             "a summary on the poster's badge is refused Not_Approved");
      DR.Handle (R, Halves, GR.Summary_Label, 1, [5, 0, 0, 0], GR.Observer_Tag, 9, False, A, Handled);
      G0 := GR.Generation (A.Values (1));
      Send (Game, Procmgr_PID);
      DR.Handle (R, Halves, GR.Entry_Label, 4, [5, Unsigned_64 (G0), 1, 0], GR.Observer_Tag, 9, False, A, Handled);
      Check (A.Label = GR.Refused_Label and then A.Values (1) = GR.Refusal'Enum_Rep (GR.Stale_Generation),
             "a page against an old generation is refused Stale_Generation");
      for I in 1 .. DR.Capacity + 5 loop
         Send ((Game with delta Id => GR.Subject_Id (1000 + I)), Procmgr_PID);
      end loop;
      Check (DR.Count (R) = DR.Capacity and then DR.Dropped (R) > 0, "a full report counts what it drops");
      R := DR.Empty;
      Send (Game, Procmgr_PID);
      DR.Handle (R, Halves, GR.Gone_Label, 1, [77, 0, 0, 0], 0, 41, False, A, Handled);
      Check (not DR.Item (R, 1).Stale, "only procmgr says a program is gone");
      DR.Handle (R, Halves, GR.Gone_Label, 1, [77, 0, 0, 0], 0, Procmgr_PID, True, A, Handled);
      Check (DR.Item (R, 1).Stale, "procmgr's Gone turns the program's own entry stale");
   end Store_Checks;
begin
   Wire_Checks;
   Store_Checks;
   Put_Line ("GRAPHICS-REPORT: " & (if Failed then "FAIL" else "PASS"));
   if Failed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Graphics_Report_Tests;
