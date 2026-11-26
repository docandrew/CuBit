------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  task-card-check (UI-061): the hover card's headless test app. It opens
--  two windows and titles the first "Files", then renames it "Calculator"
--  at run time: both attempts to pass for another app. Desktop must keep
--  its task button "Task Card Check" (its manifest's signed name) and show
--  the titles only in the title bars and the hover card. The second window
--  keeps changing (a moving bar), so the card's thumbnail is live.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.Monotonic;

procedure Main is
   package DP renames CuBit.Desktop_Protocol;
   First, Second : CuBit.UI.App.Window;
   Flags : constant Unsigned_64 := DP.Feature_Bits
     ([DP.Decorated | DP.Closeable | DP.Minimizable => True, others => False]);
   --  Sizes the windows are designed for (a compact test window each).
   First_W : constant := 480;
   First_H : constant := 300;
   Second_W : constant := 360;
   Second_H : constant := 240;
   --  The run-time rename comes this long after both windows show.
   Rename_After_Ms : constant := 1_500;
   --  The second window's bar moves this often.
   Frame_Ms : constant := 50;
   Bar_W : constant := 32;
   Step : Natural := 0;
   Renamed : Boolean := False;
   Started : Unsigned_64;

   procedure Paint (Win : in out CuBit.UI.App.Window; Stripes : Boolean) is
      Repair : Rect;
      Ready : Boolean;
      Colors : constant Theme := Current_Theme;
   begin
      CuBit.UI.App.Begin_Paint (Win, CuBit.UI.App.Full_Rect (Win), Repair, Ready);
      if not Ready then return; end if;
      declare
         C : constant Canvas := CuBit.UI.App.Canvas (Win);
         Full : constant Rect := CuBit.UI.App.Full_Rect (Win);
      begin
         Fill_Rect (C, Full, Colors.panel);
         if Stripes then
            --  Bold diagonal-ish bands: recognisable at thumbnail size.
            for I in 0 .. Full.h / 24 loop
               Fill_Rect (C, (0, I * 24, Full.w, 12), (if I mod 2 = 0 then Colors.accent else Colors.good));
            end loop;
         else
            Fill_Rect (C, ((Step * 8) mod (Full.w - Bar_W), 0, Bar_W, Full.h), Colors.danger);
         end if;
         CuBit.UI.App.Present (Win, Full);
      end;
   end Paint;

   procedure Open (Win : in out CuBit.UI.App.Window; W, H : Natural; Title : String; OK : out Boolean) is
      Outcome : CuBit.UI.App.Open_Result;
   begin
      CuBit.UI.App.Open (Win, W, H, Flags, Outcome, title => Title, preferred_size => True);
      OK := Outcome in CuBit.UI.App.Opened;
   end Open;

   Opened_First, Opened_Second, Found : Boolean;
   Event : CuBit.UI.App.Input_Event;
begin
   Open (First, First_W, First_H, "Files", Opened_First);
   Open (Second, Second_W, Second_H, "Second window", Opened_Second);
   if not (Opened_First and Opened_Second) then
      debugPrint ("task-card-check: FAIL windows did not open" & ASCII.LF);
      return;
   end if;
   Paint (First, Stripes => True);
   Paint (Second, Stripes => False);
   debugPrint ("task-card-check: two windows open" & ASCII.LF);
   Started := CuBit.Monotonic.Milliseconds;
   loop
      CuBit.UI.App.Wait_Input_Until (First, CuBit.Monotonic.Milliseconds + Frame_Ms, Event, Found);
      loop
         CuBit.UI.App.Poll_Input (Second, Event, Found);
         exit when not Found;
      end loop;
      if not Renamed and then CuBit.Monotonic.Milliseconds >= Started + Rename_After_Ms then
         CuBit.UI.App.Set_Title (First, "Calculator");
         Renamed := True;
         debugPrint ("task-card-check: renamed its window Calculator" & ASCII.LF);
      end if;
      Step := Step + 1;
      Paint (Second, Stripes => False);
      exit when not CuBit.UI.App.Is_Open (First) and then not CuBit.UI.App.Is_Open (Second);
   end loop;
end Main;
