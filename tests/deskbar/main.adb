--  Hosted tests for the Deskbar's pure units: the notification history and
--  toast, task-button grouping and order, button layout, the volume slider
--  and the panel's date words; Desktop's tooltip targets and their words.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Desktop_Icons;
with Desktop_Notifications;
with Desktop_Task_Groups;
with Desktop_Deskbar_Layout;
with Desktop_Status_Items;
with Desktop_Tooltips;
with Client_Tooltip_Policy;
with Desktop_Window_Placement;

procedure Main is
   package DN renames Desktop_Notifications;
   package TG renames Desktop_Task_Groups;
   package DL renames Desktop_Deskbar_Layout;
   use type DN.Serial, DN.Count, DN.Minute_Of_Day, TG.Group_Count, TG.Window_Count, TG.Window_Slot,
     DL.Weekday;

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Record_Of (Title : String; Level : DN.Urgency := DN.Normal) return DN.Notification is
     (DN.Make (DN.Desktop, 0, "Files", Level, Desktop_Icons.Files,
               (Declared => True, Value => 16#E0A21F#), 1_000, (Known => True, Minute => 16 * 60 + 42),
               Title, "It has no startup table (rebuild it)."));

   procedure Notifications is
      H : DN.History;
      Id, First, Second : DN.Serial;
      Changed : Boolean;
   begin
      Check (DN.Valid (H) and then DN.Length (H) = 0 and then DN.Toast (H) = DN.No_Notification,
             "a history starts empty");
      DN.Post (H, Record_Of ("Files could not start"), 1_000, First);
      Check (First /= DN.No_Notification and then DN.Length (H) = 1, "posted");
      Check (DN.Title_Of (DN.Item (H, 1)) = "Files could not start", "title kept");
      Check (DN.Name_Of (DN.Item (H, 1)) = "Files", "source kept");
      Check (DN.Toast (H) = First and then DN.Toast_Due (H) = 1_000 + DN.Toast_Normal_Ms,
             "a normal post is toasted for Toast_Normal_Ms");
      Check (DN.Unread (H) = 1, "unread");

      DN.Post (H, Record_Of ("Penny could not start"), 2_000, Second);
      Check (Second > First and then DN.Item (H, 1).Id = Second and then DN.Item (H, 2).Id = First,
             "newest first");
      Check (DN.Toast (H) = Second, "the newest post takes the toast");

      DN.Expire_Toast (H, 2_000 + DN.Toast_Normal_Ms - 1, Changed);
      Check (not Changed and then DN.Toast (H) = Second, "toast still showing before its time");
      DN.Expire_Toast (H, 2_000 + DN.Toast_Normal_Ms, Changed);
      Check (Changed and then DN.Toast (H) = DN.No_Notification, "toast ends on time");
      Check (DN.Length (H) = 2, "an ended toast stays in the history");

      DN.Mark_Read (H);
      Check (DN.Unread (H) = 0 and then DN.Length (H) = 2, "opening the panel marks read");

      DN.Dismiss (H, First);
      Check (DN.Length (H) = 1 and then DN.Find (H, First) = 0 and then DN.Item (H, 1).Id = Second,
             "dismiss removes one record");
      DN.Dismiss (H, First);
      Check (DN.Length (H) = 1, "dismissing an absent record changes nothing");

      --  Critical stays until dismissed; quiet still toasts it.
      DN.Set_Quiet (H, True);
      DN.Post (H, Record_Of ("Low", DN.Low), 3_000, Id);
      Check (DN.Toast (H) = DN.No_Notification, "quiet: no toast for a low post");
      Check (DN.Item (H, 1).Id = Id, "quiet: still in the history");
      DN.Post (H, Record_Of ("Battery critical", DN.Critical), 4_000, Id);
      Check (DN.Toast (H) = Id and then DN.Toast_Due (H) = DN.Never, "critical always toasted, never ends");
      DN.Expire_Toast (H, Unsigned_64'Last - 1, Changed);
      Check (not Changed, "critical toast does not expire");
      DN.Hide_Toast (H);
      Check (DN.Toast (H) = DN.No_Notification and then DN.Find (H, Id) = 1, "hide keeps the record");
      DN.Set_Quiet (H, False);

      --  Dismissing the toasted record ends its toast.
      DN.Post (H, Record_Of ("x"), 5_000, Id);
      DN.Dismiss (H, Id);
      Check (DN.Toast (H) = DN.No_Notification, "dismiss ends that record's toast");

      --  Bounded: past Capacity the oldest goes, and is counted.
      DN.Clear (H);
      Check (DN.Length (H) = 0 and then DN.Unread (H) = 0, "clear all");
      for K in 1 .. DN.Capacity + 3 loop
         DN.Post (H, Record_Of ("n" & K'Image), Unsigned_64 (K), Id);
      end loop;
      Check (DN.Length (H) = DN.Capacity, "bounded history");
      Check (DN.Dropped (H) = 3, "evictions counted");
      Check (DN.Title_Of (DN.Item (H, 1)) = "n" & Integer'(DN.Capacity + 3)'Image, "newest kept");
      Check (DN.Title_Of (DN.Item (H, DN.Capacity)) = "n 4", "oldest kept is the fourth");
      Check (DN.Unread (H) = DN.Capacity, "unread bounded by length");

      --  Long words are cut, never overrun.
      declare
         N : constant DN.Notification :=
           DN.Make (DN.Service, 7, [1 .. 80 => 'n'], DN.Low, Desktop_Icons.Start, (Declared => False),
                    0, (Known => False), [1 .. 200 => 't'], [1 .. 300 => 'd']);
      begin
         Check (N.Source.Length = DN.Maximum_Name and then N.Title_Len = DN.Maximum_Title and then
                N.Detail_Len = DN.Maximum_Detail, "words cut to their bounds");
      end;
   end Notifications;

   function Win (Key : Unsigned_64; Order : Unsigned_64; Minimized, Focused : Boolean := False)
     return TG.Window is
     ((Present => True, Key => TG.Group_Key (Key), Order => Order,
       Minimized => Minimized, Focused => Focused));

   procedure Groups is
      --  A table of eight slots, as Desktop starts with.
      subtype Table is TG.Windows (0 .. 7);
      subtype Table_Model is TG.Model (7, 8);
      W : Table;
      M : Table_Model;
      Found : Boolean;
      Slot : TG.Window_Slot;
   begin
      TG.Build (W, M);
      Check (M.Count = 0, "no windows, no buttons");

      --  Slots in z-order (raised windows move to later slots); Order is
      --  when each opened. Files (key 5) has two windows.
      W (0) := Win (9, 30);                      --  Penny, opened third
      W (2) := Win (5, 40, Minimized => True);   --  Files #2, opened fourth
      W (3) := Win (5, 10, Focused => True);     --  Files #1, opened first
      W (6) := Win (7, 20, Minimized => True);   --  Text, opened second
      TG.Build (W, M);
      Check (TG.Well_Formed (W, M), "well formed");
      Check (M.Count = 3, "three apps, three buttons");
      Check (M.Of_Window (2) = M.Of_Window (3) and then M.Of_Window (2) /= 0, "one app's windows share a button");
      declare
         Files : constant TG.Group_Index := M.Of_Window (3);
         Text  : constant TG.Group_Index := M.Of_Window (6);
         Penny : constant TG.Group_Index := M.Of_Window (0);
      begin
         Check (M.Groups (Files).Members = 2 and then M.Groups (Files).First = 3, "group count and first window");
         Check (M.Groups (Files).Active and then not M.Groups (Files).Minimized, "focused group is active");
         Check (M.Groups (Text).Minimized and then not M.Groups (Text).Active, "all-minimized group is dimmed");
         Check (TG.Position (W, M, Files) = 1 and then TG.Position (W, M, Text) = 2 and then
                TG.Position (W, M, Penny) = 3, "buttons stand in opening order");
         Check (TG.At_Position (W, M, 2) = Text, "button at a place");
         Check (TG.At_Position (W, M, 4) = 0, "no button past the last");
         Check (TG.Rank (W, 3) = 1 and then TG.Rank (W, 2) = 2, "list in opening order");
         TG.Member_At (W, 5, 2, Found, Slot);
         Check (Found and then Slot = 2, "second Files window");
         TG.Member_At (W, 5, 3, Found, Slot);
         Check (not Found, "no third Files window");
      end;

      --  Raising a window (swapping slots) keeps the button order.
      declare
         Raised : Table := W;
      begin
         Raised (0) := W (3);
         Raised (3) := W (0);
         TG.Build (Raised, M);
         Check (TG.Position (Raised, M, M.Of_Window (0)) = 1, "raising never moves a button");
      end;

      --  Every window present, every app distinct: eight buttons, all placed.
      for S in W'Range loop
         W (S) := Win (Unsigned_64 (100 + Integer (S)), Unsigned_64 (8 - Integer (S)));
      end loop;
      TG.Build (W, M);
      Check (M.Count = 8, "eight buttons");
      declare
         Seen : array (TG.Group_Index range 1 .. 8) of Boolean := [others => False];
      begin
         for G in 1 .. M.Count loop
            Seen (TG.Position (W, M, G)) := True;
         end loop;
         Check ((for all P in Seen'Range => Seen (P)), "positions are a permutation");
      end;

      --  The table grew (Desktop doubles it): the same windows in a larger
      --  table keep their buttons, and the new slots hold more apps.
      declare
         subtype Grown is TG.Windows (0 .. 15);
         subtype Grown_Model is TG.Model (15, 16);
         G2 : Grown;
         M2 : Grown_Model;
      begin
         G2 (0 .. 7) := W;
         for S in TG.Window_Slot range 8 .. 15 loop
            G2 (S) := Win (Unsigned_64 (100 + Integer (S)), Unsigned_64 (100 + Integer (S)));
         end loop;
         G2 (15) := Win (100, 200);   --  a second window of the app in slot 0
         TG.Build (G2, M2);
         Check (TG.Well_Formed (G2, M2) and then M2.Count = 15, "a grown table: fifteen apps");
         Check (TG.Position (G2, M2, M2.Of_Window (7)) = 1 and then
                TG.Position (G2, M2, M2.Of_Window (14)) = 15, "older buttons keep their places");
         Check (M2.Of_Window (15) = M2.Of_Window (0) and then M2.Groups (M2.Of_Window (0)).Members = 2,
                "a window in a new slot joins its app's button");
      end;
   end Groups;

   procedure Layout is
      MIN : constant := 64;
      GAP : constant := 4;
      --  Labels of different lengths: each button as wide as its content.
      Wide : constant DL.Width_List := [1 => 200, 2 => 110, 3 => 150, 4 => 90];
      function Inside_And_Ordered (R : DL.Span_List; Left, Right : Natural) return Boolean is
        ((for all P in R'Range => (if R (P).W /= 0 then R (P).X >= Left and then R (P).X + R (P).W <= Right))
         and then (for all P in R'First .. R'Last - 1 =>
                     (if R (P).W /= 0 and then R (P + 1).W /= 0 then R (P).X + R (P).W + GAP <= R (P + 1).X)));
      R : DL.Span_List (1 .. 4);
   begin
      R := DL.Spans (100, 900, Wide, MIN, GAP, 1);
      Check (R (1).X = 100 and then R (1).W = 200 and then R (2).X = 100 + 200 + GAP and then R (2).W = 110
             and then R (3).W = 150 and then R (4).W = 90, "each button its own content width when they fit");
      --  Too little room: inactive buttons shrink in proportion, the
      --  active one (place 1) keeps its width.
      R := DL.Spans (100, 520, Wide, MIN, GAP, 1);
      Check (R (1).W = 200, "the active button stays readable");
      Check ((for all P in 2 .. 4 => R (P).W >= MIN and then R (P).W < Wide (P)),
             "the others shrink, not below the minimum");
      Check (R (3).W >= R (2).W and then R (2).W >= R (4).W, "in proportion: wider labels stay wider");
      Check (R (4).X + R (4).W <= 520 and then Inside_And_Ordered (R, 100, 520), "all four fit, in order");
      --  Still less: the others at the minimum, then the active one gives.
      R := DL.Spans (100, 400, Wide, MIN, GAP, 1);
      Check ((for all P in 2 .. 4 => R (P).W = MIN) and then R (1).W < 200 and then R (1).W >= MIN
             and then R (4).X + R (4).W <= 400, "others at the minimum, then the active one shrinks");
      --  Many windows: past everything at the minimum, the tail is hidden.
      declare
         Many : constant DL.Span_List := DL.Spans (100, 300, [1 .. 8 => 168], MIN, GAP, 0);
      begin
         Check (Many (8).W = 0 and then Many (1).W = MIN and then Inside_And_Ordered (Many, 100, 300),
                "a button past the space is not shown; the rest at the minimum, inside");
      end;
      --  Far more buttons than eight (the surface table grows): all at
      --  the minimum while they fit, inside and in order.
      declare
         Crowd : constant DL.Span_List := DL.Spans (0, 2_000, [1 .. 24 => 300], MIN, GAP, 3);
      begin
         Check ((for all P in Crowd'Range => Crowd (P).W >= MIN) and then Crowd (3).W >= Crowd (1).W
                and then Inside_And_Ordered (Crowd, 0, 2_000), "twenty-four buttons share the bar");
      end;
      --  No active button: everyone shrinks.
      R := DL.Spans (0, 400, Wide, MIN, GAP, 0);
      Check (R (1).W < 200 and then R (4).X + R (4).W <= 400, "no active button: all shrink");

      Check (DL.Level_At (100, 200, 50) = 0 and then DL.Level_At (100, 200, 400) = 100 and then
             DL.Level_At (100, 200, 200) = 50, "slider level");
      Check (DL.Offset_Of (200, 100) = 200 and then DL.Offset_Of (200, 0) = 0, "slider thumb");

      Check (DL.Day_Of_Week (2026, 10, 18) = DL.Sunday, "18 October 2026 is a Sunday");
      Check (DL.Day_Of_Week (2000, 2, 29) = DL.Tuesday, "29 February 2000 is a Tuesday");
      Check (DL.Day_Of_Week (1970, 1, 1) = DL.Thursday, "1 January 1970 is a Thursday");
      Check (DL.Weekday_Name (DL.Sunday) & " 18 " & DL.Month_Name (10) = "Sunday 18 October", "date words");
   end Layout;

   procedure Tooltips is
      package DT renames Desktop_Tooltips;
      package DSI renames Desktop_Status_Items;
      use type DT.Target;
      type Seen_Set is array (0 .. DT.BUTTON_TARGET) of Boolean;
      type Seen_Access is access Seen_Set;
      Seen : constant Seen_Access := new Seen_Set'(others => False);
      Distinct : Boolean := True;
      procedure Note (T : DT.Target) is
         V : constant Natural := DT.Encode (T);
      begin
         if V <= Seen'Last then
            Distinct := Distinct and then not Seen (V);
            Seen (V) := True;
         end if;
         Check (DT.Decode (V) = T, "a target decodes to itself");
      end Note;
   begin
      Check (DT.Encode (DT.NO_TARGET) = Client_Tooltip_Policy.NO_TARGET, "nothing is the policy's no target");
      for Slot in DT.Window_Slot loop
         for B in DT.Window_Button loop
            Note ((Kind => DT.Button_Of_Window, Slot => Slot, Button => B));
         end loop;
      end loop;
      for I in DSI.Item loop
         Note ((Kind => DT.Status_Icon, Item => I));
      end loop;
      Note ((Kind => DT.Unread_Count));
      Note ((Kind => DT.Status_Button));
      Check (Distinct, "every target has its own number");
      Check (DT.Decode (Natural'Last) = DT.NO_TARGET, "an unknown number is no target");
      Check (DT.Button_Text (DT.Maximize_Button, False, False) = "Maximize unavailable"
             and then DT.Button_Text (DT.Maximize_Button, True, True) = "Restore"
             and then DT.Button_Text (DT.Maximize_Button, False, True) = "Maximize"
             and then DT.Button_Text (DT.Minimize_Button, False, True) = "Minimize"
             and then DT.Button_Text (DT.Close_Button, True, False) = "Close", "window button words");
      Check (DSI.Name (DSI.Volume, False) = "Volume" and then DSI.Name (DSI.Volume, True) = "Volume (muted)"
             and then DSI.Name (DSI.Network, False) = "Network", "status item names");
   end Tooltips;
   procedure Placement is
      package WP renames Desktop_Window_Placement;
      Work_W : constant := 1920;
      Work_H : constant := 1040;
      Min_W : constant := 120;
      Min_H : constant := 80;
      P : WP.Placement;
   begin
      --  A small request opens at the default share of the work area.
      P := WP.Place (Work_W, Work_H, 908, 556, Min_W, Min_H, 0, WP.At_Least_Share);
      Check (P.Width = Work_W * 2 / 3 and then P.Height = Work_H * 3 / 4
             and then P.X = WP.Cascade_Left and then P.Y = WP.Cascade_Top,
             "default share at the cascade corner:" & P.Width'Image & P.Height'Image);
      --  No preference: the share too.
      P := WP.Place (Work_W, Work_H, 0, 0, Min_W, Min_H, 0, WP.At_Least_Share);
      Check (P.Width = Work_W * 2 / 3 and then P.Height = Work_H * 3 / 4, "no preference: the share");
      --  A larger request is kept.
      P := WP.Place (Work_W, Work_H, 1500, 900, Min_W, Min_H, 0, WP.At_Least_Share);
      Check (P.Width = 1500 and then P.Height = 900, "a larger request is kept");
      --  The next slots step down the diagonal.
      P := WP.Place (Work_W, Work_H, 0, 0, Min_W, Min_H, 2, WP.At_Least_Share);
      Check (P.X = WP.Cascade_Left + 2 * WP.Cascade_Step and then P.Y = WP.Cascade_Top + 2 * WP.Cascade_Step,
             "cascade by slot");
      --  The cascade starts over before a window would leave the work area.
      for Slot in 0 .. 200 loop
         P := WP.Place (Work_W, Work_H, 0, 0, Min_W, Min_H, Slot, WP.At_Least_Share);
         Check (P.X + P.Width <= Work_W and then P.Y + P.Height <= Work_H
                and then P.Width = Work_W * 2 / 3 and then P.Height = Work_H * 3 / 4,
                "slot" & Slot'Image & " stays inside, full size");
      end loop;
      --  A request beyond the work area is cut to fit it.
      P := WP.Place (1024, 728, 5000, 5000, Min_W, Min_H, 0, WP.At_Least_Share);
      Check (P.X + P.Width = 1024 and then P.Y + P.Height = 728, "an oversized request is cut to fit");
      --  At 1024x768 a request larger than the share is unchanged.
      P := WP.Place (1024, 728, 868, 600, Min_W, Min_H, 1, WP.At_Least_Share);
      Check (P.Width = 868 and then P.Height = 600 and then P.X = 98 and then P.Y = 82,
             "small screens keep the request where it fits");
      --  A declared preferred size opens as asked, smaller than the share.
      P := WP.Place (Work_W, Work_H, 360, 520, Min_W, Min_H, 0, WP.As_Requested);
      Check (P.Width = 360 and then P.Height = 520 and then P.X = WP.Cascade_Left,
             "a preferred size is kept:" & P.Width'Image & P.Height'Image);
      --  ... still at least the minimum, and cut to fit the work area.
      P := WP.Place (Work_W, Work_H, 60, 40, Min_W, Min_H, 0, WP.As_Requested);
      Check (P.Width = Min_W and then P.Height = Min_H, "a preferred size below the minimum");
      P := WP.Place (1024, 728, 5000, 5000, Min_W, Min_H, 0, WP.As_Requested);
      Check (P.X + P.Width = 1024 and then P.Y + P.Height = 728, "a preferred size is cut to fit");
      --  Without a request the share stands in.
      P := WP.Place (Work_W, Work_H, 0, 0, Min_W, Min_H, 0, WP.As_Requested);
      Check (P.Width = Work_W * 2 / 3 and then P.Height = Work_H * 3 / 4, "no preferred size: the share");
      --  A tiny work area: the window keeps the minimum where it fits.
      P := WP.Place (100, 60, 0, 0, Min_W, Min_H, 3, WP.At_Least_Share);
      Check (P.X = 0 and then P.Y = 0 and then P.Width = 100 and then P.Height = 60, "tiny work area");
   end Placement;
begin
   Notifications;
   Groups;
   Layout;
   Tooltips;
   Placement;
   if Failures = 0 then
      Put_Line ("DESKBAR: PASS" & Checks'Image & " checks");
   else
      Put_Line ("DESKBAR: FAIL" & Failures'Image & " of" & Checks'Image);
   end if;
end Main;
