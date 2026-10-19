------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's task buttons: which windows share one button (one app's
--  windows are grouped) and in what order the buttons stand.
--
--  @description
--  Desktop describes each window slot (Window): whether a window is there,
--  its app (Key: the launcher entry that started it, or its own process
--  when no entry did), when it was opened (Order, a sequence Desktop never
--  reuses), and whether it is minimized or focused. Build groups the windows
--  by Key. Buttons stand in the order their app's first window opened
--  (Position), so raising or focusing a window never moves its button;
--  the windows in one group's list are in the order they opened (Rank).
--
--  Window slots are Desktop's surface table (its z-order changes as windows
--  are raised), so nothing here depends on slot order except as the last
--  tie-break between equal Orders. The table grows with use, so Windows and
--  Model are sized by it (Fits).
--
--  The per-button security-context mark (CTX-001) will be one more field
--  of Window and Group; nothing else here changes for it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Desktop_Surface_Limits;

package Desktop_Task_Groups with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   --  As Desktop's surface table, which grows with use: a Windows value
   --  is the whole table (slots 0 .. its capacity - 1).
   Max_Windows : constant := Desktop_Surface_Limits.Maximum_Surfaces;
   type Window_Slot is range 0 .. Max_Windows - 1;
   type Window_Count is range 0 .. Max_Windows;
   subtype Window_Rank is Window_Count range 1 .. Max_Windows;

   type Group_Key is new Interfaces.Unsigned_64;

   type Window is record
      Present   : Boolean := False;
      Key       : Group_Key := 0;
      Order     : Interfaces.Unsigned_64 := 0;
      Minimized : Boolean := False;
      Focused   : Boolean := False;
   end record;
   type Windows is array (Window_Slot range <>) of Window;

   type Group_Count is range 0 .. Max_Windows;
   subtype Group_Index is Group_Count range 1 .. Max_Windows;

   --  First: the group's earliest window (the button's icon and colour
   --  come from it). Active: a member has focus and is shown. Minimized:
   --  every member is minimized (the button is dimmed).
   type Group is record
      Key       : Group_Key := 0;
      First     : Window_Slot := 0;
      Members   : Window_Count := 0;
      Active    : Boolean := False;
      Minimized : Boolean := True;
   end record;
   type Group_Array is array (Group_Index range <>) of Group;
   type Group_Of_Window is array (Window_Slot range <>) of Group_Count;

   --  A model of a table whose last slot is Last_Slot: room for one group
   --  per window (Groups_Room, the table's capacity).
   type Model (Last_Slot : Window_Slot; Groups_Room : Group_Index) is record
      Groups    : Group_Array (1 .. Groups_Room) := [others => (others => <>)];
      Count     : Group_Count := 0;
      Of_Window : Group_Of_Window (0 .. Last_Slot) := [others => 0];
   end record;

   --  W is a whole table (from slot 0) and a model with these
   --  discriminants is sized for it.
   function Fits (W : Windows; Last_Slot : Window_Slot; Groups_Room : Group_Index) return Boolean is
     (W'First = 0 and then W'Length >= 1 and then W'Last = Last_Slot and then
      Integer (Groups_Room) = W'Length);

   --  A opened before B: by Order, then by slot.
   function Earlier (W : Windows; A, B : Window_Slot) return Boolean is
     (W (A).Order < W (B).Order or else (W (A).Order = W (B).Order and then A < B))
     with Pre => A in W'Range and then B in W'Range;

   --  Every window is in exactly the group of its Key, every group has a
   --  member, its First is one of them and no earlier member exists, and
   --  no two groups share a Key.
   function Well_Formed (W : Windows; M : Model) return Boolean is
     (Fits (W, M.Last_Slot, M.Groups_Room) and then M.Count <= M.Groups_Room
      and then
      (for all S in W'Range =>
         (if W (S).Present then
            M.Of_Window (S) in 1 .. M.Count and then
            M.Groups (M.Of_Window (S)).Key = W (S).Key
          else M.Of_Window (S) = 0))
      and then
      (for all G in 1 .. M.Count =>
         M.Groups (G).Members >= 1 and then
         M.Groups (G).First in W'Range and then
         W (M.Groups (G).First).Present and then
         M.Of_Window (M.Groups (G).First) = G and then
         (for all S in W'Range =>
            (if M.Of_Window (S) = G then not Earlier (W, S, M.Groups (G).First))))
      and then
      (for all G in 1 .. M.Count =>
         (for all H in 1 .. M.Count => (if G /= H then M.Groups (G).Key /= M.Groups (H).Key))));

   procedure Build (W : Windows; M : out Model)
     with Pre  => Fits (W, M.Last_Slot, M.Groups_Room),
          Post => Well_Formed (W, M);

   --  Where group G's button stands, 1 .. M.Count (by its First window).
   function Position (W : Windows; M : Model; G : Group_Index) return Group_Index
     with Pre  => Well_Formed (W, M) and then G <= M.Count,
          Post => Position'Result <= M.Count;

   --  The group whose button stands at P (0 if none).
   function At_Position (W : Windows; M : Model; P : Group_Index) return Group_Count
     with Pre  => Well_Formed (W, M),
          Post => At_Position'Result <= M.Count and then
                  (if At_Position'Result /= 0 then
                     Position (W, M, At_Position'Result) = P);

   --  Window S's place in its group's list (1 = opened first).
   function Rank (W : Windows; S : Window_Slot) return Window_Rank
     with Pre => W'First = 0 and then S in W'Range and then W (S).Present;

   --  The member of group Key at list place R, if any (Found False if none).
   procedure Member_At
     (W : Windows; Key : Group_Key; R : Window_Rank;
      Found : out Boolean; Slot : out Window_Slot)
     with Pre  => W'First = 0 and then W'Length >= 1,
          Post => (if Found then Slot in W'Range and then W (Slot).Present and then W (Slot).Key = Key
                   and then Rank (W, Slot) = R);
end Desktop_Task_Groups;
