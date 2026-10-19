------------------------------------------------------------------------------
--  Hosted tests for Grant_Sweeps: the kernel's production unit, driven the
--  way Process.IPC drives it, against a reference model.
--
--  Grants live in an array (as the kernel's grant records do). Each is on
--  its owner's owned list and its grantee's received list, linked through
--  two pairs of fields, so one grant is on two lists at once, as in the
--  kernel. Walks run random visits that unlink the visited grant, unlink
--  others (on the walked list or another), link new grants, and start
--  nested walks, on the same list too. Unlinking calls Pass first, as
--  Process.IPC's unlinkReceived and releaseOwnedSlot do.
--
--  Checked for every walk: each visited grant is on the walked list when
--  visited, was on it when the walk began, and is visited at most once;
--  every grant on the list at the start that never left it is visited.
--  The first scenario is the one the kernel's old snapshot existed for:
--  visiting a grant unlinks its successor.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Grant_Sweeps;

procedure Main is

   Grants    : constant := 48;
   Processes : constant := 3;
   Rounds    : constant := 40_000;
   --  Nesting the test drives (the unit itself has no depth limit).
   Max_Depth : constant := 4;

   type Slot is range 0 .. Grants;
   No_Slot : constant Slot := 0;
   subtype Grant_Slot is Slot range 1 .. Grants;
   type Process_Id is range 1 .. Processes;
   type List_Key is record
      Proc  : Process_Id := Process_Id'First;
      Owned : Boolean := True;
   end record;

   type Links is record
      Prev, Next : Slot := No_Slot;
   end record;
   type Grant_Record is record
      Live           : Boolean := False;
      Owner, Grantee : Process_Id := Process_Id'First;
      Own, Got       : Links;
   end record;
   Table : array (Grant_Slot) of Grant_Record;
   Heads : array (Process_Id, Boolean) of Slot :=
     [others => [others => No_Slot]];

   package Sweeps is new Grant_Sweeps (Slot, No_Slot, List_Key);

   Failed : exception;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         raise Failed;
      end if;
   end Check;

   function Key_Of (S : Grant_Slot; Owned : Boolean) return List_Key is
     ((Proc  => (if Owned then Table (S).Owner else Table (S).Grantee),
       Owned => Owned));

   function Listed (S : Grant_Slot; Key : List_Key) return Boolean is
     (Table (S).Live and then Key_Of (S, Key.Owned) = Key);

   --  Link fields of S on the owned or received list.
   function Field (S : Grant_Slot; Owned : Boolean) return Links is
     (if Owned then Table (S).Own else Table (S).Got);
   procedure Set_Field (S : Grant_Slot; Owned : Boolean; L : Links) is
   begin
      if Owned then Table (S).Own := L; else Table (S).Got := L; end if;
   end Set_Field;

   function First (Key : List_Key) return Slot is
     (Heads (Key.Proc, Key.Owned));
   function Next (Key : List_Key; S : Slot) return Slot is
     (Field (S, Key.Owned).Next);

   --  The walks in progress, innermost last, as the test sees them.
   type Flags is array (Grant_Slot) of Boolean;
   type Walk_Record is record
      Key      : List_Key;
      Original : Flags := [others => False];  -- listed when it began
      Departed : Flags := [others => False];  -- left the list since
      Visited  : Flags := [others => False];
   end record;
   Walks : array (1 .. Max_Depth) of Walk_Record;
   Depth : Natural := 0;

   procedure Link_Front (S : Grant_Slot; Owned : Boolean) is
      Key  : constant List_Key := Key_Of (S, Owned);
      Head : constant Slot := Heads (Key.Proc, Owned);
   begin
      Set_Field (S, Owned, (Prev => No_Slot, Next => Head));
      if Head /= No_Slot then
         Set_Field (Head, Owned, (Prev => S, Next => Field (Head, Owned).Next));
      end if;
      Heads (Key.Proc, Owned) := S;
   end Link_Front;

   --  As Process.IPC unlinks: Pass first, then the list edit.
   procedure Unlink (S : Grant_Slot; Owned : Boolean) is
      Key : constant List_Key := Key_Of (S, Owned);
      L   : constant Links := Field (S, Owned);
   begin
      Sweeps.Pass (Key, S, L.Next);
      if L.Prev /= No_Slot then
         Set_Field (L.Prev, Owned, (Prev => Field (L.Prev, Owned).Prev, Next => L.Next));
      else
         Heads (Key.Proc, Owned) := L.Next;
      end if;
      if L.Next /= No_Slot then
         Set_Field (L.Next, Owned, (Prev => L.Prev, Next => Field (L.Next, Owned).Next));
      end if;
      --  The kernel reuses the owned link for its free list: scribble.
      Set_Field (S, Owned, (Prev => No_Slot, Next => Grant_Slot'Last));
      for W in 1 .. Depth loop
         if Walks (W).Key = Key then
            Walks (W).Departed (S) := True;
         end if;
      end loop;
   end Unlink;

   procedure Create (S : Grant_Slot; Owner, Grantee : Process_Id) is
   begin
      Table (S) := (Live => True, Owner => Owner, Grantee => Grantee,
                    Own => <>, Got => <>);
      Link_Front (S, True);
      Link_Front (S, False);
   end Create;

   procedure Retire (S : Grant_Slot) is
   begin
      Unlink (S, False);
      Unlink (S, True);
      Table (S).Live := False;
   end Retire;

   subtype Choice is Natural range 0 .. 99;
   package Random_Choice is new Ada.Numerics.Discrete_Random (Choice);
   package Random_Slot is new Ada.Numerics.Discrete_Random (Grant_Slot);
   package Random_Process is new Ada.Numerics.Discrete_Random (Process_Id);
   G_Choice  : Random_Choice.Generator;
   G_Slot    : Random_Slot.Generator;
   G_Process : Random_Process.Generator;

   function Random_Key return List_Key is
     ((Proc => Random_Process.Random (G_Process),
       Owned => Random_Choice.Random (G_Choice) < 50));

   Visits, Nested, Removed_Ahead : Natural := 0;

   --  What a visit does: Plan picks it (scripted or random).
   type Action is (Retire_Visited, Retire_Successor, Retire_Other,
                   Create_New, Walk_Same, Walk_Other, Nothing);
   Scripted : Boolean := False;
   Script   : Action := Nothing;

   procedure Start_Walk (Key : List_Key);

   procedure Visit (S : Slot) is
      W : Walk_Record renames Walks (Depth);
      A : Action;
      R : Choice;
   begin
      Visits := Visits + 1;
      Check (S in Grant_Slot, "visited no grant");
      Check (Listed (S, W.Key), "visited a grant not on the walked list");
      Check (W.Original (S), "visited a grant linked during the walk");
      Check (not W.Visited (S), "visited a grant twice");
      W.Visited (S) := True;
      if Scripted then
         A := Script;
      else
         R := Random_Choice.Random (G_Choice);
         A := (case R is
                 when 0 .. 29  => Retire_Visited,
                 when 30 .. 44 => Retire_Successor,
                 when 45 .. 59 => Retire_Other,
                 when 60 .. 74 => Create_New,
                 when 75 .. 79 => Walk_Same,
                 when 80 .. 84 => Walk_Other,
                 when others   => Nothing);
      end if;
      case A is
         when Retire_Visited =>
            Retire (S);
         when Retire_Successor =>
            if Next (W.Key, S) /= No_Slot then
               Removed_Ahead := Removed_Ahead + 1;
               Retire (Next (W.Key, S));
            end if;
         when Retire_Other =>
            declare
               T : constant Grant_Slot := Random_Slot.Random (G_Slot);
            begin
               if Table (T).Live then
                  Retire (T);
               end if;
            end;
         when Create_New =>
            declare
               T : constant Grant_Slot := Random_Slot.Random (G_Slot);
            begin
               if not Table (T).Live then
                  Create (T, Random_Process.Random (G_Process),
                          Random_Process.Random (G_Process));
               end if;
            end;
         when Walk_Same =>
            if Depth < Max_Depth then
               Start_Walk (W.Key);
            end if;
         when Walk_Other =>
            if Depth < Max_Depth then
               Start_Walk (Random_Key);
            end if;
         when Nothing =>
            null;
      end case;
   end Visit;

   procedure Walk is new Sweeps.Walk (First, Next, Visit);

   procedure Start_Walk (Key : List_Key) is
   begin
      if Depth > 0 then
         Nested := Nested + 1;
      end if;
      Depth := Depth + 1;
      Walks (Depth) := (Key => Key, others => <>);
      for S in Grant_Slot loop
         Walks (Depth).Original (S) := Listed (S, Key);
      end loop;
      Walk (Key);
      for S in Grant_Slot loop
         declare
            W : Walk_Record renames Walks (Depth);
         begin
            Check (W.Visited (S) or else not W.Original (S) or else W.Departed (S),
                   "a grant on the list throughout was not visited");
         end;
      end loop;
      Depth := Depth - 1;
   end Start_Walk;

   procedure Fill is
   begin
      for S in Grant_Slot loop
         if not Table (S).Live and then Random_Choice.Random (G_Choice) < 70 then
            Create (S, Random_Process.Random (G_Process),
                    Random_Process.Random (G_Process));
         end if;
      end loop;
   end Fill;

   Seed : constant Integer :=
     (if Ada.Command_Line.Argument_Count > 0
      then Integer'Value (Ada.Command_Line.Argument (1)) else 20261018);
begin
   Random_Choice.Reset (G_Choice, Seed);
   Random_Slot.Reset (G_Slot, Seed + 1);
   Random_Process.Reset (G_Process, Seed + 2);

   --  Scripted: every grant on process 1's owned list; each visit retires
   --  its successor, then (second pass) the visited grant itself.
   for S in Grant_Slot loop
      Create (S, 1, 2);
   end loop;
   Scripted := True;
   Script := Retire_Successor;
   Start_Walk ((Proc => 1, Owned => True));
   Script := Retire_Visited;
   Start_Walk ((Proc => 2, Owned => False));
   Check (First ((Proc => 1, Owned => True)) = No_Slot and then
          First ((Proc => 2, Owned => False)) = No_Slot,
          "teardown walks left grants listed");
   Scripted := False;

   for Round in 1 .. Rounds loop
      Fill;
      Start_Walk (Random_Key);
   end loop;
   Put_Line ("grant-sweeps: PASS" & Natural'Image (Visits) & " visits," &
             Natural'Image (Nested) & " nested walks," &
             Natural'Image (Removed_Ahead) & " successors unlinked ahead of a walk");
exception
   when Failed =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;
