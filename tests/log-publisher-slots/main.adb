--  tests/log-publisher-slots: which entry logstore gives a new publishing
--  channel (Publisher_Slots, the code Publisher_Rings.Open runs). Before
--  the fix, a full table let the longest-idle LIVE channel go for the new
--  publisher, which then kept writing into a ring nobody read (lost
--  records); the properties below are what that broke.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Publisher_Slots;

procedure Main is
   Size : constant := 8;
   type Entry_View is record
      Live : Boolean := False;
      Owner, Authority : Unsigned_64 := 0;
   end record;
   Table : array (1 .. Size) of Entry_View;
   Used : Natural := 0;
   Checks, Failures : Natural := 0;

   function Count return Natural is (Used);
   function Live (I : Positive) return Boolean is (Table (I).Live);
   function Owner (I : Positive) return Unsigned_64 is (Table (I).Owner);
   function Authority (I : Positive) return Unsigned_64 is (Table (I).Authority);
   package Slots is new Publisher_Slots (Count, Live, Owner, Authority);
   use type Slots.Choice_Kind;

   procedure Check (OK : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not OK then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL " & What);
      end if;
   end Check;

   --  Never another publisher's live channel, whatever the table holds.
   procedure Never_Takes_Another (From, Auth : Unsigned_64; May_Grow : Boolean) is
      Pick : constant Slots.Choice := Slots.Choose (From, Auth, May_Grow);
   begin
      if Pick.Kind in Slots.Same_Publisher | Slots.Free_Entry then
         Check (not Table (Pick.Index).Live
                or else (Table (Pick.Index).Owner = From and then Table (Pick.Index).Authority = Auth),
                "never another publisher's live channel");
      end if;
   end Never_Takes_Another;

   Pick : Slots.Choice;
begin
   --  Empty: grow (or no room).
   Check (Slots.Choose (7, 1, True).Kind = Slots.Grow, "empty table grows");
   Check (Slots.Choose (7, 1, False).Kind = Slots.No_Room, "empty table at its ceiling: no room");
   --  Four live publishers, one free entry between.
   Used := 5;
   Table (1 .. 5) := [(True, 10, 1), (True, 11, 1), (False, 0, 0), (True, 12, 1), (True, 13, 2)];
   Pick := Slots.Choose (12, 1, True);
   Check (Pick.Kind = Slots.Same_Publisher and then Pick.Index = 4, "the same publisher's channel");
   Pick := Slots.Choose (13, 1, True);
   Check (Pick.Kind = Slots.Free_Entry and then Pick.Index = 3, "same process, other authority: a free entry");
   Pick := Slots.Choose (99, 1, False);
   Check (Pick.Kind = Slots.Free_Entry and then Pick.Index = 3, "a new publisher: the free entry");
   --  Full: grow, or refuse; never a live entry.
   Table (3) := (True, 14, 1);
   Check (Slots.Choose (99, 1, True).Kind = Slots.Grow, "full table grows");
   Check (Slots.Choose (99, 1, False).Kind = Slots.No_Room, "full table at its ceiling: no room, nothing let go");
   Pick := Slots.Choose (11, 1, False);
   Check (Pick.Kind = Slots.Same_Publisher and then Pick.Index = 2, "full table: the same publisher still reopens");
   --  Every table of up to Size entries, owners 1 .. 3, all free/live mixes.
   for N in 0 .. Size loop
      Used := N;
      for Mask in 0 .. 2 ** N - 1 loop
         for I in 1 .. N loop
            Table (I) := (Live => (Mask / 2 ** (I - 1)) mod 2 = 1,
                          Owner => Unsigned_64 (I mod 3 + 1), Authority => 1);
         end loop;
         for From in Unsigned_64 range 1 .. 4 loop
            Never_Takes_Another (From, 1, True);
            Never_Takes_Another (From, 1, False);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("log-publisher-slots:" & Natural'Image (Checks) & " checks," &
                         Natural'Image (Failures) & " failures");
   if Failures > 0 then
      raise Program_Error;
   end if;
end Main;
