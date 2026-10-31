pragma Ada_2022;
with Ada.Text_IO;
with AML_Definition_Loads;
with AML_Identity;
with AML_Identity.Issuer;
procedure Registry_Session_Tests is
   package D renames AML_Definition_Loads;
   use type D.Load_Phase, D.Definition_Identity, D.Slot_Selection;
   Capacity : constant Positive := 2;
   type Model_Phase is (Unused, Reserved, Complete, Terminal);
   type Token is (Empty, First, Second, Other);
   type Model_Entry is record
      Phase : Model_Phase := Unused;
      ID : Token := Empty;
   end record;
   subtype Slot is Positive range 1 .. Capacity;
   type Model_Array is array (Slot) of Model_Entry;
   Empty_Model : constant Model_Array := [others => <>];
   type Action_Kind is (Clear, Reserve, Consume, Finish, Recover);
   type Action is record
      Kind : Action_Kind;
      Index : Slot := Slot'First;
      ID : Token := Empty;
      Success : Boolean := False;
   end record;
   Actions : constant array (Positive range <>) of Action :=
     [(Clear, 1, Empty, False), (Reserve, 1, Empty, False), (Reserve, 2, Empty, False),
      (Consume, 1, Empty, False), (Consume, 1, First, False), (Consume, 1, Second, False),
      (Consume, 2, Empty, False), (Consume, 2, First, False), (Consume, 2, Second, False),
      (Finish, 1, Empty, False), (Finish, 1, Empty, True),
      (Finish, 2, Empty, False), (Finish, 2, Empty, True),
      (Recover, 1, Empty, False), (Recover, 2, Empty, False)];
   Max_States : constant Positive := 64;
   Max_Path : constant Positive := 8;
   type Action_Path is array (Positive range 1 .. Max_Path) of Action;
   type Search_Entry is record
      Model : Model_Array := Empty_Model;
      Path : Action_Path := [others => (Clear, 1, Empty, False)];
      Length : Natural range 0 .. Max_Path := 0;
   end record;
   Queue : array (Positive range 1 .. Max_States) of Search_Entry;
   Head, Tail : Positive := 1;
   Checks, Transitions : Natural := 0;
   Loads : D.State (Capacity);
   IDs : array (Token) of D.Definition_Identity := [others => D.No_Definition];
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with "registry model disagreement"; end if; end Check;
   function Active_Count (Model : Model_Array) return Natural is
      Count : Natural := 0;
   begin for Item of Model loop if Item.Phase = Reserved then Count := Count + 1; end if; end loop; return Count; end Active_Count;
   function Admitted (Model : Model_Array; Enclosing : D.Slot_Selection; Expected : Token) return Boolean is
   begin
      if Enclosing = D.No_Slot then return Active_Count (Model) = 0; end if;
      if Enclosing > D.Slot_Selection (Capacity) then return False; end if;
      return Active_Count (Model) = 1 and then Expected /= Empty
        and then Model (Positive (Enclosing)).Phase = Reserved
        and then Model (Positive (Enclosing)).ID = Expected;
   end Admitted;
   procedure Check_Model (Model : Model_Array) is
      Selections : constant array (Positive range <>) of D.Slot_Selection :=
        [D.No_Slot, 1, 2, D.Slot_Selection (Capacity + 1), D.Slot_Selection'Last];
   begin
      pragma Assert (D.Unique_Definitions (Loads));
      Check (D.Valid (Loads)); Check (D.Active (Loads) = (Active_Count (Model) > 0));
      for I in Slot loop
         Check (D.Phase (Loads, I) = (case Model (I).Phase is when Unused => D.Retained,
           when Reserved => D.Loading, when Complete => D.Loaded, when Terminal => D.Failed));
         Check (D.Definition (Loads, I) = IDs (Model (I).ID));
      end loop;
      for Enclosing of Selections loop
         for Expected in Token loop
            Check (D.Session_Admitted (Loads, Enclosing, IDs (Expected)) = Admitted (Model, Enclosing, Expected));
         end loop;
      end loop;
   end Check_Model;
   procedure Model_Step (Model : in out Model_Array; Item : Action; Accepted : out Boolean) is
      Present : Boolean := False;
   begin
      Accepted := False;
      case Item.Kind is
         when Clear => Model := Empty_Model; Accepted := True;
         when Reserve =>
            if Model (Item.Index).Phase = Unused then Model (Item.Index).Phase := Reserved; Accepted := True; end if;
         when Consume =>
            for Model_Item of Model loop if Model_Item.ID = Item.ID then Present := True; end if; end loop;
            if Model (Item.Index) = Model_Entry'(Reserved, Empty) and then Item.ID /= Empty and then not Present then
               Model (Item.Index).ID := Item.ID; Accepted := True;
            end if;
         when Finish =>
            if Model (Item.Index).Phase = Reserved then
               Accepted := True;
               if Model (Item.Index).ID = Empty then Model (Item.Index).Phase := Unused;
               elsif Item.Success then Model (Item.Index).Phase := Complete;
               else Model (Item.Index).Phase := Terminal; end if;
            end if;
         when Recover =>
            if Model (Item.Index).Phase = Complete then Model (Item.Index).Phase := Terminal; Accepted := True; end if;
      end case;
   end Model_Step;
   procedure Step (Model : in out Model_Array; Item : Action) is
      Expected, Actual : Boolean;
   begin
      Model_Step (Model, Item, Expected);
      case Item.Kind is
         when Clear => D.Reset (Loads); Actual := True;
         when Reserve => D.Reserve (Loads, Item.Index, Actual);
         when Consume => D.Consume (Loads, Item.Index, IDs (Item.ID), Actual);
         when Finish => D.Finish (Loads, Item.Index, Item.Success, Actual);
         when Recover => D.Require_Recovery (Loads, Item.Index, Actual);
      end case;
      Check (Actual = Expected); Check_Model (Model);
   end Step;
begin
   for ID in First .. Other loop
      declare Value : AML_Identity.Identity; Success : Boolean; begin
         AML_Identity.Issuer.Issue (Value, Success); Check (Success); IDs (ID) := D.Definition_Identity (Value);
      end;
   end loop;
   Check_Model (Empty_Model);
   while Head <= Tail loop
      for Item of Actions loop
         declare Model : Model_Array := Empty_Model; Known : Boolean := False; begin
            D.Reset (Loads); Check_Model (Model);
            for I in 1 .. Queue (Head).Length loop Step (Model, Queue (Head).Path (I)); end loop;
            Check (Model = Queue (Head).Model); Step (Model, Item); Transitions := Transitions + 1;
            for I in 1 .. Tail loop if Queue (I).Model = Model then Known := True; exit; end if; end loop;
            if not Known then
               Check (Tail < Max_States and then Queue (Head).Length < Max_Path);
               Tail := Tail + 1; Queue (Tail) := Queue (Head); Queue (Tail).Model := Model;
               Queue (Tail).Length := Queue (Tail).Length + 1; Queue (Tail).Path (Queue (Tail).Length) := Item;
            end if;
         end;
      end loop;
      Head := Head + 1;
   end loop;
   Ada.Text_IO.Put_Line ("Registry session checks" & Checks'Image & " states" & Tail'Image & " transitions" & Transitions'Image);
end Registry_Session_Tests;
