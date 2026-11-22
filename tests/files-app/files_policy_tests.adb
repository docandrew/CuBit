with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Directory_Pages;
with Files_Limits; use Files_Limits;
with Files_Listing; use Files_Listing;
with Files_Order; use Files_Order;
with Files_Filter;
with Files_Viewport;
with Files_Marks;
with Files_Pages;

package body Files_Policy_Tests is
   Total_Checks, Total_Failures : Natural := 0;
   function Checks return Natural is (Total_Checks);
   function Failures return Natural is (Total_Failures);

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Total_Checks := Total_Checks + 1;
      if not Condition then
         Total_Failures := Total_Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   --  A small deterministic generator (xorshift).
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Random (Below : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Natural (Seed mod Unsigned_64 (Below));
   end Random;

   function Bytes (Text : String) return Name_Bytes is
      Result : Name_Bytes (1 .. Text'Length);
   begin
      for K in Result'Range loop
         Result (K) := Character'Pos (Text (Text'First + K - 1));
      end loop;
      return Result;
   end Bytes;

   ALPHABET : constant String := "aAbBzZ_-.019 x";
   function Random_Name return String is
      Length : constant Positive := 1 + Random (12);
      Result : String (1 .. Length);
   begin
      for C of Result loop
         C := ALPHABET (ALPHABET'First + Random (ALPHABET'Length));
      end loop;
      if Result = "." or else Result = ".." then
         return "dot";
      end if;
      return Result;
   end Random_Name;

   type Listing_Access is access Listing;
   type Order_Access is access Order_State;
   type Filter_Access is access Files_Filter.Filter_State;

   --  Reference natural comparison, without the sort prefix.
   function Reference_Names (A, B : String) return Ordering is
      function Lower (C : Character) return Character is
        (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
      function Digit (C : Character) return Boolean is (C in '0' .. '9');
      I : Positive := A'First;
      J : Positive := B'First;
   begin
      loop
         if I > A'Last or else J > B'Last then
            return (if I > A'Last and then J > B'Last then Same elsif I > A'Last then Less else Greater);
         end if;
         if Digit (A (I)) and then Digit (B (J)) then
            while I <= A'Last and then A (I) = '0' loop I := I + 1; end loop;
            while J <= B'Last and then B (J) = '0' loop J := J + 1; end loop;
            declare
               EA : Positive := I;
               EB : Positive := J;
            begin
               while EA <= A'Last and then Digit (A (EA)) loop EA := EA + 1; end loop;
               while EB <= B'Last and then Digit (B (EB)) loop EB := EB + 1; end loop;
               if EA - I /= EB - J then
                  return (if EA - I < EB - J then Less else Greater);
               elsif A (I .. EA - 1) /= B (J .. EB - 1) then
                  return (if A (I .. EA - 1) < B (J .. EB - 1) then Less else Greater);
               end if;
               I := EA;
               J := EB;
            end;
         else
            declare
               X : constant Character := (if Digit (A (I)) then '0' else Lower (A (I)));
               Y : constant Character := (if Digit (B (J)) then '0' else Lower (B (J)));
            begin
               if X /= Y then
                  return (if X < Y then Less else Greater);
               elsif Digit (A (I)) /= Digit (B (J)) then
                  return (if Digit (A (I)) then Less else Greater);
               end if;
               I := I + 1;
               J := J + 1;
            end;
         end if;
      end loop;
   end Reference_Names;

   procedure Test_Names is
      L : constant Listing_Access := new Listing (64, 4096);
      procedure Pair (A, B : String; Expected : Ordering) is
      begin
         Clear (L.all);
         Append (L.all, Bytes (A), (others => <>));
         Append (L.all, Bytes (B), (others => <>));
         Check (Compare_Names (L.all, 1, 2) = Expected, "names " & A & " " & B & " " & Expected'Image);
      end Pair;
   begin
      Pair ("file2", "file10", Less);
      Pair ("file10", "file2", Greater);
      Pair ("File", "file", Same);
      Pair ("_x", "a", Less);
      Pair ("ab", "ab1", Less);
      Pair ("ab-x", "ab1", Less);
      Pair ("ab:x", "ab0", Greater);
      Pair ("x01", "x1", Same);
      Pair ("x001y", "x1z", Less);
      Pair ("abcdefghij2", "abcdefghij10", Less);
      Pair ("a", "b", Less);
      --  The prefix never disagrees with the full comparison.
      for Trial in 1 .. 20_000 loop
         declare
            A : constant String := Random_Name;
            B : constant String := Random_Name;
         begin
            Clear (L.all);
            Append (L.all, Bytes (A), (others => <>));
            Append (L.all, Bytes (B), (others => <>));
            if Compare_Names (L.all, 1, 2) /= Reference_Names (A, B) then
               Check (False, "natural order of '" & A & "' and '" & B & "'");
               exit;
            end if;
         end;
      end loop;
      Check (True, "natural order matches the reference on random names");
   end Test_Names;

   procedure Fill (L : in out Listing; Count : Natural) is
   begin
      for N in 1 .. Count loop
         Append (L, Bytes (Random_Name),
                 (Kind => (if Random (5) = 0 then Directory_Kind else File_Kind),
                  Size => Byte_Size (Random (1000)), Size_Known => True,
                  Modified => Time_Ms (Random (50)), Modified_Known => True, others => <>));
      end loop;
   end Fill;

   --  The published order is sorted and holds each arrived entry once.
   procedure Verify (L : Listing; O : Order_State; Label : String) is
      Seen : array (1 .. L.Count) of Boolean := [others => False];
      Good : Boolean := Count (O) = L.Count;
   begin
      for P in 1 .. Count (O) loop
         declare
            Id : constant Entry_Id := At_Position (O, P);
         begin
            if Id > L.Count or else Seen (Id) then
               Good := False;
            else
               Seen (Id) := True;
            end if;
            if P > 1 and then not Before (L, Published_Rule (O), At_Position (O, P - 1), Id) then
               Good := False;
            end if;
         end;
      end loop;
      Check (Good, Label & ": sorted permutation of" & L.Count'Image & " entries");
   end Verify;

   procedure Settle (L : Listing; O : in out Order_State; Budget : Work_Budget) is
      Used : Work_Budget;
   begin
      for Round in 1 .. 1_000_000 loop
         exit when not Busy (O, L);
         Step (O, L, Budget, True, Used);
         if Used > Budget then
            Check (False, "a step exceeded its budget");
         end if;
      end loop;
   end Settle;

   procedure Test_Order is
      L : constant Listing_Access := new Listing (20_000, 400_000);
      O : constant Order_Access := new Order_State (20_000);
      Used : Work_Budget;
   begin
      Reset (O.all);
      --  Streaming: pages arrive between small slices of work.
      for Batch in 1 .. 100 loop
         Fill (L.all, 14 * (1 + Random (8)));
         Step (O.all, L.all, 97, False, Used);
         Check (Used <= 97, "streaming step within budget");
      end loop;
      L.Complete := True;
      Settle (L.all, O.all, 501);
      Verify (L.all, O.all, "streamed by name");
      for K in Sort_Key loop
         for D in Sort_Direction loop
            Set_Rule (O.all, (K, D));
            Settle (L.all, O.all, 1 + Random (4000));
            Verify (L.all, O.all, "re-sorted " & K'Image & " " & D'Image);
         end loop;
      end loop;
      --  A new rule mid-job, and one entry alone.
      Set_Rule (O.all, (By_Size, Descending));
      Step (O.all, L.all, 1000, True, Used);
      Set_Rule (O.all, (By_Name, Ascending));
      Settle (L.all, O.all, 3000);
      Verify (L.all, O.all, "rule changed mid-sort");
      Clear (L.all);
      Reset (O.all);
      Fill (L.all, 1);
      Settle (L.all, O.all, 10);
      Verify (L.all, O.all, "one entry");
      --  Tracking: the cursor's entry is found in every new order.
      Clear (L.all);
      Reset (O.all);
      Fill (L.all, 5_000);
      Settle (L.all, O.all, 100_000);
      Track (O.all, 1234, Position_Of (O.all, 1234));
      Set_Rule (O.all, (By_Modified, Ascending));
      Settle (L.all, O.all, 777);
      Check (Tracked_Position (O.all) > 0 and then At_Position (O.all, Tracked_Position (O.all)) = 1234,
             "tracked entry followed through a re-sort");
   end Test_Order;

   procedure Test_Filter is
      L : constant Listing_Access := new Listing (20_000, 400_000);
      O : constant Order_Access := new Order_State (20_000);
      F : constant Filter_Access := new Files_Filter.Filter_State (20_000);
      Used : Work_Budget;
      procedure Expect (Label : String) is
         Expected : Natural := 0;
         Good : Boolean := True;
      begin
         for P in 1 .. Count (O.all) loop
            if Files_Filter.Matches (L.all, At_Position (O.all, P), F.all) then
               Expected := Expected + 1;
               if Expected > Files_Filter.Count (F.all)
                 or else Files_Filter.At_Position (F.all, Expected) /= At_Position (O.all, P)
               then
                  Good := False;
               end if;
            end if;
         end loop;
         Check (Good and then Expected = Files_Filter.Count (F.all) and then Files_Filter.Complete (F.all),
                Label & ": filter equals the reference (" & Expected'Image & " )");
      end Expect;
      procedure Run_Filter (Budget : Work_Budget) is
      begin
         for Round in 1 .. 100_000 loop
            Files_Filter.Step (F.all, L.all, O.all, Budget, Used);
            exit when Used = 0;
         end loop;
      end Run_Filter;
   begin
      Reset (O.all);
      Files_Filter.Reset (F.all);
      Fill (L.all, 10_000);
      L.Complete := True;
      Settle (L.all, O.all, 50_000);
      Files_Filter.Set_Query (F.all, "a");
      --  The first slice already shows a correct prefix.
      Files_Filter.Step (F.all, L.all, O.all, 300, Used);
      declare
         Good : Boolean := Files_Filter.Count (F.all) > 0;
      begin
         for P in 1 .. Files_Filter.Count (F.all) loop
            Good := Good and then Files_Filter.Matches (L.all, Files_Filter.At_Position (F.all, P), F.all);
         end loop;
         Check (Good, "a live filter's first slice shows matches at once");
      end;
      Run_Filter (300);
      Expect ("query a");
      Files_Filter.Set_Query (F.all, "ab");
      Run_Filter (211);
      Expect ("refined to ab");
      Files_Filter.Set_Query (F.all, "B");
      Run_Filter (5000);
      Expect ("widened to B");
      Set_Rule (O.all, (By_Size, Descending));
      Settle (L.all, O.all, 9000);
      Run_Filter (1234);
      Expect ("order changed under the filter");
      Files_Filter.Set_Query (F.all, "");
      Check (not Files_Filter.Active (F.all), "an empty query turns the filter off");
   end Test_Filter;

   procedure Test_Viewport is
      use Files_Viewport;
      V : Viewport;
   begin
      Place (V, 1000, 1);
      Fit (V, 20);
      Move (V, 25);
      Check (V.Cursor = 26 and then V.Top = 6, "moving down scrolls minimally");
      Move (V, -1000);
      Check (V.Cursor = 1 and then V.Top = 0, "moving to the top");
      Scroll (V, 50);
      Check (V.Cursor = 1 and then V.Top = 50, "scrolling leaves the cursor");
      Go (V, 1000);
      Check (V.Top = 980, "the last row at the bottom");
      Move (V, -5);
      Place (V, 500, 300);
      Check (V.Cursor = 300 and then V.Top = 300 - 15, "new rows keep the cursor's place on screen");
      Place (V, 0, 7);
      Check (V.Cursor = 0 and then V.Top = 0, "no rows, no cursor");
   end Test_Viewport;

   procedure Test_Marks is
      type Marks_Access is access Files_Marks.Mark_State;
      M : constant Marks_Access := new Files_Marks.Mark_State (100);
   begin
      Files_Marks.Clear (M.all);
      Files_Marks.Set (M.all, 3, True, 10);
      Files_Marks.Set (M.all, 3, True, 10);
      Files_Marks.Set (M.all, 7, True, 5);
      Check (Files_Marks.Count (M.all) = 2 and then Files_Marks.Bytes (M.all) = 15, "marks count and bytes");
      Files_Marks.Set (M.all, 3, False, 10);
      Check (Files_Marks.Count (M.all) = 1 and then Files_Marks.Bytes (M.all) = 5
             and then not Files_Marks.Marked (M.all, 3), "unmarking");
      Files_Marks.Set (M.all, 101, True, 1);
      Check (Files_Marks.Count (M.all) = 1, "an Id beyond the capacity is ignored");
   end Test_Marks;

   procedure Test_Pages is
      use Files_Pages;
      package V2 renames CuBit.Directory_Pages;
      L : constant Listing_Access := new Listing (100, 10_000);
      Page : Page_Image := [others => 0];
      Position : Cursor_State;
      Result : Page_Result;
      --  A Directory.Page.V2 of Names (separated by '|'), as the service
      --  writes it, into Page.
      procedure Build (Names : String; Ended : Boolean := False; Resume : Unsigned_64 := 1) is
         P : V2.Page;
         W : V2.Writer;
         Start : Positive := Names'First;
      begin
         V2.Start (P, W);
         for K in Names'First .. Names'Last + 1 loop
            if K > Names'Last or else Names (K) = '|' then
               declare
                  Bytes : V2.Name_Bytes := [others => 0];
               begin
                  for N in Start .. K - 1 loop
                     Bytes (N - Start + 1) := Character'Pos (Names (N));
                  end loop;
                  V2.Append (P, W, (Kind => V2.Kind_File, Valid => V2.Valid_Size or V2.Valid_Object,
                                    Size => 42, Object => Unsigned_64 (W.Count + 1), others => <>),
                             Bytes, K - Start);
               end;
               Start := K + 1;
            end if;
         end loop;
         V2.Finish (P, W, Ended, Resume, 0);
         for I in V2.Page_Index loop
            Page (I + 1) := P (I);
         end loop;
      end Build;
   begin
      Clear (L.all);
      Build ("alpha|beta", Resume => 5);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Taken and then L.Count = 2 and then Name (L.all, 2) = "beta"
             and then Facts (L.all, 1).Size = 42 and then Facts (L.all, 1).Size_Known, "a good page");
      Build ("gamma", Resume => 5);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Malformed and then L.Count = 2, "a resume token that does not move is rejected");
      Build ("ok|bad/name", Resume => 9);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Malformed and then L.Count = 2, "a name with '/' rejects the whole page");
      Build ("ok|bad:name", Resume => 9);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Malformed and then L.Count = 2, "a name with ':' rejects the whole page");
      Build ("ok|..", Resume => 9);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Malformed and then L.Count = 2, "a dot name rejects the whole page");
      Build ("x", Resume => 9);
      Page (V2.Version_At + 1) := 7;
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Malformed, "a wrong version is rejected");
      Build ("last", Ended => True, Resume => 9);
      Take_Page (L.all, Page, Position, Result);
      Check (Result = Page_Last and then L.Count = 3, "the END page");
      --  Hostile bytes never raise.
      for Trial in 1 .. 2_000 loop
         for B of Page loop
            B := Unsigned_8 (Random (256));
         end loop;
         if Random (2) = 0 then
            Page (V2.Version_At + 1 .. V2.Version_At + 2) := [V2.Version, 0];
            Page (V2.Header_Bytes_At + 1 .. V2.Header_Bytes_At + 2) := [V2.Header_Bytes, 0];
         end if;
         Take_Page (L.all, Page, Position, Result);
      end loop;
      Check (Valid (L.all), "random pages leave the listing valid");
   end Test_Pages;

   procedure Run is
   begin
      Test_Names;
      Test_Order;
      Test_Filter;
      Test_Viewport;
      Test_Marks;
      Test_Pages;
   end Run;
end Files_Policy_Tests;
