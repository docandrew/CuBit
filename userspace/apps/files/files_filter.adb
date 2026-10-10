package body Files_Filter with SPARK_Mode is
   use Files_Listing;

   function Query (F : Filter_State) return String is
      Result : String (1 .. F.Length);
   begin
      for K in Result'Range loop
         Result (K) := Character'Val (F.Text (K));
      end loop;
      return Result;
   end Query;

   function Matches (L : Listing; Id : Entry_Id; F : Filter_State) return Boolean is
      N : constant Name_Length := Length (L, Id);
      Q : constant Query_Length := F.Length;
   begin
      if Q = 0 then
         return True;
      elsif Q > N then
         return False;
      end if;
      for Start in 1 .. N - Q + 1 loop
         declare
            Found : Boolean := True;
         begin
            for K in 1 .. Q loop
               if Fold (Byte_At (L, Id, Start + K - 1)) /= F.Text (K) then
                  Found := False;
                  exit;
               end if;
            end loop;
            if Found then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Matches;

   procedure Reset (F : in out Filter_State) is
   begin
      F.Length := 0;
      F.Shown := 0;
      F.Scanning := False;
      F.Fresh := False;
      F.Live := False;
      F.Tracked_At := 0;
      F.Changes := F.Changes + 1;
   end Reset;

   procedure Set_Query (F : in out Filter_State; Text : String) is
      New_Text : Query_Bytes := [others => 0];
      Same, Contains_Old : Boolean;
   begin
      for K in 1 .. Text'Length loop
         New_Text (K) := Fold (Character'Pos (Text (Text'First + (K - 1))));
      end loop;
      Same := Text'Length = F.Length and then
        (for all K in 1 .. F.Length => New_Text (K) = F.Text (K));
      if Same then
         return;
      end if;
      --  The new query's matches are among the old one's when it contains
      --  the old query.
      Contains_Old := False;
      if F.Length > 0 and then F.Length <= Text'Length then
         for Start in 1 .. Text'Length - F.Length + 1 loop
            if (for all K in 1 .. F.Length => New_Text (Start + K - 1) = F.Text (K)) then
               Contains_Old := True;
               exit;
            end if;
         end loop;
      end if;
      F.Text := New_Text;
      F.Length := Text'Length;
      F.Changes := F.Changes + 1;
      if F.Length = 0 then
         F.Shown := 0;
         F.Scanning := False;
         F.Fresh := False;
         F.Live := False;
      elsif Contains_Old and then not F.Scanning and then not F.Fresh then
         --  Refine the shown result, showing the new one as it grows.
         F.Scanning := True;
         F.Live := True;
         F.From_Order := False;
         F.Source_Count := Count (F);
         F.Target := (if F.Front = 1 then 2 else 1);
         F.Front := F.Target;
         F.Next := 1;
         F.Built := 0;
         F.Built_Tracked_At := 0;
         F.Shown := 0;
         F.Scan_Of := F.Seen;
      else
         F.Fresh := True;
      end if;
   end Set_Query;

   procedure Track (F : in out Filter_State; Id : Entry_Id; Position : Entry_Count) is
   begin
      F.Tracked := Id;
      F.Tracked_At := Position;
      F.Built_Tracked_At := 0;
   end Track;

   procedure Step
     (F : in out Filter_State; L : Listing; O : Files_Order.Order_State;
      Budget : Work_Budget; Used : out Work_Budget)
   is
      Left_Over : Work_Budget := Budget;
      Order_Now : constant Unsigned_64 := Files_Order.Revision (O);
      Shown_Before : constant Entry_Count := Count (F);
   begin
      Used := 0;
      if F.Length = 0 then
         return;
      end if;
      loop
         pragma Loop_Invariant (Left_Over <= Budget and then F.Length > 0);
         exit when Left_Over = 0;
         if F.Fresh or else (not F.Scanning and then Order_Now /= F.Seen)
           or else (F.Scanning and then Order_Now /= F.Scan_Of)
         then
            --  A new query or a new order: scan the order. A query change,
            --  or nothing shown yet, shows the result as it grows.
            F.Live := F.Fresh or else (F.Scanning and then F.Live) or else F.Shown = 0;
            F.Fresh := False;
            F.Scanning := True;
            F.From_Order := True;
            F.Source_Count := Files_Order.Count (O);
            if F.Live then
               F.Target := F.Front;
               F.Shown := 0;
            else
               F.Target := (if F.Front = 1 then 2 else 1);
            end if;
            F.Next := 1;
            F.Built := 0;
            F.Built_Tracked_At := 0;
            F.Scan_Of := Order_Now;
         elsif not F.Scanning then
            exit;
         elsif F.Next > F.Source_Count or else F.Next = 0 then
            --  Done: the result replaces the shown one.
            F.Front := F.Target;
            F.Shown := Natural'Min (F.Built, F.Capacity);
            F.Tracked_At := F.Built_Tracked_At;
            F.Scanning := False;
            F.Live := False;
            F.Seen := F.Scan_Of;
            F.Changes := F.Changes + 1;
            exit;
         else
            declare
               Source : constant Buffer_Number := (if F.Target = 1 then 2 else 1);
               Id : constant Entry_Id :=
                 (if F.From_Order then
                    (if F.Next <= Files_Order.Count (O) then Files_Order.At_Position (O, F.Next) else 1)
                  elsif F.Next <= F.Capacity then F.Ids (Source, F.Next) else 1);
            begin
               if Matches (L, Id, F) and then F.Built < F.Capacity then
                  F.Built := F.Built + 1;
                  F.Ids (F.Target, F.Built) := Id;
                  if Id = F.Tracked then
                     F.Built_Tracked_At := F.Built;
                  end if;
                  if F.Live then
                     F.Shown := F.Built;
                     F.Tracked_At := F.Built_Tracked_At;
                  end if;
               end if;
               F.Next := F.Next + 1;
               Left_Over := Left_Over - 1;
            end;
         end if;
      end loop;
      if Count (F) /= Shown_Before then
         F.Changes := F.Changes + 1;
      end if;
      Used := Budget - Left_Over;
   end Step;
end Files_Filter;
