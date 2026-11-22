package body Files_Order with SPARK_Mode is
   use Files_Listing;

   --  The digit '0' and the first byte past a name: cursors run to it.
   ZERO : constant Unsigned_8 := Character'Pos ('0');
   subtype Cursor is Natural range 1 .. MAXIMUM_NAME_BYTES + 1;

   function Compare_Names (L : Listing; A, B : Entry_Id) return Ordering is
      LA : constant Name_Length := Length (L, A);
      LB : constant Name_Length := Length (L, B);
      PA : constant Sort_Prefix := Prefix (L, A);
      PB : constant Sort_Prefix := Prefix (L, B);
      I, J : Cursor := 1;
   begin
      --  The prefixes agree with the full comparison wherever they differ.
      if PA /= PB then
         return (if PA < PB then Less else Greater);
      end if;
      --  Every round takes at least one byte of A.
      for Round in Cursor loop
         pragma Loop_Invariant (I <= Cursor'Last and then J <= Cursor'Last);
         if I > LA or else J > LB then
            return (if I > LA and then J > LB then Same elsif I > LA then Less else Greater);
         end if;
         declare
            X : constant Unsigned_8 := Fold (Byte_At (L, A, I));
            Y : constant Unsigned_8 := Fold (Byte_At (L, B, J));
         begin
            if Is_Digit (X) and then Is_Digit (Y) then
               --  Numbers by value: leading zeros skipped, longer is larger,
               --  then digit by digit.
               while I <= LA and then Byte_At (L, A, I) = ZERO loop
                  pragma Loop_Variant (Increases => I);
                  I := I + 1;
               end loop;
               while J <= LB and then Byte_At (L, B, J) = ZERO loop
                  pragma Loop_Variant (Increases => J);
                  J := J + 1;
               end loop;
               declare
                  EA : Cursor := I;
                  EB : Cursor := J;
               begin
                  while EA <= LA and then Is_Digit (Byte_At (L, A, EA)) loop
                     pragma Loop_Invariant (EA >= I);
                     pragma Loop_Variant (Increases => EA);
                     EA := EA + 1;
                  end loop;
                  while EB <= LB and then Is_Digit (Byte_At (L, B, EB)) loop
                     pragma Loop_Invariant (EB >= J);
                     pragma Loop_Variant (Increases => EB);
                     EB := EB + 1;
                  end loop;
                  if EA - I /= EB - J then
                     return (if EA - I < EB - J then Less else Greater);
                  end if;
                  for K in 0 .. EA - I - 1 loop
                     pragma Loop_Invariant (EA - I = EB - J);
                     declare
                        DA : constant Unsigned_8 := Byte_At (L, A, I + K);
                        DB : constant Unsigned_8 := Byte_At (L, B, J + K);
                     begin
                        if DA /= DB then
                           return (if DA < DB then Less else Greater);
                        end if;
                     end;
                  end loop;
                  I := EA;
                  J := EB;
               end;
            elsif X /= Y then
               --  A number against anything else compares as its '0'.
               declare
                  XA : constant Unsigned_8 := (if Is_Digit (X) then DIGIT_MARK else X);
                  YB : constant Unsigned_8 := (if Is_Digit (Y) then DIGIT_MARK else Y);
               begin
                  return (if XA < YB then Less elsif XA > YB then Greater
                          elsif Is_Digit (X) then Less else Greater);
               end;
            else
               I := I + 1;
               J := J + 1;
            end if;
         end;
      end loop;
      return Same;
   end Compare_Names;

   --  Extensions by folded bytes; none sorts first.
   function Compare_Extensions (L : Listing; A, B : Entry_Id) return Ordering is
      LA : constant Name_Length := Length (L, A);
      LB : constant Name_Length := Length (L, B);
      EA : constant Name_Length := Extension_At (L, A);
      EB : constant Name_Length := Extension_At (L, B);
      I : Cursor := (if EA = 0 then LA + 1 else EA);
      J : Cursor := (if EB = 0 then LB + 1 else EB);
   begin
      for Round in Cursor loop
         if I > LA or else J > LB then
            return (if I > LA and then J > LB then Same elsif I > LA then Less else Greater);
         end if;
         declare
            X : constant Unsigned_8 := Fold (Byte_At (L, A, I));
            Y : constant Unsigned_8 := Fold (Byte_At (L, B, J));
         begin
            if X /= Y then
               return (if X < Y then Less else Greater);
            end if;
         end;
         I := I + 1;
         J := J + 1;
      end loop;
      return Same;
   end Compare_Extensions;

   function Before (L : Listing; Rule : Sort_Rule; A, B : Entry_Id) return Boolean is
      FA : constant Entry_Facts := Facts (L, A);
      FB : constant Entry_Facts := Facts (L, B);
      DA : constant Boolean := FA.Kind = Directory_Kind;
      DB : constant Boolean := FB.Kind = Directory_Kind;
      C : Ordering := Same;
   begin
      if DA /= DB then
         return DA;
      end if;
      case Rule.Key is
         when By_Name =>
            null;
         when By_Extension =>
            C := Compare_Extensions (L, A, B);
         when By_Size =>
            --  Folders have no size of their own: by name.
            if not DA then
               declare
                  SA : constant Byte_Size := (if FA.Size_Known then FA.Size else 0);
                  SB : constant Byte_Size := (if FB.Size_Known then FB.Size else 0);
               begin
                  C := (if SA < SB then Less elsif SA > SB then Greater else Same);
               end;
            end if;
         when By_Modified =>
            declare
               TA : constant Time_Ms := (if FA.Modified_Known then FA.Modified else 0);
               TB : constant Time_Ms := (if FB.Modified_Known then FB.Modified else 0);
            begin
               C := (if TA < TB then Less elsif TA > TB then Greater else Same);
            end;
      end case;
      if C = Same then
         C := Compare_Names (L, A, B);
      end if;
      if C = Same then
         return A < B;
      end if;
      return (if Rule.Direction = Ascending then C = Less else C = Greater);
   end Before;

   procedure Track (O : in out Order_State; Id : Entry_Id; Position : Entry_Count) is
   begin
      O.Tracked := Id;
      O.Tracked_At := Position;
      --  A job may already have moved it unseen: found by search then.
      O.Built_Tracked_At := 0;
   end Track;

   function Position_Of (O : Order_State; Id : Entry_Id) return Entry_Count is
   begin
      for Position in 1 .. Count (O) loop
         if O.Ids (O.Front, Position) = Id then
            return Position;
         end if;
      end loop;
      return 0;
   end Position_Of;

   procedure Reset (O : in out Order_State) is
   begin
      O.Published := 0;
      O.Phase := Idle;
      O.Shown_Rule := O.Wanted;
      O.Changes := O.Changes + 1;
   end Reset;

   procedure Set_Rule (O : in out Order_State; Value : Sort_Rule) is
   begin
      if Value /= O.Wanted then
         O.Wanted := Value;
         --  Any job in progress sorts by the old rule: start again.
         O.Phase := Idle;
      end if;
   end Set_Rule;

   --  The work buffer that is neither published nor holding the runs.
   function Other (O : Order_State) return Buffer_Number is
     (if O.Front /= 1 and then O.Runs /= 1 then 1
      elsif O.Front /= 2 and then O.Runs /= 2 then 2 else 3);

   --  Total entries in From are the new order; a job whose entries the
   --  listing no longer holds (Limit) is dropped instead.
   procedure Publish (O : in out Order_State; From : Buffer_Number; Total, Limit : Entry_Count)
     with Post => O.Wanted = O.Wanted'Old and then O.Phase = Idle
                  and then (if O.Published /= O.Published'Old then O.Published <= Limit and then O.Published <= O.Capacity)
   is
   begin
      if Total > Limit or else Total > O.Capacity then
         O.Phase := Idle;
         return;
      end if;
      O.Front := From;
      O.Published := Total;
      O.Tracked_At := (if O.Built_Tracked_At <= Total then O.Built_Tracked_At else 0);
      O.Shown_Rule := O.Wanted;
      O.Changes := O.Changes + 1;
      O.Phase := Idle;
   end Publish;

   procedure Start (O : in out Order_State; Full : Boolean; Base, Last : Entry_Count)
     with Pre => Base < Last and then Last <= O.Capacity,
          Post => O.Wanted = O.Wanted'Old and then O.Published = O.Published'Old and then O.Phase = Sorting
   is
   begin
      O.Full := Full;
      O.Base := Base;
      O.Last := Last;
      O.Length := Last - Base;
      O.Runs := (if O.Front = 1 then 2 else 1);
      O.Width := 1;
      O.Pair := 1;
      O.Left := 1;
      O.Right := 2;
      O.Output := 1;
      O.Built_Tracked_At := 0;
      O.Phase := Sorting;
   end Start;

   --  The runs are sorted: publish them, or merge them into the published
   --  order.
   procedure Finish_Sort (O : in out Order_State; Limit : Entry_Count)
     with Post => O.Wanted = O.Wanted'Old
                  and then (if O.Published /= O.Published'Old then O.Published <= Limit and then O.Published <= O.Capacity)
   is
   begin
      if O.Length = 1 and then O.Base + 1 <= ABSOLUTE_MAXIMUM_ENTRIES then
         --  No pass ran: the one entry is its own run.
         O.Ids (O.Runs, 1) := O.Base + 1;
         if O.Base + 1 = O.Tracked then
            O.Built_Tracked_At := 1;
         end if;
      end if;
      if O.Full or else O.Base = 0 then
         Publish (O, O.Runs, O.Last, Limit);
      else
         O.Phase := Merging;
         O.Left := 1;
         O.Right := 1;
         O.Output := 1;
      end if;
   end Finish_Sort;

   procedure Sort_Work (O : in out Order_State; L : Listing; Left_Over : in out Work_Budget)
     with Pre => O.Phase = Sorting and then O.Published <= O.Capacity and then Count (O) <= L.Count,
          Post => Left_Over <= Left_Over'Old and then O.Wanted = O.Wanted'Old
                  and then O.Published <= O.Capacity and then Count (O) <= L.Count
   is
   begin
      while Left_Over > 0 loop
         pragma Loop_Invariant
           (O.Phase = Sorting and then Left_Over <= Left_Over'Loop_Entry and then O.Wanted = O.Wanted'Loop_Entry
            and then O.Published = O.Published'Loop_Entry);
         if O.Front = O.Runs or else O.Length = 0 or else O.Length > O.Capacity or else O.Width = 0
           or else O.Pair = 0 or else O.Base > O.Capacity - O.Length
         then
            --  Not a consistent job: drop it; Step starts afresh.
            O.Phase := Idle;
            return;
         end if;
         if O.Width >= O.Length then
            Finish_Sort (O, L.Count);
            return;
         end if;
         declare
            N : constant Entry_Count := O.Length;
            Mid : constant Natural := (if O.Width > N - Natural'Min (O.Pair, N) then N else O.Pair + O.Width - 1);
            Stop : constant Natural :=
              (if O.Width > N - Mid then N else Mid + O.Width);
            Target : constant Buffer_Number := Other (O);
            function Value (Position : Entry_Id) return Entry_Id is
              (if O.Width = 1 then
                 (if O.Base < ABSOLUTE_MAXIMUM_ENTRIES - Position + 1 then O.Base + Position else Position)
               elsif Position <= O.Capacity then O.Ids (O.Runs, Position) else Position);
         begin
            if O.Output > Stop or else O.Pair > N then
               --  This pair is merged: the next one, or the next pass.
               if Stop >= N then
                  O.Runs := Target;
                  O.Width := Natural'Min (2 * Natural'Min (O.Width, N), N);
                  O.Pair := 1;
               else
                  O.Pair := Stop + 1;
               end if;
               O.Left := O.Pair;
               O.Right := (if O.Width > N then N + 1 else O.Pair + O.Width);
               O.Output := O.Pair;
            elsif O.Output = 0 or else O.Output > O.Capacity or else O.Left = 0 or else O.Right = 0 then
               O.Phase := Idle;
               return;
            else
               if O.Left <= Mid and then
                 (O.Right > Stop or else not Before (L, O.Wanted, Value (O.Right), Value (O.Left)))
               then
                  O.Ids (Target, O.Output) := Value (O.Left);
                  O.Left := O.Left + 1;
                  if O.Ids (Target, O.Output) = O.Tracked then
                     O.Built_Tracked_At := O.Output;
                  end if;
               elsif O.Right <= Stop then
                  O.Ids (Target, O.Output) := Value (O.Right);
                  O.Right := O.Right + 1;
                  if O.Ids (Target, O.Output) = O.Tracked then
                     O.Built_Tracked_At := O.Output;
                  end if;
               else
                  O.Phase := Idle;
                  return;
               end if;
               O.Output := O.Output + 1;
               Left_Over := Left_Over - 1;
            end if;
         end;
      end loop;
   end Sort_Work;

   procedure Merge_Work (O : in out Order_State; L : Listing; Left_Over : in out Work_Budget)
     with Pre => O.Phase = Merging and then O.Published <= O.Capacity and then Count (O) <= L.Count,
          Post => Left_Over <= Left_Over'Old and then O.Wanted = O.Wanted'Old
                  and then O.Published <= O.Capacity and then Count (O) <= L.Count
   is
   begin
      while Left_Over > 0 loop
         pragma Loop_Invariant
           (O.Phase = Merging and then Left_Over <= Left_Over'Loop_Entry and then O.Wanted = O.Wanted'Loop_Entry
            and then O.Published = O.Published'Loop_Entry);
         if O.Front = O.Runs or else O.Base > O.Capacity or else O.Length > O.Capacity - O.Base
           or else O.Left = 0 or else O.Right = 0 or else O.Output = 0
         then
            O.Phase := Idle;
            return;
         end if;
         declare
            Total : constant Entry_Count := O.Base + O.Length;
            Target : constant Buffer_Number := Other (O);
         begin
            if O.Output > Total then
               Publish (O, Target, Total, L.Count);
               return;
            elsif O.Left <= O.Base and then
              (O.Right > O.Length or else
               not Before (L, O.Wanted, O.Ids (O.Runs, O.Right), O.Ids (O.Front, O.Left)))
            then
               O.Ids (Target, O.Output) := O.Ids (O.Front, O.Left);
               O.Left := O.Left + 1;
               if O.Ids (Target, O.Output) = O.Tracked then
                  O.Built_Tracked_At := O.Output;
               end if;
            elsif O.Right <= O.Length then
               O.Ids (Target, O.Output) := O.Ids (O.Runs, O.Right);
               O.Right := O.Right + 1;
               if O.Ids (Target, O.Output) = O.Tracked then
                  O.Built_Tracked_At := O.Output;
               end if;
            else
               O.Phase := Idle;
               return;
            end if;
            O.Output := O.Output + 1;
            Left_Over := Left_Over - 1;
         end;
      end loop;
   end Merge_Work;

   procedure Step
     (O : in out Order_State; L : Listing; Budget : Work_Budget; Settle : Boolean;
      Used : out Work_Budget)
   is
      Left_Over : Work_Budget := Budget;
   begin
      loop
         pragma Loop_Invariant (Left_Over <= Budget and then O.Wanted = O.Wanted'Loop_Entry
                                and then Consistent (O) and then Count (O) <= L.Count);
         exit when Left_Over = 0;
         case O.Phase is
            when Idle =>
               if O.Wanted /= O.Shown_Rule then
                  if O.Published = 0 then
                     O.Shown_Rule := O.Wanted;
                     O.Changes := O.Changes + 1;
                  else
                     Start (O, Full => True, Base => 0, Last => L.Count);
                  end if;
               elsif O.Published < L.Count and then
                 (Settle or else L.Complete or else O.Published < FIRST_ROWS or else
                  L.Count - O.Published >= O.Published / 2)
               then
                  Start (O, Full => False, Base => O.Published, Last => L.Count);
               else
                  exit;
               end if;
            when Sorting =>
               Sort_Work (O, L, Left_Over);
            when Merging =>
               Merge_Work (O, L, Left_Over);
         end case;
      end loop;
      Used := Budget - Left_Over;
   end Step;
end Files_Order;
