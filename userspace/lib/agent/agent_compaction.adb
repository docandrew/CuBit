pragma Ada_2022;

package body Agent_Compaction with SPARK_Mode is
   use type P.Role;
   use type P.Block_Kind;

   Maximum_Digits : constant := 10;
   function Decimal (Value : Natural) return String
   with Post => Decimal'Result'Length in 1 .. Maximum_Digits
   is
      Digits_Text : String (1 .. Maximum_Digits) := [others => '0'];
      First : Positive range 1 .. Maximum_Digits := Maximum_Digits;
      V : Natural := Value;
   begin
      for K in reverse 1 .. Maximum_Digits loop
         Digits_Text (K) := Character'Val (Character'Pos ('0') + V mod 10);
         V := V / 10;
         if Digits_Text (K) /= '0' then
            First := K;
         end if;
      end loop;
      return Digits_Text (First .. Maximum_Digits);
   end Decimal;

   --  Whether a block's bytes stay in the kept tail.
   function Kept (Kind : P.Block_Kind) return Boolean is
     (Kind not in P.Thinking_Block | P.Redacted_Block);

   function Plan_For (Conversation : String; Target : Natural) return Plan is
      At_Index : Positive := Conversation'First;
      View : Agent_Blocks.Block_View;
      OK : Boolean;
      Blocks_Before : Natural := 0;
      Result : Plan;
   begin
      while At_Index <= Conversation'Last loop
         pragma Loop_Invariant (At_Index in Conversation'Range);
         pragma Loop_Invariant
           (if Result.Found then
              Result.Keep_From in 2 .. Conversation'Last and then
              Result.Dropped_Bytes = Result.Keep_From - 1);
         pragma Loop_Variant (Increases => At_Index);
         Agent_Blocks.Read (Conversation, At_Index, View, OK);
         exit when not OK;
         if At_Index > Conversation'First and then View.Item_Role = P.User
           and then View.Kind = P.Text_Block
         then
            --  A turn starts here: drop what comes before it.
            Result := (Found => True, Keep_From => At_Index, Dropped_Blocks => Blocks_Before,
                       Dropped_Bytes => At_Index - 1);
            exit when Conversation'Last - At_Index + 1 + P.Block_Header_Bytes + Maximum_Marker <= Target;
         end if;
         if Blocks_Before < Natural'Last then
            Blocks_Before := Blocks_Before + 1;
         end if;
         exit when View.Next > Conversation'Last;
         At_Index := View.Next;
      end loop;
      return Result;
   end Plan_For;

   function Marker_Text (Item : Plan) return String is
      Text : constant String :=
        Marker_Prefix & Decimal (Item.Dropped_Blocks) & " earlier blocks (" & Decimal (Item.Dropped_Bytes) &
        " bytes) were dropped to keep this conversation within its limit; earlier thinking is not " &
        "carried forward]";
   begin
      if Text'Length <= Maximum_Marker then
         return Text;
      else
         return Text (Text'First .. Text'First + Maximum_Marker - 1);
      end if;
   end Marker_Text;

   function Compacted_Length (Conversation : String; Item : Plan) return Natural is
      Total : Natural := P.Block_Header_Bytes + Marker_Text (Item)'Length;
      At_Index : Positive := Item.Keep_From;
      View : Agent_Blocks.Block_View;
      OK : Boolean;
   begin
      while At_Index <= Conversation'Last loop
         pragma Loop_Invariant (At_Index in Item.Keep_From .. Conversation'Last);
         pragma Loop_Invariant
           (Total <= P.Block_Header_Bytes + Maximum_Marker + (At_Index - Item.Keep_From));
         pragma Loop_Invariant (Total >= P.Block_Header_Bytes + Marker_Text (Item)'Length);
         pragma Loop_Variant (Increases => At_Index);
         Agent_Blocks.Read (Conversation, At_Index, View, OK);
         exit when not OK;
         if Kept (View.Kind) then
            Total := Total + (View.Next - At_Index);
         end if;
         exit when View.Next > Conversation'Last;
         At_Index := View.Next;
      end loop;
      return Total;
   end Compacted_Length;

   procedure Compact (Conversation : String; Item : Plan; Into : out String; Last : out Natural) is
      Marker : constant String := Agent_Blocks.Encode (P.User, P.Text_Block, Marker_Text (Item));
      At_Index : Positive := Item.Keep_From;
      View : Agent_Blocks.Block_View;
      OK : Boolean;
   begin
      Into := [others => ' '];
      Into (1 .. Marker'Length) := Marker;
      Last := Marker'Length;
      while At_Index <= Conversation'Last loop
         pragma Loop_Invariant (At_Index in Item.Keep_From .. Conversation'Last);
         pragma Loop_Invariant (Last <= Into'Last);
         pragma Loop_Variant (Increases => At_Index);
         Agent_Blocks.Read (Conversation, At_Index, View, OK);
         exit when not OK;
         if Kept (View.Kind) then
            declare
               Length : constant Natural := View.Next - At_Index;
            begin
               exit when Length > Into'Last - Last;
               Into (Last + 1 .. Last + Length) := Conversation (At_Index .. View.Next - 1);
               Last := Last + Length;
            end;
         end if;
         exit when View.Next > Conversation'Last;
         At_Index := View.Next;
      end loop;
   end Compact;
end Agent_Compaction;
