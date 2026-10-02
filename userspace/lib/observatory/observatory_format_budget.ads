package Observatory_Format_Budget with SPARK_Mode is
   subtype Row_Count is Natural range 0 .. 16;
   subtype Row_Index is Row_Count range 0 .. 15;
   type State is private;
   function Pending (Item : State) return Row_Count;
   function Active (Item : State) return Boolean is (Pending (Item) > 0);
   function Current (Item : State) return Row_Index with Pre => Active (Item);
   procedure Start (Item : in out State; Rows : Row_Count)
     with Pre => not Active (Item), Post => Pending (Item) = Rows;
   procedure Advance (Item : in out State)
     with Pre => Active (Item), Post => Pending (Item) = Pending (Item'Old) - 1;
   procedure Cancel (Item : out State) with Post => not Active (Item);
private
   type State is record
      Next, Limit : Row_Count := 0;
   end record with Type_Invariant => State.Next <= State.Limit;
   function Pending (Item : State) return Row_Count is (Item.Limit - Item.Next);
end Observatory_Format_Budget;
