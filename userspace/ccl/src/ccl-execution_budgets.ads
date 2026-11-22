with Interfaces;

package CCL.Execution_Budgets with
   SPARK_Mode => On
is
   use type Interfaces.Unsigned_32;

   type Budget is private;

   type Consume_Result is (Consumed, Exhausted, Invalid_Budget);

   function Has_Fuel (Item : Budget) return Boolean;
   function Remaining (Item : Budget) return Interfaces.Unsigned_32;
   function Steps (Item : Budget) return Interfaces.Unsigned_32;
   function Limit (Item : Budget) return Interfaces.Unsigned_32;

   procedure Initialize (Item : out Budget; Amount : Natural) with
     Post => Limit (Item) = Interfaces.Unsigned_32 (Amount) and then
       Remaining (Item) = Interfaces.Unsigned_32 (Amount) and then
       Steps (Item) = 0;

   procedure Consume (Item : in out Budget; Result : out Consume_Result) with
     Post => Limit (Item) = Limit (Item'Old) and then
       Steps (Item) <= Limit (Item);

private
   type Budget is record
      Initial   : Interfaces.Unsigned_32 := 0;
      Available : Interfaces.Unsigned_32 := 0;
   end record;

   function Has_Fuel (Item : Budget) return Boolean is
     (Item.Initial <= Interfaces.Unsigned_32 (Natural'Last) and then
      Item.Available > 0 and then Item.Available <= Item.Initial);

   function Remaining (Item : Budget) return Interfaces.Unsigned_32 is
     (Item.Available);

   function Steps (Item : Budget) return Interfaces.Unsigned_32 is
     (if Item.Initial <= Interfaces.Unsigned_32 (Natural'Last) and then
         Item.Available <= Item.Initial
      then
         Item.Initial - Item.Available
      else
         0);

   function Limit (Item : Budget) return Interfaces.Unsigned_32 is
     (if Item.Initial <= Interfaces.Unsigned_32 (Natural'Last) then
         Item.Initial
      else
         0);
end CCL.Execution_Budgets;
