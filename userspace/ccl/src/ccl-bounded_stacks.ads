with Interfaces;

generic
   type Index_Type is mod <>;
   type Element_Type is private;
   Default_Value : Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package CCL.Bounded_Stacks with
   SPARK_Mode => On
is
   use Interfaces;
   Capacity : constant Unsigned_32 := Unsigned_32 (Index_Type'Modulus);
   type Stack is private;

   type Operation_Result is
     (Stack_Ok, Stack_Full, Stack_Empty, Stack_Invalid);

   procedure Initialize (Item : out Stack);

   procedure Push
     (Item   : in out Stack;
      Value  : Element_Type;
      Result : out Operation_Result);

   procedure Pop
     (Item   : in out Stack;
      Value  : out Element_Type;
      Result : out Operation_Result);

   procedure Peek_Top
     (Item   : Stack;
      Value  : out Element_Type;
      Result : out Operation_Result);

   function "=" (Left, Right : Stack) return Boolean;

private
   type Element_Array is array (Index_Type) of Element_Type;

   type Stack is record
      Elements : Element_Array := [others => Default_Value];
      Next     : Index_Type := Index_Type'First;
      Count    : Unsigned_32 := 0;
   end record;

end CCL.Bounded_Stacks;
