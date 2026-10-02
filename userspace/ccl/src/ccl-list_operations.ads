with Interfaces;

--  The list built-ins' algorithms, shared by the interpreter (CCL.Language)
--  and the bytecode VM (CCL.VM), as CCL.Text_Operations is for strings: the
--  index arithmetic, the size of a range and the sort live here once. Each
--  engine supplies only element access over its own list region, so the two
--  cannot disagree about which elements a built-in keeps or in what order.
package CCL.List_Operations with SPARK_Mode => On is
   use Interfaces;

   --  The built-ins a compiled program can name: List_Builtin's immediate.
   --  The subject list (or, for split, the subject string) is the last
   --  operand in source; range has two Integer operands and no subject.
   type Operation is
     (First_Items, Last_Items, Skip_Items, Reverse_Items, Sort_Items,
      Sum_Items, Min_Items, Max_Items, Contains_Item, Join_Items,
      Range_Items, Split_Text);
   for Operation use
     (First_Items => 0, Last_Items => 1, Skip_Items => 2, Reverse_Items => 3,
      Sort_Items => 4, Sum_Items => 5, Min_Items => 6, Max_Items => 7,
      Contains_Item => 8, Join_Items => 9, Range_Items => 10, Split_Text => 11);

   --  The built-ins that apply a function to each element (List_Apply's
   --  immediate). Their operands in source: the function, fold's initial
   --  value, then the subject list.
   type Apply_Operation is
     (Each_Items, Where_Items, Fold_Items, Any_Items, All_Items, Count_Items, Sort_By_Items);
   for Apply_Operation use
     (Each_Items => 0, Where_Items => 1, Fold_Items => 2, Any_Items => 3, All_Items => 4,
      Count_Items => 5, Sort_By_Items => 6);

   --  The positions From .. To of a Length-element list that first, last or
   --  skip keeps, N clamped to 0 .. Length. Empty when To < From.
   procedure Take_Bounds
     (Item : Operation; N : Integer_64; Length : Natural;
      From : out Positive; To : out Natural)
   with Pre => Item in First_Items | Last_Items | Skip_Items and then
               Length < Positive'Last,
        Post => To <= Length and then From <= Length + 1;

   --  The length of (range Low High): High - Low + 1, or 0 when High < Low.
   --  Fits is False when that exceeds Capacity.
   procedure Range_Length
     (Low, High : Integer_64; Capacity : Natural;
      Size : out Natural; Fits : out Boolean)
   with Post => (if Fits then Size <= Capacity);

   --  Heapsort of positions 1 .. Length: n log n calls of Less, no scratch.
   --  Less may charge fuel; any Good = False stops the sort.
   generic
      with procedure Less (I, J : Positive; Before : out Boolean; Good : out Boolean);
      with procedure Swap (I, J : Positive; Good : out Boolean);
   procedure Heap_Sort (Length : Natural; Good : out Boolean)
   with Pre => Length < Positive'Last / 2;
end CCL.List_Operations;
