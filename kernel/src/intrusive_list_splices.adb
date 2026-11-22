package body Intrusive_List_Splices with SPARK_Mode is
   procedure Insert_Front
     (Head_Next : in out Reference;
      First_Previous, New_Previous, New_Next : out Reference;
      Head, Item : Reference)
   is
   begin
      New_Previous := Head;
      New_Next := Head_Next;
      Head_Next := Item;
      First_Previous := Item;
   end Insert_Front;

   procedure Remove
     (Previous_Next, Next_Previous : out Reference;
      Previous, Following : Reference)
   is
   begin
      Previous_Next := Following;
      Next_Previous := Previous;
   end Remove;

   procedure Prove_Insert_Remove
     (Head, Item, First : Reference; Count : Natural)
   is
      Head_Next : Reference := First;
      First_Previous : Reference := Head;
      New_Previous, New_Next : Reference;
      Length : Natural := Count;
   begin
      Insert_Front (Head_Next, First_Previous, New_Previous, New_Next,
                    Head, Item);
      Length := Added (Length);
      Remove (Head_Next, First_Previous, New_Previous, New_Next);
      Length := Removed (Length);
      pragma Assert (Head_Next = First and First_Previous = Head and Length = Count);
   end Prove_Insert_Remove;

   procedure Prove_Singleton_Removal (Head, Item : Reference) is
      Head_Next : Reference := Item;
      Head_Previous : Reference := Item;
      Length : Natural := 1;
   begin
      Remove (Head_Next, Head_Previous, Head, Head);
      Length := Removed (Length);
      pragma Assert (Head_Next = Head and Head_Previous = Head and Length = 0);
   end Prove_Singleton_Removal;
end Intrusive_List_Splices;
