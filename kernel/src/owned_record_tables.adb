with System.Address_To_Access_Conversions;
package body Owned_Record_Tables is
   use type System.Address;
   use type Interfaces.Unsigned_64;
   subtype Offset is Natural range 0 .. Records_Per_Block - 1;
   subtype Page_Index is Natural range 0 .. (Maximum_Records - 1) / Records_Per_Block;
   type Record_Data is record
      Used : Boolean := False;
      Key : Interfaces.Unsigned_64 := 0;
      Previous, Following : Id := 0;
      Value : aliased Element;
   end record;
   type Records is array (Offset) of Record_Data;
   type Block is record
      Used : Natural range 0 .. Records_Per_Block := 0;
      Items : Records;
   end record;
   type Block_Pointer is access all Block;
   type Directory is array (Page_Index) of Block_Pointer;
   package Block_Conversion is new System.Address_To_Access_Conversions (Block);
   package Directory_Conversion is new System.Address_To_Access_Conversions (Directory);
   Block_Bytes : constant Positive := (Block'Size + System.Storage_Unit - 1) / System.Storage_Unit;
   Directory_Bytes : constant Positive := (Directory'Size + System.Storage_Unit - 1) / System.Storage_Unit;
   function Page_Of (Index : Valid_Id) return Page_Index is ((Index - 1) / Records_Per_Block);
   function Offset_Of (Index : Valid_Id) return Offset is ((Index - 1) mod Records_Per_Block);
   function Present (Object : Table; Index : Id) return Boolean is
     (Index /= 0 and then Object.Pages /= null and then
      Object.Pages(Page_Of(Index)) /= null and then
      Object.Pages(Page_Of(Index)).Items(Offset_Of(Index)).Used);
   function Get (Object : Table; Index : Valid_Id) return Reference is
     ((Value => Object.Pages(Page_Of(Index)).Items(Offset_Of(Index)).Value'Access));
   function First (Object : Table) return Id is (Object.Head);
   function Next (Object : Table; Index : Valid_Id) return Id is
     (Object.Pages(Page_Of(Index)).Items(Offset_Of(Index)).Following);
   function Count (Object : Table) return Natural is (Object.Used);

   function Start (Object : View) return Cursor is
      I : constant Id := First (Object.Object.all);
   begin
      return (I, (if I = 0 then 0 else Next (Object.Object.all, I)));
   end Start;
   function Advance (Object : View; Position : Cursor) return Cursor is
      I : constant Id := Position.Following;
   begin
      return (I, (if I = 0 then 0 else Next (Object.Object.all, I)));
   end Advance;
   function Has_Element (Object : View; Position : Cursor) return Boolean is
     (Position.Index /= 0);
   function Current (Object : View; Position : Cursor) return Id is (Position.Index);
   function Iterate (Object : not null Table_Access) return View is ((Object => Object));

   procedure Insert (Object : in out Table; Key : Interfaces.Unsigned_64;
                     Initial : Element; Index : out Id) is
      Address : System.Address;
      Chosen : Page_Index := 0;
      Found : Boolean := False;
      Before, Prior : Id := 0;
   begin
      Index := 0;
      if Object.Used = Maximum_Records then return; end if;
      if Object.Pages = null then
         Allocate_Memory (Directory_Bytes, Address);
         if Address = System.Null_Address then return; end if;
         Object.Pages := Directory_Pointer (Directory_Conversion.To_Pointer (Address));
         Object.Pages.all := (others => null);
      end if;
      -- Reuse existing non-full blocks before allocating more backing.
      for Delta_Index in 0 .. Page_Index'Last loop
         Chosen := (Object.Hint + Delta_Index) mod (Page_Index'Last + 1);
         if Object.Pages(Chosen) /= null and then
           Object.Pages(Chosen).Used < Natural'Min
             (Records_Per_Block, Maximum_Records - Chosen * Records_Per_Block)
         then Found := True; exit; end if;
      end loop;
      if not Found then
         for P in Page_Index loop
            if Object.Pages(P) = null then Chosen := P; Found := True; exit; end if;
         end loop;
         pragma Assert (Found);
         Allocate_Memory (Block_Bytes, Address);
         if Address = System.Null_Address then
            if Object.Used = 0 then
               Free_Memory (Directory_Bytes, Object.Pages.all'Address);
               Object.Pages := null;
            end if;
            return;
         end if;
         Object.Pages(Chosen) := Block_Pointer (Block_Conversion.To_Pointer (Address));
         -- Initialize only admission bits. Payload and links become readable
         -- after Insert assigns them; avoid a whole-block stack temporary.
         Object.Pages(Chosen).Used := 0;
         for O in Offset loop Object.Pages(Chosen).Items(O).Used := False; end loop;
      end if;
      Object.Hint := Chosen;
      for O in Offset loop
         if not Object.Pages(Chosen).Items(O).Used then
            Index := Chosen * Records_Per_Block + O + 1;
            exit;
         end if;
      end loop;
      pragma Assert (Index /= 0);
      -- Fast append for the common monotonically allocated address sequence.
      if Object.Tail /= 0 and then
        Object.Pages(Page_Of(Object.Tail)).Items(Offset_Of(Object.Tail)).Key <= Key
      then Prior := Object.Tail;
      else
         Before := Object.Head;
         while Before /= 0 loop
            exit when Object.Pages(Page_Of(Before)).Items(Offset_Of(Before)).Key > Key;
            Prior := Before;
            Before := Next (Object, Before);
         end loop;
      end if;
      Object.Pages(Chosen).Items(Offset_Of(Index)) :=
        (Used => True, Key => Key, Previous => Prior, Following => Before, Value => Initial);
      if Prior = 0 then Object.Head := Index;
      else Object.Pages(Page_Of(Prior)).Items(Offset_Of(Prior)).Following := Index; end if;
      if Before = 0 then Object.Tail := Index;
      else Object.Pages(Page_Of(Before)).Items(Offset_Of(Before)).Previous := Index; end if;
      Object.Pages(Chosen).Used := Object.Pages(Chosen).Used + 1;
      Object.Used := Object.Used + 1;
   end Insert;

   procedure Release (Object : in out Table; Index : Valid_Id) is
      P : constant Page_Index := Page_Of (Index);
      Prior, Following : Id;
   begin
      if not Present (Object, Index) then return; end if;
      Prior := Object.Pages(P).Items(Offset_Of(Index)).Previous;
      Following := Next (Object, Index);
      if Prior = 0 then Object.Head := Following;
      else Object.Pages(Page_Of(Prior)).Items(Offset_Of(Prior)).Following := Following; end if;
      if Following = 0 then Object.Tail := Prior;
      else Object.Pages(Page_Of(Following)).Items(Offset_Of(Following)).Previous := Prior; end if;
      Object.Pages(P).Items(Offset_Of(Index)).Used := False;
      Object.Pages(P).Used := Object.Pages(P).Used - 1;
      Object.Used := Object.Used - 1;
      Object.Hint := P;
      if Object.Pages(P).Used = 0 then
         Free_Memory (Block_Bytes, Object.Pages(P).all'Address);
         Object.Pages(P) := null;
      end if;
      if Object.Used = 0 then
         Free_Memory (Directory_Bytes, Object.Pages.all'Address);
         Object.Pages := null;
         Object.Hint := 0;
      end if;
   end Release;
end Owned_Record_Tables;
