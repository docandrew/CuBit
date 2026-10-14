with Ada.Unchecked_Conversion;

package body Fake_Pages with SPARK_Mode => Off is
   Page_Bytes : constant := 4096;
   type Page_Block is array (1 .. Page_Bytes) of Unsigned_8 with
     Alignment => Page_Bytes;
   type Page_Ref is access Page_Block;
   function To_Address is new Ada.Unchecked_Conversion (Page_Ref, System.Address);

   -- Pages currently held, unordered (swap-remove on release). Released
   -- blocks are poisoned and never handed out again, so a use after
   -- release reads poison instead of another table's live data.
   Max_Live : constant := 100_000;
   type Live_Index is range 1 .. Max_Live;
   type Page_Record is record
      Page  : Page_Ref := null;
      Owner : Owner_Id := 0;
   end record;
   Live      : array (Live_Index) of Page_Record;
   Live_Used : Natural := 0;

   Max_Owners : constant := 64;
   Per_Owner  : array (Owner_Id range 0 .. Max_Owners - 1) of Page_Total :=
     [others => 0];

   Budget_On  : Boolean := False;
   Budget     : Page_Total := 0;
   Alloc_N, Release_N, Refuse_N, Bad_N : Page_Total := 0;

   procedure Allocate_Page
     (Owner : Owner_Id; Page : out System.Address; OK : out Boolean) is
      P : Page_Ref;
   begin
      Page := System.Null_Address;
      OK := False;
      if Owner >= Max_Owners then
         raise Program_Error with "fake pages: owner out of range";
      end if;
      if Budget_On then
         if Budget = 0 then
            Refuse_N := Refuse_N + 1;
            return;
         end if;
         Budget := Budget - 1;
      end if;
      P := new Page_Block'(others => 16#A5#);  -- garbage, not zero
      Live_Used := Live_Used + 1;
      Live (Live_Index (Live_Used)) := (Page => P, Owner => Owner);
      Per_Owner (Owner) := Per_Owner (Owner) + 1;
      Alloc_N := Alloc_N + 1;
      Page := To_Address (P);
      OK := True;
   end Allocate_Page;

   procedure Release_Page (Page : System.Address) is
      use type System.Address;
   begin
      for I in 1 .. Live_Index'Base (Live_Used) loop
         if To_Address (Live (I).Page) = Page then
            Live (I).Page.all := [others => 16#5A#];  -- poison
            Per_Owner (Live (I).Owner) := Per_Owner (Live (I).Owner) - 1;
            Live (I) := Live (Live_Index (Live_Used));
            Live_Used := Live_Used - 1;
            Release_N := Release_N + 1;
            return;
         end if;
      end loop;
      Bad_N := Bad_N + 1;
   end Release_Page;

   procedure Reset is
   begin
      Budget_On := False;
      Budget := 0;
   end Reset;

   procedure Set_Budget (Pages : Page_Total) is
   begin
      Budget_On := True;
      Budget := Pages;
   end Set_Budget;

   procedure Clear_Budget is
   begin
      Budget_On := False;
   end Clear_Budget;

   function Held (Owner : Owner_Id) return Page_Total is
     (if Owner < Max_Owners then Per_Owner (Owner) else 0);
   function Held_Total return Page_Total is (Page_Total (Live_Used));
   function Allocations return Page_Total is (Alloc_N);
   function Releases return Page_Total is (Release_N);
   function Refusals return Page_Total is (Refuse_N);
   function Bad_Releases return Page_Total is (Bad_N);
end Fake_Pages;
