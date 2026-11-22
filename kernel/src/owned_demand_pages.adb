package body Owned_Demand_Pages with SPARK_Mode is
   function Length (Pages : Map) return Page_Count is (Pages.Size);
   function Resident_Count (Pages : Map) return Page_Count is (Pages.Backed);
   function Valid_Index (Pages : Map; Index : Page_Index) return Boolean is
     (Index < Pages.Size);
   function Mode (Pages : Map; Index : Page_Index) return Access_Mode is
     (Pages.Permissions (Index));
   function Resident (Pages : Map; Index : Page_Index) return Boolean is
     (Pages.Present (Index));
   procedure Initialize
     (Pages : out Map; Count : Page_Count; Initial_Mode : Access_Mode) is
   begin
      Pages := (Size => Count, Backed => 0,
                Permissions => (others => Initial_Mode),
                Present => (others => False));
   end Initialize;
   function Preceding (Pages : Map; Index : Page_Index) return Page_Count is
      Result : Page_Count := 0;
   begin
      for I in Index + 1 .. Maximum_Pages - 1 loop
         pragma Loop_Invariant (Result <= I - Index - 1);
         if Pages.Present (I) then Result := Result + 1; end if;
      end loop;
      return Result;
   end Preceding;
   procedure Commit (Pages : in out Map; Index : Page_Index) is
   begin
      Pages.Present (Index) := True;
      Pages.Backed := Pages.Backed + 1;
   end Commit;
   procedure Discard (Pages : in out Map; Index : Page_Index) is
   begin
      Pages.Present (Index) := False;
      Pages.Backed := Pages.Backed - 1;
   end Discard;
   function Valid_Range
     (Pages : Map; First : Page_Index; Count : Page_Count) return Boolean is
     (Count > 0 and then First < Pages.Size
      and then Count <= Pages.Size - First);
   procedure Set_Mode
     (Pages : in out Map; First : Page_Index; Count : Page_Count;
      New_Mode : Access_Mode) is
   begin
      for I in First .. First + Count - 1 loop
         Pages.Permissions (I) := New_Mode;
         pragma Loop_Invariant
           (for all J in Page_Index =>
              Pages.Permissions (J) =
                (if J >= First and then J <= I then New_Mode
                 else Pages.Permissions'Loop_Entry (J)));
      end loop;
   end Set_Mode;
end Owned_Demand_Pages;
