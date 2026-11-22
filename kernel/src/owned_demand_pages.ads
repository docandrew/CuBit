with Owned_Demand_Policy;

-- Per-reservation state, small enough for one charged metadata frame.
-- Caller serializes all access. Residency is published only after successful
-- frame ownership and PTE installation; this unit never allocates or unmaps.
package Owned_Demand_Pages with SPARK_Mode, Pure is
   use type Owned_Demand_Policy.Access_Mode;
   Maximum_Pages : constant := 4096;
   subtype Page_Count is Natural range 0 .. Maximum_Pages;
   subtype Page_Index is Natural range 0 .. Maximum_Pages - 1;
   subtype Access_Mode is Owned_Demand_Policy.Access_Mode;
   type Map is private;

   function Length (Pages : Map) return Page_Count;
   function Resident_Count (Pages : Map) return Page_Count;
   function Valid_Index (Pages : Map; Index : Page_Index) return Boolean;
   function Mode (Pages : Map; Index : Page_Index) return Access_Mode
     with Pre => Valid_Index (Pages, Index);
   function Resident (Pages : Map; Index : Page_Index) return Boolean
     with Pre => Valid_Index (Pages, Index);

   procedure Initialize
     (Pages : out Map; Count : Page_Count; Initial_Mode : Access_Mode)
     with Post => Length (Pages) = Count and then Resident_Count (Pages) = 0
       and then (for all I in Page_Index =>
         (if Valid_Index (Pages, I) then
           not Resident (Pages, I) and then Mode (Pages, I) = Initial_Mode));

   -- Number of resident nodes preceding Index in descending virtual order.
   -- This identifies insertion position in the owned frame-list segment.
   function Preceding (Pages : Map; Index : Page_Index) return Page_Count
     with Pre => Valid_Index (Pages, Index),
          Post => Preceding'Result <= Maximum_Pages - 1 - Index;

   procedure Commit (Pages : in out Map; Index : Page_Index)
     with Pre => Valid_Index (Pages, Index) and then not Resident (Pages, Index)
                 and then Resident_Count (Pages) < Length (Pages),
          Post => Resident (Pages, Index)
            and then Resident_Count (Pages) = Resident_Count (Pages'Old) + 1
            and then Length (Pages) = Length (Pages'Old)
            and then Mode (Pages, Index) = Mode (Pages'Old, Index);

   -- Publish removal only after the caller has unmapped the page, completed
   -- TLB invalidation, and safely retired its frame-list node. Permissions
   -- remain intact so a later permitted fault can install zero-filled backing.
   procedure Discard (Pages : in out Map; Index : Page_Index)
     with Pre => Valid_Index (Pages, Index) and then Resident (Pages, Index)
                 and then Resident_Count (Pages) > 0,
          Post => Resident_Count (Pages) = Resident_Count (Pages'Old) - 1
            and then Length (Pages) = Length (Pages'Old)
            and then (for all I in Page_Index =>
              (if Valid_Index (Pages, I) then
                 Mode (Pages, I) = Mode (Pages'Old, I)
                 and then Resident (Pages, I) =
                   (if I = Index then False else Resident (Pages'Old, I))));

   function Valid_Range
     (Pages : Map; First : Page_Index; Count : Page_Count) return Boolean;
   procedure Set_Mode
     (Pages : in out Map; First : Page_Index; Count : Page_Count;
      New_Mode : Access_Mode)
     with Pre => Valid_Range (Pages, First, Count),
          Post => Resident_Count (Pages) = Resident_Count (Pages'Old)
            and then Length (Pages) = Length (Pages'Old)
            and then (for all I in Page_Index =>
              (if Valid_Index (Pages, I) then
                 Resident (Pages, I) = Resident (Pages'Old, I)
                 and then Mode (Pages, I) =
                   (if I >= First and then I - First < Count then New_Mode
                    else Mode (Pages'Old, I))));
private
   type Modes is array (Page_Index) of Access_Mode
     with Component_Size => 2;
   type Bits is array (Page_Index) of Boolean with Component_Size => 1;
   type Map is record
      Size : Page_Count := 0;
      Backed : Page_Count := 0;
      Permissions : Modes := (others => Owned_Demand_Policy.Guard);
      Present : Bits := (others => False);
   end record;
   pragma Compile_Time_Error (Map'Size > 4096 * 8,
                              "demand metadata exceeds one frame");
end Owned_Demand_Pages;
