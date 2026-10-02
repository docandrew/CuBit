with Compositor_Identity;
with Heap_Extents;
with Compositor_Glyph_Layout;
-- Reuse the allocator's proved contiguous-run ownership. Its page identifiers
-- are abstract cells here: no mapping or Heap_Extents.Page_Bytes arithmetic is
-- used. The actual backing is 4096 cells of 128 bytes, not a 16 MiB heap.
generic
   Last_Identity : Compositor_Identity.Positive_Value := Compositor_Identity.Positive_Value'Last;
package Compositor_Glyph_Arena with SPARK_Mode, Pure is
   subtype Serial_Number is Compositor_Identity.Value;
   use type Serial_Number;
   package H renames Heap_Extents;
   Cell_Bytes : constant := 128;
   Backing_Bytes : constant := H.Page_Count * Cell_Bytes;
   subtype Request_Bytes is Positive range 1 .. Compositor_Glyph_Layout.Maximum_Bytes;
   type Token is private;
   No_Token : constant Token;
   type State is private;
   function Valid (S : State) return Boolean with Ghost;
   function Current (S : State; T : Token) return Boolean;
   function Sequence (S : State) return Serial_Number;
   function Offset (T : Token) return Natural
     with Post => Offset'Result < Backing_Bytes and Offset'Result mod Cell_Bytes = 0;
   function Capacity (T : Token) return Positive;
   function Fits (T : Token) return Boolean;
   function Occupied (S : State; Cell : H.Page_Id) return Boolean;
   function Belongs (T : Token; Cell : H.Page_Id) return Boolean;
   -- One fresh backing owner. Never reset while callbacks/tokens can survive.
   procedure Initialize (S : out State)
     with Post => Valid (S) and Sequence (S) = 0 and
       (for all P in H.Page_Id => not Occupied (S, P));
   procedure Reserve (S : in out State; Size : Request_Bytes; T : out Token)
     with Pre => Valid (S), Post => Valid (S) and
       (if T = No_Token then S = S'Old else
         Current (S, T) and Fits (T) and
         Capacity (T) >= Size and Capacity (T) - Size < Cell_Bytes and
         Offset (T) + Capacity (T) <= Backing_Bytes and
         Sequence (S) = Sequence (S'Old) + 1 and
         (for all P in H.Page_Id =>
           (if Belongs (T, P) then not Occupied (S'Old, P) and Occupied (S, P)
            else Occupied (S, P) = Occupied (S'Old, P))));
   -- Must follow successful cache Begin_Retirement and confirmed foreign
   -- release. An unknown outcome leaves the whole run unavailable for reuse.
   procedure Release (S : in out State; T : Token; Readers_Retired : Boolean;
                      Released : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Sequence (S) = Sequence (S'Old) and
       Released = (Current (S'Old, T) and Readers_Retired) and
       (if not Released then S = S'Old else not Current (S, T) and
         (for all P in H.Page_Id => Occupied (S, P) =
           (Occupied (S'Old, P) and not Belongs (T, P))));
private
   type Token is record
      First : H.Page_Id := H.Page_Id'First;
      Cells : H.Run_Length := 1;
      Identity : Serial_Number := 0;
   end record;
   No_Token : constant Token := (H.Page_Id'First, 1, 0);
   type Identities is array (H.Page_Id) of Serial_Number;
   type State is record
      Runs : H.State;
      Serial : Serial_Number range 0 .. Last_Identity := 0;
      Ids : Identities := (others => 0);
   end record;
   function Valid (S : State) return Boolean is
     (H.Valid (S.Runs) and then (for all P in H.Page_Id =>
       S.Ids (P) <= S.Serial and then (if H.Length (S.Runs, P) > 0 then S.Ids (P) > 0)));
   function Current (S : State; T : Token) return Boolean is
     (T.Identity > 0 and then S.Ids (T.First) = T.Identity and then H.Length (S.Runs, T.First) = T.Cells);
   function Sequence (S : State) return Serial_Number is (S.Serial);
   function Offset (T : Token) return Natural is ((T.First - 1) * Cell_Bytes);
   function Capacity (T : Token) return Positive is (T.Cells * Cell_Bytes);
   function Fits (T : Token) return Boolean is (H.Fits (T.First, T.Cells));
   function Occupied (S : State; Cell : H.Page_Id) return Boolean is (H.Owner (S.Runs, Cell) /= H.No_Page);
   function Belongs (T : Token; Cell : H.Page_Id) return Boolean is
     (Cell >= T.First and then Cell - T.First < T.Cells);
end Compositor_Glyph_Arena;
