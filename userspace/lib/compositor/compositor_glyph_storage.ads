with System;
with Compositor_Glyph_Layout;
with Compositor_Glyph_Memory;
with Compositor_Glyph_Software;
package Compositor_Glyph_Storage with SPARK_Mode is
   package L renames Compositor_Glyph_Layout;
   package Software renames Compositor_Glyph_Software;
   use type Software.Word;
   subtype Slot is Positive range 1 .. 128;
   type State is limited private with Default_Initial_Condition => Empty (State);
   type Flags is array (Slot) of Boolean;
   function Same_Others (Before, After : Flags; I : Slot) return Boolean is
     (for all J in Slot => (if J /= I then Before (J) = After (J)));
   function Occupied (S : State) return Flags with Global => null;
   function Has (S : State; I : Slot) return Boolean with Global => null,
     Post => Has'Result = Occupied (S) (I);
   function Empty (S : State) return Boolean is (for all I in Slot => not Has (S, I));
   function Pixels (S : State; I : Slot) return System.Address with Global => null;
   function Capacity (S : State; I : Slot) return Natural with Global => null;
   procedure Allocate (S : in out State; I : Slot; Layout : L.Layout; Success : out Boolean)
     with Pre => not Has (S, I) and L.Valid (Layout),
       Post => Has (S, I) = Success and
         (if Success then Capacity (S, I) >= Layout.Bytes) and
         Same_Others (Occupied (S)'Old, Occupied (S), I);
   procedure Rasterize (S : in out State; I : Slot; Face, Code : Natural;
                        Layout : L.Layout; Advance : out Natural; Success : out Boolean)
     with Pre => Has (S, I) and L.Valid (Layout) and Layout.Bytes <= Capacity (S, I),
       Post => Occupied (S) = Occupied (S)'Old;
   function Can_Paint (S : State; I : Slot; Screen : L.G.Output) return Boolean with Global => null;
   -- Caller holds a read lease and has retired every foreign reader. Target is
   -- a distinct caller-owned mapping; the bridge additionally rejects virtual
   -- overlap and stale backing tokens before exposing the A8 array.
   procedure Paint (S : in out State; I : Slot; Screen : L.G.Output;
                    Origin : L.G.Logical_Point; Damage : L.G.Physical_Rectangle;
                    Target : in out Software.Pixels; Pitch : Positive; Tint : Software.Word;
                    Success : out Boolean)
     with Pre => Software.Fits_Target (Screen, Target, Pitch),
       Post => Occupied (S) = Occupied (S)'Old and
         (for all J in Target'Range =>
           (if not Success or else not Software.Inside (J, Pitch, Software.Bounds (Screen, Origin, Damage)) then
              Target (J) = Target'Old (J)));
   -- Caller has already retired every Mesa view and read lease for this slot.
   procedure Release (S : in out State; I : Slot; Success : out Boolean)
     with Pre => Has (S, I),
       Post => (Success = not Has (S, I)) and
         Same_Others (Occupied (S)'Old, Occupied (S), I);
private
   pragma SPARK_Mode (Off);
   package M renames Compositor_Glyph_Memory;
   type Item is record
      Token : M.Arena.Token := M.Arena.No_Token;
      Address : System.Address := System.Null_Address;
      Size : Natural := 0;
      Raster : L.Layout := L.Plan ((1, 1));
   end record;
   type Items is array (Slot) of Item;
   type State is limited record
      Memory : M.State;
      Backing : Items;
   end record;
end Compositor_Glyph_Storage;
