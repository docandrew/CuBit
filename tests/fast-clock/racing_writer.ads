--  A writer task updating a clock publication while the caller reads it
--  through Clock_Publication.Sample (tests/fast-clock).
with Interfaces; use Interfaces;

package Racing_Writer is
   --  Torn: stable reads whose time mixes two versions (must be zero).
   procedure Run (Torn : out Unsigned_64);
   function Stable_Reads return Natural;
end Racing_Writer;
