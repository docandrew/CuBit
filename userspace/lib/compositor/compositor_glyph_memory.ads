with System;
with Interfaces;
with Compositor_Glyph_Arena;
-- Audited pointer bridge for caller-owned, fixed backing. This limited object
-- must stay at a stable address and outlive every returned pointer/import.
-- All calls are serialized by its owner. No allocation, growth or compaction.
package Compositor_Glyph_Memory with SPARK_Mode => Off is
   package Arena is new Compositor_Glyph_Arena;
   type State is limited private;
   procedure Reserve
     (S : in out State; Size : Arena.Request_Bytes; T : out Arena.Token;
      Pixels : out System.Address; Capacity : out Natural);
   function Address_Of (S : in out State; T : Arena.Token) return System.Address;
   procedure Release
     (S : in out State; T : Arena.Token; Readers_Retired : Boolean; Released : out Boolean);
private
   type Bytes is array (Natural range 0 .. Arena.Backing_Bytes - 1) of aliased Interfaces.Unsigned_8
     with Alignment => Arena.Cell_Bytes;
   type State is limited record
      Data : Bytes := (others => 0);
      Policy : Arena.State;
      Started : Boolean := False;
   end record with Alignment => Arena.Cell_Bytes;
end Compositor_Glyph_Memory;
