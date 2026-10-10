with Compositor_Source_Region;
with Desktop_Cursors;
-- Vertical packing of the existing immutable icon tables. No CPU atlas is
-- allocated: rows are written straight into the admitted staging mapping.
package Desktop_Icon_Pixels.Atlases with SPARK_Mode is
   package R renames Compositor_Source_Region;
   function Width (Kind : Family) return Positive is
     (if Kind = Application then Desktop_Icons.ICON_SIZE else Desktop_Cursors.MAX_WIDTH);
   function Height (Kind : Family) return Positive is
     (if Kind = Application then Desktop_Icons.Icon_ID'Range_Length * Desktop_Icons.ICON_SIZE else 161);
   -- The second atlas holds straight-alpha controls and premultiplied cursors.
   -- Blend mode belongs to each draw, not the texture. Cursor metadata/hotspots
   -- remain in Desktop_Cursors; this only selects immutable source pixels.
   function Cursor_Row (Item : Desktop_Cursors.Cursor_ID) return Natural is
     (case Item is when Desktop_Cursors.Arrow => 45, when Desktop_Cursors.Text => 73,
       when Desktop_Cursors.Horizontal_Resize => 97, when Desktop_Cursors.Vertical_Resize => 115,
       when Desktop_Cursors.Diagonal_Resize => 140);
   function Cursor_Region (Item : Desktop_Cursors.Cursor_ID) return R.Rectangle
     with Post => R.Valid (Cursor_Region'Result);
   function Region (Item : Asset) return R.Rectangle with Post => R.Valid (Region'Result);
   function Pixel (Kind : Family; X, Y : Natural) return Interfaces.Unsigned_32
     with Pre => X < Width (Kind) and Y < Height (Kind);
   procedure Copy_Chunk (Kind : Family; Target : in out Pixels;
      Plan : U.Plan; Complete : out Boolean)
     with Pre => Target'First = 0 and Target'Last < U.Byte_Count'Last / 4,
       Post => (if not Complete then Target = Target'Old);
private
   function Cursor_Value (Item : Desktop_Cursors.Cursor_ID; X, Y : Natural)
     return Interfaces.Unsigned_32
     with Pre => X < Desktop_Cursors.Metadata (Item).Width and
       Y < Desktop_Cursors.Metadata (Item).Height;
end Desktop_Icon_Pixels.Atlases;
