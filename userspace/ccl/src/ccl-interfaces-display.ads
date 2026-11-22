with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The display interface (REMOTE-001 "Remote screenshots"): a read-only
--  copy of the composed frame as an Image (CCL.Interfaces.Images), at the
--  output's native pixels: (display.capture) takes output 0 whole;
--  (display.capture-area (Capture_Area output x y width height)) takes an
--  output, or a rectangle of it (width and height 0: the whole output).
--  Only a host holding Desktop's display-capture capability answers it
--  (ccl-remote), and Desktop copies a frame only while the person at the
--  device allows screen capture. The types are CCL source (TYPE_SOURCE);
--  the keys are SHA-256 of it, then "#" and the type's name
--  (tests/ccl-console/check_interface_keys.py checks them).
package CCL.Interfaces.Display with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 1;
   PUBLISHED_TYPES : constant := 1;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Capture_Area (record (output Integer) (x Integer) (y Integer) (width Integer) " &
     "(height Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#0F98_CDAC_1D43_132F#,
      16#CB83_8BA7_FF76_5F5B#,
      16#376B_6BDB_4E5D_7879#,
      16#DDA6_5711_930F_0CE4#];
   AREA_KEY : constant CCL.Objects.Schema_Key :=
     [16#D76A_5F54_3DDE_CB25#,
      16#8A50_D15D_C644_9568#,
      16#DAB2_A10C_14D2_CC1B#,
      16#A83A_E0CD_A83B_10B6#];

   type Operation is (Capture, Capture_Area);
   function Name (Item : Operation) return String is
     (case Item is when Capture => "capture", when Capture_Area => "capture-area");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Display);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));
   AREA_FIELDS : constant := 5;

   --  Capture_Area in Types, bound to its key.
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Area : out CCL.Objects.Binding; Accepted : out Boolean);

   --  The schema and the display interface, for discovery; the image
   --  interface must be published first (the result is its Image).
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Display;
