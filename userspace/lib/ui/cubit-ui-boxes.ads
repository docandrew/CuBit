------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Layout containers (UI-033): rows, columns and grids of widgets that size
--  themselves from their content, nest, and share out a window's space by
--  resize policy, so an app never places a widget at a pixel rectangle.
--
--  @description
--  An app describes its window as a tree each time it needs the geometry
--  (drawing, hit testing): containers (Begin_Row, Begin_Column, Begin_Grid
--  .. End_Box) and leaves (labels, wrapping labels, buttons, blocks for
--  anything the app draws itself, glue). Every leaf reports a minimum and a
--  preferred size from its content, text measured with the real UI font
--  (CuBit.UI.UI_Text_Width); each axis has a policy, Fixed, Preferred or
--  Expanding, with a stretch factor (Box_Distribution). Containers report
--  their own minimum and preferred size upward. Arrange then lays the tree
--  out in two passes, as Haiku's Layout Kit, Qt and CSS do for
--  height-for-width: widths first (top-down from the window's width), then
--  heights, which a wrapping label knows only once its width is known.
--
--  Spacing and margins come from CuBit.UI_Metrics. Widgets are found by
--  their Id (the app's own numbering, e.g. a control enumeration's 'Pos):
--  Bounds and Hit, never pixel constants, so tests address widgets by
--  identity too.
--
--  Proved (Box_Distribution, level 2): within a box no two children
--  overlap, each gets at least its minimum, a non-expanding child at most
--  its preferred size, and they stay inside the box whenever the minimums
--  fit; otherwise Fits is False and the window's Minimum_Size is what
--  avoids it. Tested (tests/ui-layout): the tree passes, wrapping, grids.
--
--  A tree holds Capacity nodes and Text_Capacity characters of label text,
--  both chosen by the app from its content (Text_Needed helps); adding past
--  either is refused and reported by Overflowed, never a silent cut.
------------------------------------------------------------------------------
with Box_Distribution;
with CuBit.UI_Metrics;

package CuBit.UI.Boxes is
   package BD renames Box_Distribution;
   subtype Extent is BD.Extent;
   subtype Size_Hint is BD.Size_Hint;
   subtype Policy is BD.Policy;
   Fixed     : constant Policy := BD.Fixed;
   Preferred : constant Policy := BD.Preferred;
   Expanding : constant Policy := BD.Expanding;

   --  Named ceilings on one tree.
   MAXIMUM_NODES : constant := 4_096;
   MAXIMUM_TEXT  : constant := 65_536;
   subtype Node_Count is Natural range 0 .. MAXIMUM_NODES;
   subtype Text_Count is Natural range 0 .. MAXIMUM_TEXT;

   --  An app's name for a widget; No_Id names none.
   subtype Widget_Id is Natural;
   No_Id : constant Widget_Id := 0;

   --  Code_Text: the monospace face (Code_Text_Width), e.g. a pairing code.
   type Text_Style is (Body_Text, Muted_Text, Heading_Text, Code_Text);

   --  Where a child sits across its container when it does not fill it.
   type Alignment is (Align_Start, Align_Center, Align_End);

   --  A wrapping label is never narrower than this (or its longest word,
   --  or its whole text if shorter): about a dozen words per line.
   Wrap_Minimum_Width : constant := 12 * CuBit.UI_Metrics.Base_Em;
   --  ... and prefers lines no longer than this (a comfortable measure,
   --  about 60 characters), wider only when the window gives it room.
   Wrap_Preferred_Width : constant := 36 * CuBit.UI_Metrics.Base_Em;
   --  A button: its label with its insets (BUTTON_TEXT_INSET, a tab's
   --  TAB_TEXT_LEFT and TAB_TEXT_INSET) and Space_XS of air each side,
   --  never narrower than Button_Minimum_Width; one control high.
   Button_Minimum_Width : constant := 4 * CuBit.UI_Metrics.Base_Em;

   --  How a container grows along an axis: as its children say (it expands
   --  when one of them does), never past its preferred size (a sidebar of
   --  filling buttons), or always (a page that takes what the window has).
   type Growth is (From_Content, Shrink_Wrap, Fill);

   type Size is record
      Width, Height : Natural := 0;
   end record;

   type Tree (Capacity : Node_Count; Text_Capacity : Text_Count) is private;

   --  Start describing a new layout (an empty tree).
   procedure Clear (T : in out Tree);

   --  Containers. A row lays its children left to right, a column top to
   --  bottom, a grid fills Columns columns row by row. Spacing is between
   --  children (a grid: between columns; Row_Spacing between its rows);
   --  Margin around them all. Align places children that do not fill the
   --  cross axis. Every Begin_* is closed by one End_Box.
   procedure Begin_Row
     (T : in out Tree; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_M; Margin : Extent := 0;
      Align : Alignment := Align_Center;
      Width_Growth, Height_Growth : Growth := From_Content);
   procedure Begin_Column
     (T : in out Tree; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_S; Margin : Extent := 0;
      Align : Alignment := Align_Start;
      Width_Growth, Height_Growth : Growth := From_Content);
   procedure Begin_Grid
     (T : in out Tree; Columns : Positive; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_L;
      Row_Spacing : Extent := CuBit.UI_Metrics.Space_XS; Margin : Extent := 0;
      Width_Growth, Height_Growth : Growth := From_Content);
   procedure End_Box (T : in out Tree);

   --  Leaves.
   --  One line of text, as wide as it measures; never cut.
   procedure Add_Label
     (T : in out Tree; Text : String; Style : Text_Style := Body_Text; Id : Widget_Id := No_Id);
   --  Text that wraps at spaces to the width it is given (a word longer
   --  than a line breaks between characters); as tall as its lines.
   procedure Add_Wrapping_Label
     (T : in out Tree; Text : String; Style : Text_Style := Body_Text; Id : Widget_Id := No_Id);
   --  A push button sized from its label. Fill: as wide as its column
   --  allows (sidebar tabs), otherwise its own width.
   procedure Add_Button
     (T : in out Tree; Label : String; Id : Widget_Id; Fill : Boolean := False);
   --  A tab (Draw_Tab) sized from its label, as wide as its column allows.
   procedure Add_Tab (T : in out Tree; Label : String; Id : Widget_Id);
   --  Anything the app draws itself (a list, a preview, a diagram), with
   --  its own size hints. Scrolls: it scrolls its content (a list, a text
   --  field), so the mouse wheel over it is its (Scroll_Target).
   procedure Add_Block
     (T : in out Tree; Id : Widget_Id; Width, Height : Size_Hint; Scrolls : Boolean := False)
     with Pre => BD.Valid (Width) and then BD.Valid (Height);
   --  Empty space that takes what is left along its container's axis.
   procedure Add_Glue (T : in out Tree; Stretch : BD.Stretch_Factor := 1);
   --  Empty space of one fixed length along both axes.
   procedure Add_Gap (T : in out Tree; Length : Extent);

   --  Characters of label text a tree for these labels needs.
   function Text_Needed (Lengths : Natural) return Text_Count is
     (Natural'Min (Lengths, MAXIMUM_TEXT));

   --  A node or character was refused (capacity), or a Begin_* was not
   --  closed; the layout is then incomplete.
   function Overflowed (T : Tree) return Boolean;

   --  The whole tree's minimum and preferred size (the root's, margins
   --  included). Minimum height is measured at the minimum width, where
   --  text wraps the most; preferred height at the preferred width.
   procedure Measure (T : in out Tree; Minimum, Natural_Size : out Size);

   --  Lay the tree out in Bounds.
   procedure Arrange (T : in out Tree; Bounds : Rect);
   --  Every box's children fitted at their minimums or better.
   function Fits (T : Tree) return Boolean;

   --  The arranged rectangle of the first widget named Id (empty: none).
   function Bounds (T : Tree; Id : Widget_Id) return Rect;
   --  The named leaf (button or block) under X, Y; No_Id when none.
   function Hit (T : Tree; X, Y : Natural) return Widget_Id;
   --  UI-035: the scrolling block under X, Y, which takes the mouse wheel
   --  there (no click into it first); No_Id when none.
   function Scroll_Target (T : Tree; X, Y : Natural) return Widget_Id;

   --  Every named widget and its arranged rectangle, in the order added
   --  (publishing a layout for tests and tools).
   generic
      with procedure Visit (Id : Widget_Id; Box : Rect);
   procedure Visit_Named (T : Tree);

   --  The lines a wrapping label of Text breaks into at Width: each line's
   --  first and last index in Text (exposed for tests and for drawing).
   generic
      with procedure Line (First, Last : Natural);
   procedure Wrap (Text : String; Style : Text_Style; Width : Natural);
   function Text_Width (Text : String; Style : Text_Style) return Natural;
   function Line_Count (Text : String; Style : Text_Style; Width : Natural) return Natural;
   --  The widest word of Text (the least width it wraps to whole words).
   function Longest_Word (Text : String; Style : Text_Style) return Natural;

   --  Draw every label (each wrapped line at its place) and button of the
   --  arranged tree.
   generic
      with procedure Draw_Text (X, Y : Natural; Text : String; Style : Text_Style; Id : Widget_Id);
      with procedure Draw_Button (R : Rect; Label : String; Id : Widget_Id);
   procedure Draw (T : Tree);

private
   subtype Node_Ref is Node_Count;
   No_Node : constant Node_Ref := 0;

   type Node_Kind is (Row_Box, Column_Box, Grid_Box, Glue, Gap, Block, Label, Wrapping_Label, Button);
   --  (A tab is a Button node: drawn by the app through Draw_Button's Id.)
   subtype Container_Kind is Node_Kind range Row_Box .. Grid_Box;

   type Node is record
      Kind : Node_Kind := Block;
      Id : Widget_Id := No_Id;
      Parent, First_Child, Last_Child, Next : Node_Ref := No_Node;
      Children : Node_Count := 0;
      Spacing, Row_Spacing, Margin : Extent := 0;
      Columns : Positive := 1;
      Align : Alignment := Align_Start;
      Width_Growth, Height_Growth : Growth := From_Content;
      Scrolls : Boolean := False;
      Style : Text_Style := Body_Text;
      Text_First : Positive := 1;
      Text_Last : Natural := 0;
      --  Measured hints, and the arranged rectangle.
      Width, Height : Size_Hint;
      Box : Rect;
   end record;
   type Node_Array is array (Node_Ref range <>) of Node;

   type Tree (Capacity : Node_Count; Text_Capacity : Text_Count) is record
      Nodes : Node_Array (1 .. Capacity);
      Used : Node_Count := 0;
      Text : String (1 .. Text_Capacity);
      Text_Used : Text_Count := 0;
      --  The innermost open container.
      Open : Node_Ref := No_Node;
      Refused : Boolean := False;
      Short : Boolean := False;
   end record;
end CuBit.UI.Boxes;
