with Interfaces;
with CuBit.QOI;
with Desktop_Icons;
--  The application icon atlas (docs/assets.md): Assets/cubit-icons/<version>/
--  applications.qoi, every Desktop_Icons.Icon_ID at every exported edge
--  (Desktop_Icons.Edges: one column per edge, each icon's square stacked top
--  to bottom in Icon_ID order), straight-alpha 16#AARRGGBB#. Its pixels live
--  in one CuAlloc block (Desktop_Icon_Store.Atlas_Memory), filled once at startup
--  by the streaming QOI decoder and then read-only. Until it is Loaded (and
--  for good if it is missing, malformed, the wrong size or the heap is out)
--  every icon is drawn as the built-in Fallback glyph, never as blank
--  pixels. No I/O here: the loader (Desktop_Icon_Assets) feeds the bytes.
package Desktop_Icon_Store with SPARK_Mode, Abstract_State => State,
  Initializes => State
is
   package Q renames CuBit.QOI;
   use type Interfaces.Unsigned_32;

   subtype Edge_Index is Desktop_Icons.Edge_Index;
   Atlas_Width  : constant := Desktop_Icons.ATLAS_WIDTH;
   Atlas_Height : constant := Desktop_Icons.ATLAS_HEIGHT;
   Atlas_Pixels : constant := Atlas_Width * Atlas_Height;
   pragma Compile_Time_Error (Atlas_Pixels > Q.Maximum_Pixels, "icon atlas exceeds the QOI decoder's limit");
   subtype Coordinate is Natural range 0 .. Desktop_Icons.MAX_EDGE - 1;
   subtype Edge_Pixels is Positive range 1 .. Desktop_Icons.MAX_EDGE;

   --  Every edge's column fits the atlas, and every icon's square at that
   --  edge fits the column: the layout the generator wrote.
   function Layout_Fits return Boolean is
     (for all E in Edge_Index =>
        Desktop_Icons.Edges (E) <= Desktop_Icons.MAX_EDGE and then
        Desktop_Icons.Columns (E) + Desktop_Icons.Edges (E) <= Atlas_Width);
   pragma Assert (Layout_Fits);

   type Status is (Not_Loaded, Loading, Loaded, Unavailable);
   function Current return Status with Global => State;

   --  The built-in "icon unavailable" glyph at Edge pixels: a light square
   --  with a dark frame, crossed in red. Computed, not stored.
   Glyph_Clear  : constant := 16#0000_0000#;
   Glyph_Frame  : constant := 16#FF5A_5A5A#;
   Glyph_Fill   : constant := 16#FFF2_F2F2#;
   Glyph_Cross  : constant := 16#FFC0_392B#;
   Glyph_Margin : constant := 2;
   function Fallback (Edge : Edge_Pixels; X, Y : Coordinate) return Interfaces.Unsigned_32 is
     (if X < Glyph_Margin or else Y < Glyph_Margin or else
         X + Glyph_Margin >= Edge or else Y + Glyph_Margin >= Edge
      then Glyph_Clear
      elsif X = Glyph_Margin or else Y = Glyph_Margin or else
            X + Glyph_Margin + 1 = Edge or else Y + Glyph_Margin + 1 = Edge
      then Glyph_Frame
      elsif X = Y or else X + Y + 1 = Edge then Glyph_Cross
      else Glyph_Fill);

   --  Icon's pixel at (X, Y) of its Edge-pixel square: from the atlas once
   --  Loaded, else Fallback.
   function Pixel (Icon : Desktop_Icons.Icon_ID; Edge : Edge_Index; X, Y : Coordinate)
     return Interfaces.Unsigned_32
     with Global => State,
          Pre  => X < Desktop_Icons.Edges (Edge) and then Y < Desktop_Icons.Edges (Edge),
          Post => (if Current /= Loaded then
                     Pixel'Result = Fallback (Desktop_Icons.Edges (Edge), X, Y));

   type Load_Result is record
      Loaded : Boolean := False;
      Error : Q.Failure := Q.No_Failure;   --  the decoder's, if it failed
      Wrong_Size : Boolean := False;       --  a valid image of another size
      No_Memory : Boolean := False;        --  the heap had no room for it
      Width, Height : Natural := 0;        --  as the file declared them
   end record;

   --  Reserves the atlas's memory; without it the store is Unavailable.
   procedure Begin_Load
     with Global => (In_Out => State),
          Pre  => Current = Not_Loaded,
          Post => Current in Loading | Unavailable;
   --  More of the file. Wants_More turns False once the stream is rejected.
   procedure Feed (Input : Q.Byte_Array)
     with Global => (In_Out => State),
          Pre  => Current = Loading,
          Post => Current = Loading;
   function Wants_More return Boolean
     with Global => State, Pre => Current = Loading;
   --  The file has ended (or reading it failed: Read_Failed). Loaded only
   --  if exactly one complete atlas of Atlas_Width x Atlas_Height was decoded.
   --  Unavailable (No_Memory) if Begin_Load found no memory.
   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result)
     with Global => (In_Out => State),
          Pre  => Current in Loading | Unavailable,
          Post => Current = (if Result.Loaded then Loaded else Unavailable);
end Desktop_Icon_Store;
