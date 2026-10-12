with Interfaces;
with CuBit.QOI;
with Desktop_Icons;
--  The application icon atlas (docs/assets.md): Assets/cubit-icons/<version>/
--  applications.qoi, every Desktop_Icons.Icon_ID's ICON_SIZE square stacked
--  top to bottom in Icon_ID order, straight-alpha 16#AARRGGBB#. Filled once
--  at startup by the streaming QOI decoder and then read-only. Until it is
--  Loaded (and for good if it is missing, malformed or the wrong size) every
--  icon is drawn as the built-in Fallback glyph, never as blank pixels. No
--  I/O here: the loader (Desktop_Icon_Assets) feeds the bytes it read.
package Desktop_Icon_Store with SPARK_Mode, Abstract_State => State,
  Initializes => State
is
   package Q renames CuBit.QOI;
   use type Interfaces.Unsigned_32;

   Icon_Size    : constant := Desktop_Icons.ICON_SIZE;
   Icon_Count   : constant := Desktop_Icons.Icon_ID'Range_Length;
   Atlas_Width  : constant := Icon_Size;
   Atlas_Height : constant := Icon_Count * Icon_Size;
   Atlas_Pixels : constant := Atlas_Width * Atlas_Height;
   subtype Coordinate is Natural range 0 .. Icon_Size - 1;

   type Status is (Not_Loaded, Loading, Loaded, Unavailable);
   function Current return Status with Global => State;

   --  The built-in "icon unavailable" glyph: a light square with a dark
   --  frame, crossed in red. Computed, not stored.
   Glyph_Clear  : constant := 16#0000_0000#;
   Glyph_Frame  : constant := 16#FF5A_5A5A#;
   Glyph_Fill   : constant := 16#FFF2_F2F2#;
   Glyph_Cross  : constant := 16#FFC0_392B#;
   Glyph_Margin : constant := 2;
   function Fallback (X, Y : Coordinate) return Interfaces.Unsigned_32 is
     (if X < Glyph_Margin or else Y < Glyph_Margin or else
         X > Icon_Size - 1 - Glyph_Margin or else Y > Icon_Size - 1 - Glyph_Margin
      then Glyph_Clear
      elsif X = Glyph_Margin or else Y = Glyph_Margin or else
            X = Icon_Size - 1 - Glyph_Margin or else Y = Icon_Size - 1 - Glyph_Margin
      then Glyph_Frame
      elsif X = Y or else X + Y = Icon_Size - 1 then Glyph_Cross
      else Glyph_Fill);

   --  Icon's pixel at (X, Y): from the atlas once Loaded, else Fallback.
   function Pixel (Icon : Desktop_Icons.Icon_ID; X, Y : Coordinate)
     return Interfaces.Unsigned_32
     with Global => State,
          Post => (if Current /= Loaded then Pixel'Result = Fallback (X, Y));

   type Load_Result is record
      Loaded : Boolean := False;
      Error : Q.Failure := Q.No_Failure;   --  the decoder's, if it failed
      Wrong_Size : Boolean := False;       --  a valid image of another size
      Width, Height : Natural := 0;        --  as the file declared them
   end record;

   procedure Begin_Load
     with Global => (In_Out => State),
          Pre  => Current = Not_Loaded,
          Post => Current = Loading;
   --  More of the file. Wants_More turns False once the stream is rejected.
   procedure Feed (Input : Q.Byte_Array)
     with Global => (In_Out => State),
          Pre  => Current = Loading,
          Post => Current = Loading;
   function Wants_More return Boolean
     with Global => State, Pre => Current = Loading;
   --  The file has ended (or reading it failed: Read_Failed). Loaded only
   --  if exactly one complete atlas of Atlas_Width x Atlas_Height was decoded.
   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result)
     with Global => (In_Out => State),
          Pre  => Current = Loading,
          Post => Current = (if Result.Loaded then Loaded else Unavailable);
end Desktop_Icon_Store;
