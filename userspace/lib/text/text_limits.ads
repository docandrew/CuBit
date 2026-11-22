--  The bounds every unit of the editor core shares (docs/text-editor.md).
--  These are type bounds only: a document's real capacities are chosen by
--  its host, explicitly, when it is created (Text_Documents.Limits).
package Text_Limits with SPARK_Mode, Pure is
   --  The largest document: 1 TiB. Offsets are between bytes, 0 .. length.
   MAXIMUM_BYTES : constant := 2 ** 40;
   type Byte_Count is range 0 .. MAXIMUM_BYTES;
   subtype Byte_Offset is Byte_Count;

   --  Lines: line feeds plus one.
   type Line_Count is range 0 .. MAXIMUM_BYTES + 1;
   subtype Line_Number is Line_Count range 1 .. Line_Count'Last;

   --  The longest piece. Every scan inside one piece is bounded by it; a
   --  load or a paste is cut into pieces of at most this length.
   MAXIMUM_PIECE_BYTES : constant := 16_384;
   type Piece_Length is range 0 .. MAXIMUM_PIECE_BYTES with Size => 16;

   --  A line is laid out in segments of at most this many bytes (moved back
   --  to a code point start), each wrapped on its own: no row's layout ever
   --  depends on more than one segment, however long its line.
   SEGMENT_BYTES : constant := 16_384;
   subtype Segment_Length is Natural range 0 .. SEGMENT_BYTES;
   subtype Segment_Position is Positive range 1 .. SEGMENT_BYTES;

   --  Display columns within one segment's rows (a tab at most TAB_WIDTH,
   --  a wide character two, per cluster).
   MAXIMUM_TAB_WIDTH : constant := 16;
   subtype Tab_Width is Positive range 1 .. MAXIMUM_TAB_WIDTH;
   MAXIMUM_COLUMNS : constant := SEGMENT_BYTES * MAXIMUM_TAB_WIDTH;
   subtype Column_Count is Natural range 0 .. MAXIMUM_COLUMNS;

   --  Work a slice may do (bytes scanned, lines lexed): highlighting,
   --  search and loading stop when it is spent and resume next frame.
   MAXIMUM_WORK : constant := 2 ** 30;
   subtype Work_Budget is Natural range 0 .. MAXIMUM_WORK;
end Text_Limits;
