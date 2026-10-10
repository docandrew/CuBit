--  The bounds every Files unit shares (docs/files-app.md). These are type
--  bounds only: a pane's real capacity is chosen at startup, explicitly, and
--  is at most these.
package Files_Limits with SPARK_Mode, Pure is
   --  The most entries one listing can hold.
   ABSOLUTE_MAXIMUM_ENTRIES : constant := 16_777_216;
   subtype Entry_Count is Natural range 0 .. ABSOLUTE_MAXIMUM_ENTRIES;
   --  An entry's identity within its listing: its arrival slot, stable for
   --  the life of the listing (sorting, filtering and marking never move
   --  it).
   subtype Entry_Id is Entry_Count range 1 .. ABSOLUTE_MAXIMUM_ENTRIES;
   subtype Entry_Capacity is Entry_Id;

   --  One name component (ext2's EXT2_NAME_LEN, the protocol's limit).
   MAXIMUM_NAME_BYTES : constant := 255;
   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME_BYTES;
   subtype Name_Position is Name_Length range 1 .. MAXIMUM_NAME_BYTES;

   --  The bytes of every name in one listing.
   ABSOLUTE_MAXIMUM_ARENA : constant := 1_073_741_824;
   subtype Arena_Count is Natural range 0 .. ABSOLUTE_MAXIMUM_ARENA;
   subtype Arena_Index is Arena_Count range 1 .. ABSOLUTE_MAXIMUM_ARENA;
   subtype Arena_Capacity is Arena_Index;

   --  Rows a pane draws: its shown entries and, below a root, the ".." row.
   subtype Row_Count is Natural range 0 .. ABSOLUTE_MAXIMUM_ENTRIES + 1;
   subtype Row_Index is Row_Count range 1 .. Row_Count'Last;
   --  A cursor or scroll move by this many rows; negative goes toward row 1.
   subtype Row_Delta is Integer range -Row_Count'Last .. Row_Count'Last;

   --  Work a slice may do (element moves, compares or scans): incremental
   --  sorting and filtering stop when it is spent and resume next frame.
   MAXIMUM_WORK : constant := 1_073_741_824;
   subtype Work_Budget is Natural range 0 .. MAXIMUM_WORK;
end Files_Limits;
