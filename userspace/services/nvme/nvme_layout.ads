------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How nvme.drv divides its DMA region (manifest request "queues", 1 MiB at
--  DMA_Virtual_Base), and how many I/O commands it keeps in flight.
--
--  Page 0:      admin SQ (16 x 64 B)
--  Page 1:      admin CQ (16 x 16 B)
--  Page 2:      I/O SQ (16 x 64 B)
--  Page 3:      I/O CQ (16 x 16 B)
--  Page 4:      Identify / scratch buffer
--  Page 5:      PRP lists, one per in-flight command
--  Pages 6-255: data buffers, one Chunk_Bytes slice per in-flight command
------------------------------------------------------------------------------
with NVMe_Queue_Cursors;

package NVMe_Layout with SPARK_Mode, Pure is

   DMA_Virtual_Base : constant := 16#0000_7000_0000_0000#;
   Page_Bytes       : constant := 4096;
   Region_Pages     : constant := 256;

   Admin_SQ_Offset  : constant := 0 * Page_Bytes;
   Admin_CQ_Offset  : constant := 1 * Page_Bytes;
   IO_SQ_Offset     : constant := 2 * Page_Bytes;
   IO_CQ_Offset     : constant := 3 * Page_Bytes;
   Identify_Offset  : constant := 4 * Page_Bytes;
   PRP_List_Offset  : constant := 5 * Page_Bytes;
   Data_Offset      : constant := 6 * Page_Bytes;
   Data_Pages       : constant := Region_Pages - Data_Offset / Page_Bytes;
   --  Queue memory (pages 0-3) and the PRP list page: zeroed on reset.
   Queue_Pages      : constant := 4;

   --  A transfer is split into chunks of at most this many bytes, each in
   --  its own slice of the data area with its own PRP list.
   Chunk_Bytes : constant := 128 * 1024;
   Chunk_Pages : constant := Chunk_Bytes / Page_Bytes;

   --  Commands in flight at once: one per data slice.
   Flight_Slots : constant := (Data_Pages * Page_Bytes) / Chunk_Bytes;
   subtype Flight_Slot is Natural range 0 .. Flight_Slots - 1;

   --  One PRP list per in-flight slot, packed into the PRP list page.
   PRP_Entry_Bytes : constant := 8;
   PRP_List_Bytes  : constant := Chunk_Pages * PRP_Entry_Bytes;

   pragma Compile_Time_Error
     (Flight_Slots < 1, "the data area must hold one chunk");
   --  The I/O SQ can never fill: one entry stays empty (3.3.1.5).
   pragma Compile_Time_Error
     (Flight_Slots >= NVMe_Queue_Cursors.Queue_Depth - 1,
      "in-flight commands must fit the I/O queue");
   pragma Compile_Time_Error
     (Flight_Slots * PRP_List_Bytes > Page_Bytes, "PRP lists overflow their page");
   --  Submission entries are 64 bytes (CC.IOSQES = 6), completions 16.
   Submission_Entry_Bytes : constant := 64;
   pragma Compile_Time_Error
     (NVMe_Queue_Cursors.Queue_Depth * Submission_Entry_Bytes > Page_Bytes,
      "a queue overflows its page");

   function Slot_Data_Offset (Slot : Flight_Slot) return Natural is
     (Data_Offset + Slot * Chunk_Bytes);

   function Slot_PRP_List_Offset (Slot : Flight_Slot) return Natural is
     (PRP_List_Offset + Slot * PRP_List_Bytes);

end NVMe_Layout;
