------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How the UFS driver divides its one physically contiguous DMA region
--  (manifest request "queues"): the UTP transfer request list, the UTP
--  task-management list, one command table per transfer slot, and one
--  bounce buffer per slot. The controller never sees client memory: block
--  data is copied between a slot's buffer and the client's grant.
------------------------------------------------------------------------------
with UFSHCI;
with UFS_UPIU;

package UFS_Layout with SPARK_Mode, Pure is
   --  The manifest's dma request; devmgr maps exactly this much.
   Region_Bytes : constant := 1024 * 1024;

   --  Transfer slots this driver uses (the controller may offer more).
   Driver_Slots : constant := 8;
   subtype Slot is Natural range 0 .. Driver_Slots - 1;
   Task_Slots : constant := 1;

   --  UTRLBA and UTMRLBA must be 1 KiB aligned (UFSHCI 5.4.1, 5.5.1).
   List_Alignment : constant := 1024;
   Transfer_Descriptor_Bytes : constant := 32;
   Transfer_List_Offset : constant := 0;
   Task_List_Offset : constant := List_Alignment;

   --  One command table per slot: the request UPIU, the response UPIU and
   --  its data segment, then the physical region descriptors.
   Command_Table_Offset : constant := 16#1000#;
   Command_Table_Bytes  : constant := 16#800#;
   Request_Offset       : constant := 0;
   Response_Offset      : constant := 16#200#;
   Response_Bytes       : constant := 16#400#;
   Regions_Offset       : constant := Response_Offset + Response_Bytes;
   Region_Entry_Bytes   : constant := 16;

   --  One bounce buffer per slot, covered by one region descriptor.
   Buffer_Offset : constant := 16#10000#;
   Buffer_Bytes  : constant := 16#10000#;

   pragma Compile_Time_Error
     (Driver_Slots > UFSHCI.Maximum_Transfer_Slots, "too many slots");
   pragma Compile_Time_Error
     (Task_List_Offset + Task_Slots * UFS_UPIU.Task_Descriptor_Bytes
        > Command_Table_Offset, "task list overlaps the command tables");
   pragma Compile_Time_Error
     (Transfer_List_Offset + Driver_Slots * Transfer_Descriptor_Bytes
        > Task_List_Offset, "transfer list overlaps the task list");
   pragma Compile_Time_Error
     (Command_Table_Offset + Driver_Slots * Command_Table_Bytes
        > Buffer_Offset, "command tables overlap the buffers");
   pragma Compile_Time_Error
     (Buffer_Offset + Driver_Slots * Buffer_Bytes > Region_Bytes,
      "buffers exceed the DMA region");
   pragma Compile_Time_Error
     (Command_Table_Bytes mod UFS_UPIU.Command_Descriptor_Alignment /= 0
        or else Command_Table_Offset mod
                  UFS_UPIU.Command_Descriptor_Alignment /= 0,
      "command tables must be 128-byte aligned");
   pragma Compile_Time_Error
     (Regions_Offset + Region_Entry_Bytes > Command_Table_Bytes,
      "region descriptor outside its table");
   pragma Compile_Time_Error
     (Buffer_Bytes > UFS_UPIU.Maximum_Region_Bytes,
      "a buffer needs more than one region descriptor");

   subtype Region_Offset is Natural range 0 .. Region_Bytes - 1;

   function Transfer_Descriptor_At (Item : Slot) return Region_Offset is
     (Transfer_List_Offset + Item * Transfer_Descriptor_Bytes);
   function Command_Table_At (Item : Slot) return Region_Offset is
     (Command_Table_Offset + Item * Command_Table_Bytes);
   function Buffer_At (Item : Slot) return Region_Offset is
     (Buffer_Offset + Item * Buffer_Bytes);

   --  Offsets inside a command table in 32-bit words, as the transfer
   --  descriptor carries them.
   Response_Offset_Words : constant := Response_Offset / UFS_UPIU.Word_Bytes;
   Response_Length_Words : constant := Response_Bytes / UFS_UPIU.Word_Bytes;
   Regions_Offset_Words  : constant := Regions_Offset / UFS_UPIU.Word_Bytes;
end UFS_Layout;
