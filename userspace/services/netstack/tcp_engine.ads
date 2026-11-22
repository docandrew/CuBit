------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's TCP engine: the proved units from userspace/net/src,
--  instantiated for its connection slots.
--
--  Each slot's send queue holds up to 1 MiB in 4 KiB chunks from a shared
--  4 MiB pool (queues take chunks only as data is written, so the pool is
--  shared rather than reserved; a queue that finds it empty waits). Without window scaling (not yet
--  offered), the 16-bit window field bounds what a peer may send ahead
--  (we advertise at most 65,535); each receive queue holds 64 KiB, a power
--  of two so that its ring index is a mask, not a division per byte.
------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Chunk_Pool;
with Chunked_Send_Queue;
with TCP_Receive_Queue;
with TCP_Endpoint;
with TCP_Flow;
with TCP_Slots;

package TCP_Engine is
   Connections : constant := TCP_Slots.MAX_TCP_CONNS;
   Chunk_Bytes : constant := 4_096;
   Send_Chunks : constant := 256;
   Pool_Chunks : constant := 1_024;
   Receive_Bytes : constant := 65_536;

   package Pool is new Chunk_Pool
     (Chunk_Count => Pool_Chunks, Chunk_Size => Chunk_Bytes,
      Owner_Count => Connections);
   package Sends is new Chunked_Send_Queue (Chunks => Pool, Max_Chunks => Send_Chunks);
   package Receives is new TCP_Receive_Queue (Receive_Bytes);
   package Endpoints is new TCP_Endpoint
     (Chunks => Pool, Sends => Sends, Receives => Receives);
   package Flows is new TCP_Flow (Endpoints => Endpoints);

   --  One flow per connection slot, and the pool their send queues share.
   type Flow_Array is array (TCP_Slots.Connection_Index) of Flows.Flow;
   Flow_Table  : Flow_Array;
   Shared_Pool : Pool.Pool;

   --  A slot's owner identity in the pool.
   function Owner (Index : TCP_Slots.Connection_Index) return Pool.Owner_Id is
     (Index + 1);
end TCP_Engine;
