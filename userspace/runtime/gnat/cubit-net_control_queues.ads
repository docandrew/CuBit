------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's control queue (CuBit.Net_Channel_Layout, "Control queues"):
--  OPEN and SHUT requests and their answers as typed entries, and the
--  queue pair instance over them. The byte layout is the one C clients
--  use (cubit_net_channel.h); the records are pinned to it here.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Channel_Layout;
with CuBit.Submission_Queues;

package CuBit.Net_Control_Queues with Pure, SPARK_Mode is

   package Layout renames CuBit.Net_Channel_Layout;

   Word_Bits : constant := 32;
   Long_Bits : constant := 64;

   --  A request after its token.
   type Request is record
      Operation : Unsigned_32 := 0;   --  Layout.Queue_Open or Queue_Shut
      Length    : Unsigned_32 := 0;   --  OPEN: the target's length
      Object    : Unsigned_64 := 0;   --  OPEN: arena handle; SHUT: channel
      Buffer    : Unsigned_32 := 0;   --  OPEN: buffer index
      Reserved  : Unsigned_32 := 0;
   end record;
   for Request use record
      Operation at Layout.Request_Operation_At - Layout.Request_Operation_At
        range 0 .. Word_Bits - 1;
      Length    at Layout.Request_Length_At - Layout.Request_Operation_At
        range 0 .. Word_Bits - 1;
      Object    at Layout.Request_Object_At - Layout.Request_Operation_At
        range 0 .. Long_Bits - 1;
      Buffer    at Layout.Request_Buffer_At - Layout.Request_Operation_At
        range 0 .. Word_Bits - 1;
      Reserved  at Layout.Request_Buffer_At - Layout.Request_Operation_At + 4
        range 0 .. Word_Bits - 1;
   end record;
   for Request'Size use (Layout.Queue_Entry_Bytes - 8) * 8;
   for Request'Alignment use 8;

   --  An answer after its token.
   type Answer is record
      Status   : Unsigned_32 := Layout.Answer_OK;
      Reserved : Unsigned_32 := 0;
      Value    : Unsigned_64 := 0;   --  OPEN: the channel handle
      Spare    : Unsigned_64 := 0;
   end record;
   for Answer use record
      Status   at Layout.Answer_Status_At - Layout.Answer_Status_At
        range 0 .. Word_Bits - 1;
      Reserved at Layout.Answer_Status_At - Layout.Answer_Status_At + 4
        range 0 .. Word_Bits - 1;
      Value    at Layout.Answer_Value_At - Layout.Answer_Status_At
        range 0 .. Long_Bits - 1;
      Spare    at Layout.Answer_Value_At - Layout.Answer_Status_At + 8
        range 0 .. Long_Bits - 1;
   end record;
   for Answer'Size use (Layout.Queue_Entry_Bytes - 8) * 8;
   for Answer'Alignment use 8;

   package Queues is new CuBit.Submission_Queues
     (Request, Answer, Layout.Queue_Slot_Bits, Layout.Queue_Slot_Bits);

   pragma Compile_Time_Error
     (Queues.Submission'Size /= Layout.Queue_Entry_Bytes * 8 or else
      Queues.Completion'Size /= Layout.Queue_Entry_Bytes * 8,
      "control queue entries must be Queue_Entry_Bytes");
   pragma Compile_Time_Error
     (Queues.Submissions.Slots /= Layout.Queue_Slots or else
      Layout.Queue_Requests_At + Layout.Queue_Slots * Layout.Queue_Entry_Bytes
        > Layout.Queue_Answers_At or else
      Layout.Queue_Answers_At + Layout.Queue_Slots * Layout.Queue_Entry_Bytes
        > Layout.Queue_Bytes,
      "control queue layout");

end CuBit.Net_Control_Queues;
