------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One window's request channel to Desktop (IPC-004): a CuBit.Request_Channels
--  duplex carrying Compositor_Surface_Wire elements. A client opens it once
--  after creating the surface (one synchronous call); from then on frames,
--  publications, retirements and input move through the two rings, and a
--  side makes a system call only to wake a peer that declared itself
--  asleep (CuBit.Channels.Signal).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Request_Channels;
with Compositor_Surface_Wire;

package Compositor_Surface_Ring is

   package Wire renames Compositor_Surface_Wire;

   --  32 requests in flight per window, 16 answers waiting: a window
   --  keeps at most a few outstanding (Client_Surface_Ring's limits).
   Request_Bits    : constant := 5;
   Completion_Bits : constant := 4;
   package RC is new CuBit.Request_Channels
     (Wire.Request_Words, Wire.Result_Words, Request_Bits => Request_Bits,
      Completion_Bits => Completion_Bits);

   Schema : constant CuBit.Protocols.Schema_Id := 16#D35C_70B5_0BF1_0001#;
   Token_Bytes : constant := 8;
   Request_Bytes : constant := 6 * 8;
   pragma Compile_Time_Error
     (Wire.Request_Words'Size /= Request_Bytes * 8, "a request is six words");
   Submission_Bytes : constant := Token_Bytes + Request_Bytes;

   Element : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Submission_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => RC.Client_Pages,
      Buffers => RC.Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  What a window may have outstanding at once (the client keeps to
   --  these; Desktop owes at most this many answers per channel):
   --  presentations or publications in flight, retirement waits, input.
   Frames_In_Flight : constant := 2;
   Retirements_In_Flight : constant := 2;
   Input_Requests : constant := 1;
   Owed_Per_Channel : constant := Frames_In_Flight + Retirements_In_Flight + Input_Requests;
   pragma Compile_Time_Error
     (Owed_Per_Channel > 2 ** Completion_Bits, "a window's answers must fit its completion ring");

   --  Desktop's offer: windows per client process, and in total what its
   --  surface table can hold.
   Channels_Per_Client : constant := 16;

   --  Desktop's word in each ring's status block (CuBit.Request_Channels:
   --  Desktop writes it, the client reads it): the unrequested-input
   --  generation, counted up each time Desktop queues input for one of the
   --  client's windows that no outstanding input request could take. A
   --  window posting its next input request wakes a sleeping Desktop only
   --  when this moved since its last request, or its last answer said
   --  more was pending: otherwise Desktop has nothing to answer it with
   --  yet, and the input that will answer it wakes Desktop itself.
   Input_Generation_At : constant := 0;

end Compositor_Surface_Ring;
