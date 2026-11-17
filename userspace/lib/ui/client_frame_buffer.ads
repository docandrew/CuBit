with System;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Memory_Grants;
with Interfaces;
with Client_Frame_State;
with Client_Surface_Ring;
with Compositor_Surface_Wire;
-- Serialized owner of one reclaimable frame allocation. Handles cannot be
-- copied. Write pointers are valid only until Present or Release; Present
-- also removes write permission from the owner's mapping before IPC.
-- IPC authentication, kernel protection and backing retention are trusted
-- foreign boundaries. The pure state policy governs write eligibility.
package Client_Frame_Buffer with SPARK_Mode => Off is
   package DP renames CuBit.Desktop_Protocol;
   package Pub renames CuBit.Desktop_Protocol.Publication;
   type Buffer is limited private;
   procedure Allocate (B : in out Buffer; Bytes : Natural; OK : out Boolean);
   function Writable_Address (B : Buffer) return System.Address;
   function Capacity (B : Buffer) return Natural;
   procedure Prepare_Write (B : in out Buffer; Ready : out Boolean);
   procedure Present
     (B : in out Buffer; Surface : DP.Live_Surface_Name; Epoch : Pub.Identity;
      Area : DP.Rectangle; Accepted : out Boolean;
      Input_After : Interfaces.Unsigned_64 := 0);
   procedure Release (B : in out Buffer; Released : out Boolean);

   --  Through the window's surface ring (IPC-004): no system call. Publish
   --  and retirement are requests whose answers come back through the ring
   --  (Note_Answer). On this path the buffer is not write-protected while
   --  Desktop reads it: the owner keeps to the policy (Sealed and Held mean
   --  "do not write") without a PROTECT call per frame. Desktop maps every
   --  buffer read-only and copies the rows it shows, so an owner that writes
   --  anyway can only tear its own window.
   --  Ready False: retirement is still awaited (its request is outstanding).
   procedure Prepare_Write
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection; Ready : out Boolean);
   --  Accepted: the publication was submitted (its answer comes later);
   --  False: no room in the ring (nothing changed; retry).
   procedure Present
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection;
      Surface : DP.Live_Surface_Name; Epoch : Pub.Identity;
      Area : DP.Rectangle; Accepted : out Boolean;
      Input_After : Interfaces.Unsigned_64 := 0);
   --  An answer from the ring. Matched: it was this buffer's. Refused: its
   --  publication was refused (the frame was not shown).
   procedure Note_Answer
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection;
      Tag : Client_Surface_Ring.Token; Answer : Compositor_Surface_Wire.Result;
      Matched, Refused : out Boolean);
   --  A ring request of this buffer's is outstanding.
   function Awaiting (B : Buffer) return Boolean;
private
   type Buffer is limited record
      Policy : Client_Frame_State.State;
      Base : Interfaces.Unsigned_64 := 0;
      Bytes : Natural := 0;
      Has_Grant : Boolean := False;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Surface : DP.Live_Surface_Name := 1;
      --  Ring path: the outstanding publication (and the epoch it named)
      --  and retirement wait; 0 when none.
      Publish_Tag, Await_Tag : Client_Surface_Ring.Token := 0;
      Publish_Epoch : Pub.Identity := 1;
   end record;
end Client_Frame_Buffer;
