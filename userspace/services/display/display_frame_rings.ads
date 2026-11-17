------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display's side of the frame rings (Display_Frame_Ring, IPC-009): Desktop
--  presents through its ring and Display answers through the same channel;
--  the kernel only set the channel up and rings a side that sleeps.
--
--  @description
--  Any holder of Display's endpoint may open one (Channels_Per_Client
--  each); every frame taken from it is still checked as a frame request
--  always was (the display lease, session, sequence and buffer the sender
--  holds), so a ring carries no authority of its own. Next_Present copies
--  one present out of a ring (Request_Channels reads the client's slot
--  once) and decodes the copy. Answer writes its answer: now, or later when
--  the GPU finished the frame. A ring that ends (its client closed it or
--  died) drops what it was owed; Ended says which, so the caller forgets
--  its debts there before the slot can carry another channel.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Messages;
with Display_Frame_Ring;

package Display_Frame_Rings is

   package FR renames Display_Frame_Ring;

   --  A ring's slot (0: none).
   type Ring_Number is new Natural;
   No_Ring : constant Ring_Number := 0;

   procedure Configure (Ready : out Boolean);

   --  A request on Display's endpoint. Is_Open: it opened a frame ring
   --  (Answer is the reply to send). Other requests are not ours.
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message);

   --  The next present waiting in any ring (Got False: none). Valid False:
   --  its words did not decode (answer it anyway: every present is answered).
   procedure Next_Present
     (Got : out Boolean; Ring : out Ring_Number; From : out CuBit.Messages.Process_ID;
      Tag : out FR.RC.Token; Item : out FR.Present; Valid : out Boolean);

   --  Answer the present Tag of Ring (nothing when the ring ended since).
   procedure Answer
     (Ring : Ring_Number; Tag : FR.RC.Token; Result : CuBit.Desktop_Protocol.Wire_Message;
      Taken_Us : Unsigned_64);

   --  Before sleeping: every ring armed and every client owed a wake woken.
   --  False: a present arrived meanwhile (serve it; do not sleep).
   function Quiet return Boolean;

   --  A notice from the kernel (Publisher No_Process). Handled: a ring's
   --  wake, or a grant event; Ended: the ring it ended (No_Ring otherwise).
   procedure Take_Notice
     (Notice : CuBit.Messages.Message; Handled : out Boolean; Ended : out Ring_Number);

   function Open_Rings return Natural;

end Display_Frame_Rings;
