--  What the fixture runtime serves the driver: a queue of requests, a
--  clock, and a record of every answer.
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
package Block_Script is
   Capacity : constant := 4096;
   subtype Index is Positive range 1 .. Capacity;

   type Pending is record
      Sender  : Process_ID := 42;
      Request : Message := NULL_MESSAGE;
   end record;
   Queue : array (Index) of Pending;
   Queued, Taken : Natural := 0;
   procedure Push (Request : Message; Sender : Process_ID := 42);
   function Waiting return Natural is (Queued - Taken);

   --  Answers, in order. Saved: answered through a kept reply (Reply_Word
   --  identifies it, the request's position in the queue).
   type Answer is record
      Saved      : Boolean := False;
      Reply_Word : Unsigned_64 := 0;
      To         : Process_ID := 0;
      Response   : Message := NULL_MESSAGE;
   end record;
   Answers : array (Index) of Answer;
   Answered : Natural := 0;
   function Last return Answer is (Answers (Answered));

   Clock : Unsigned_64 := 1_000;
   Refuse_Keep : Boolean := False;
   Kept_Replies : Natural := 0;
end Block_Script;
