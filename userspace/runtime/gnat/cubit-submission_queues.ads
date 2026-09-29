------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A submission/completion queue pair in memory two processes share, as
--  in io_uring and NVMe (docs/async-rings.md): the client pushes
--  requests onto the submission ring, the service answers each on the
--  completion ring, and a token the client chose pairs them.
--
--  A service instantiates it with its own request and result records;
--  both rings are CuBit.Slot_Rings, so indices from the peer are accepted
--  only if they move forward within the ring.
--
--  The service takes a request only while it holds a completion slot for
--  it (Can_Take): it never owes more answers than the completion ring can
--  hold, so an answer is never dropped and never waits. A client that
--  submits more than it reaps stalls only itself.
--
--  Proved (tests/channel-rings, through an instance): every answer the
--  service owes has a completion slot (Owed <= Space); taking a request
--  owes one more answer, completing one owes one fewer; the client never
--  has more requests outstanding than completion slots; and the rings'
--  own properties (CuBit.Slot_Rings).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Slot_Rings;

generic
   type Request is private;    --  the service's opcode and arguments
   type Result is private;     --  the service's answer
   Submission_Bits : Natural;
   Completion_Bits : Natural;
package CuBit.Submission_Queues with Pure, SPARK_Mode is

   --  Chosen by the client, returned unchanged with the answer.
   type Token is new Unsigned_64;

   type Submission is record
      Tag  : Token;
      Item : Request;
   end record;

   type Completion is record
      Tag    : Token;
      Answer : Result;
   end record;

   package Submissions is new CuBit.Slot_Rings (Submission, Submission_Bits);
   package Completions is new CuBit.Slot_Rings (Completion, Completion_Bits);

   use type Submissions.Index, Submissions.Producer, Submissions.Consumer,
            Completions.Producer, Completions.Consumer;

   Completion_Slots : constant Positive := Completions.Slots;
   subtype Outstanding is Natural range 0 .. Completion_Slots;

   ------------------------------------------------------------------------
   --  The client: pushes requests, reaps answers.
   ------------------------------------------------------------------------
   type Client is record
      Requests : Submissions.Producer;
      Answers  : Completions.Consumer;
      Pending  : Outstanding := 0;   --  submitted, answer not yet reaped
   end record;

   --  A new request fits the submission ring and has a completion slot.
   function Can_Submit (C : Client) return Boolean is
     (Submissions.Space (C.Requests) > 0 and then
      C.Pending < Completion_Slots);

   procedure Submit
     (C : in out Client; Ring : in out Submissions.Ring;
      Tag : Token; Item : Request)
   with
     Pre  => Can_Submit (C),
     Post => C.Pending = C'Old.Pending + 1 and then
             C.Answers = C'Old.Answers and then
             Ring (Submissions.Slot_Of (C'Old.Requests.Produced)) =
               (Tag => Tag, Item => Item) and then
             C.Requests.Produced = C'Old.Requests.Produced + 1;

   --  Take the answer at the head of the completion ring. An answer the
   --  service sends without a pending request is refused (OK false), so a
   --  service cannot make the client's count go negative.
   procedure Reap
     (C : in out Client; Ring : Completions.Ring;
      Answer : out Completion; OK : out Boolean)
   with
     Pre  => C.Answers.Available > 0,
     Post => OK = (C'Old.Pending > 0) and then
             (if OK then
                C.Pending = C'Old.Pending - 1 and then
                Answer = Ring (Completions.Slot_Of (C'Old.Answers.Consumed))
              else C.Pending = 0) and then
             C.Requests = C'Old.Requests and then
             C.Answers.Consumed = C'Old.Answers.Consumed + 1;

   --  Take the service's consumed index for the submission ring: the
   --  slots of requests it has taken are free again.
   procedure Accept_Taken
     (C : in out Client; Value : Submissions.Index; OK : out Boolean)
   with
     Post => C.Pending = C'Old.Pending and then C.Answers = C'Old.Answers
             and then Submissions.Space (C.Requests) >=
                      Submissions.Space (C'Old.Requests);

   ------------------------------------------------------------------------
   --  The service: takes requests, completes them.
   ------------------------------------------------------------------------
   type Server is record
      Requests : Submissions.Consumer;
      Answers  : Completions.Producer;
      Owed     : Outstanding := 0;   --  taken, not yet answered
   end record;

   --  Every answer owed has its completion slot.
   function Valid (S : Server) return Boolean is
     (S.Owed <= Completions.Space (S.Answers));

   --  A request waits and an answer for it is guaranteed room.
   function Can_Take (S : Server) return Boolean is
     (S.Requests.Available > 0 and then
      S.Owed < Completions.Space (S.Answers));

   --  Copy the next request out (the caller then validates it) and owe
   --  its answer.
   procedure Take
     (S : in out Server; Ring : Submissions.Ring; Item : out Submission)
   with
     Pre  => Valid (S) and then Can_Take (S),
     Post => Valid (S) and then S.Owed = S'Old.Owed + 1 and then
             S.Answers = S'Old.Answers and then
             Item = Ring (Submissions.Slot_Of (S'Old.Requests.Consumed))
             and then
             S.Requests.Consumed = S'Old.Requests.Consumed + 1;

   --  Answer one request that was taken. It always has room.
   procedure Complete
     (S : in out Server; Ring : in out Completions.Ring;
      Tag : Token; Answer : Result)
   with
     Pre  => Valid (S) and then S.Owed > 0,
     Post => Valid (S) and then S.Owed = S'Old.Owed - 1 and then
             S.Requests = S'Old.Requests and then
             Ring (Completions.Slot_Of (S'Old.Answers.Produced)) =
               (Tag => Tag, Answer => Answer) and then
             S.Answers.Produced = S'Old.Answers.Produced + 1;

   --  Take the client's consumed index for the completion ring. Accepting
   --  it only frees slots, so every answer owed keeps its slot.
   procedure Accept_Reaped
     (S : in out Server; Value : Completions.Index; OK : out Boolean)
   with
     Pre  => Valid (S),
     Post => Valid (S) and then S.Owed = S'Old.Owed and then
             S.Requests = S'Old.Requests and then
             Completions.Space (S.Answers) >=
               Completions.Space (S'Old.Answers);

end CuBit.Submission_Queues;
