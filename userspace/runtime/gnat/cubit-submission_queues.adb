------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Submission_Queues with SPARK_Mode is

   procedure Submit
     (C : in out Client; Ring : in out Submissions.Ring;
      Tag : Token; Item : Request) is
   begin
      Submissions.Push (C.Requests, Ring, (Tag => Tag, Item => Item));
      C.Pending := C.Pending + 1;
   end Submit;

   procedure Reap
     (C : in out Client; Ring : Completions.Ring;
      Answer : out Completion; OK : out Boolean) is
   begin
      Completions.Take (C.Answers, Ring, Answer);
      OK := C.Pending > 0;
      if OK then
         C.Pending := C.Pending - 1;
      end if;
   end Reap;

   procedure Accept_Taken
     (C : in out Client; Value : Submissions.Index; OK : out Boolean) is
   begin
      Submissions.Accept_Consumed (C.Requests, Value, OK);
   end Accept_Taken;

   procedure Take
     (S : in out Server; Ring : Submissions.Ring; Item : out Submission) is
   begin
      Submissions.Take (S.Requests, Ring, Item);
      S.Owed := S.Owed + 1;
   end Take;

   procedure Complete
     (S : in out Server; Ring : in out Completions.Ring;
      Tag : Token; Answer : Result) is
   begin
      Completions.Push (S.Answers, Ring, (Tag => Tag, Answer => Answer));
      S.Owed := S.Owed - 1;
   end Complete;

   procedure Accept_Reaped
     (S : in out Server; Value : Completions.Index; OK : out Boolean) is
   begin
      Completions.Accept_Consumed (S.Answers, Value, OK);
   end Accept_Reaped;

end CuBit.Submission_Queues;
