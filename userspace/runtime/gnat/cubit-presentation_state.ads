with Interfaces; use Interfaces;

--  One in-flight submission per instance. The generic ceiling lets tests
--  exercise exhaustion without adding a sequence-reset operation to the API.
generic
   Maximum_Identifier : Unsigned_64 := Unsigned_64'Last;
package CuBit.Presentation_State with SPARK_Mode, Pure is
   type Submission_ID is new Unsigned_64;
   No_Submission : constant Submission_ID := 0;
   type Phase is
     (Idle, Queued, Reading, Copied, Shown_Held, Dropped_Held,
      Shown_Released, Dropped_Released);
   type Admission is (Accepted, Busy, Exhausted, Closed);
   type Event is
     (Begin_Read, Release_Buffer, Was_Presented, Discard, Retire);
   type State is private;
   function Current (Item : State) return Phase;
   function Identifier (Item : State) return Submission_ID;
   function Is_Closed (Item : State) return Boolean;
   function Buffer_Held (Item : State) return Boolean is
     (Current (Item) in Queued | Reading | Shown_Held | Dropped_Held);
   function Allows (Stage : Phase; Action : Event) return Boolean;
   function Next_Phase (Stage : Phase; Action : Event) return Phase;

   procedure Submit
     (Item : in out State; Result : out Admission; ID : out Submission_ID)
     with Post =>
       (if Result = Accepted then
          not Is_Closed (Item) and Current (Item) = Queued and
          ID /= No_Submission and ID = Identifier (Item) and
          ID = Identifier (Item'Old) + 1
        else Item = Item'Old and ID = No_Submission);

   --  Caller is the serialized service/backend adapter, NOT an unchecked IPC
   --  peer. Release_Buffer means the final reader has actually stopped using
   --  source memory; Discard requires confirmed cancellation/quiescence, not
   --  merely a request to cancel hardware work that may already be submitted.
   --  Return_Buffer is a one-shot obligation for the adapter to return its
   --  acquisition. This model does not itself map memory or call the kernel.
   procedure Apply
     (Item : in out State; ID : Submission_ID; Action : Event;
      Applied, Return_Buffer : out Boolean)
     with Post =>
       Applied = (ID /= No_Submission and ID = Identifier (Item'Old) and
                  Allows (Current (Item'Old), Action)) and
       Return_Buffer = (Applied and Action = Release_Buffer) and
       (if Return_Buffer then
          Buffer_Held (Item'Old) and not Buffer_Held (Item)) and
       (if Applied then
          Current (Item) = Next_Phase (Current (Item'Old), Action) and
          Identifier (Item) = Identifier (Item'Old) and
          Is_Closed (Item) = Is_Closed (Item'Old)
        else Item = Item'Old);

   --  Stop admission and discard unstarted work only. In-flight reads/scanout
   --  still need completion even if a copied source has already been returned.
   procedure Close (Item : in out State)
     with Post => Is_Closed (Item) and
       Identifier (Item) = Identifier (Item'Old) and
       Buffer_Held (Item) = Buffer_Held (Item'Old) and
       Current (Item) =
         (case Current (Item'Old) is
            when Queued => Dropped_Held,
            when others => Current (Item'Old));
private
   type State is record
      Stage : Phase := Idle;
      Last_ID : Submission_ID := No_Submission;
      Closing : Boolean := False;
   end record;
   function Current (Item : State) return Phase is (Item.Stage);
   function Identifier (Item : State) return Submission_ID is (Item.Last_ID);
   function Is_Closed (Item : State) return Boolean is (Item.Closing);
end CuBit.Presentation_State;
