with Interfaces;
with Input_Pending;
package Keyboard_Pending with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   subtype Word is Input_Pending.Word;
   use type Byte, Word, Input_Pending.Item;
   subtype Frame_Length is Natural range 0 .. 6;
   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Count (S : State) return Input_Pending.Count_Type;
   function Element (S : State; Offset : Input_Pending.Offset_Type) return Input_Pending.Item
     with Pre => Offset < Count (S);
   function Wake_Deadline (S : State; Now, Otherwise : Word) return Word;
   -- E0 pairs and E1 six-byte groups enter the queue as complete groups.
   -- Reserve room for the whole group before appending any byte. Overflow
   -- drops the old backlog and marks the FIRST new byte for resynchronization.
   procedure Append_Byte (S : in out State; Value : Byte;
                          Added : out Frame_Length; Lost : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (if Added = 0 then not Lost and Count (S) = Count (S'Old)
        else Count (S) = (if Lost then Added else Count (S'Old) + Added)) and
       (if not Lost then
          (for all I in Input_Pending.Offset_Type =>
             (if I < Count (S'Old) then Element (S, I) = Element (S'Old, I)))) and
       (if Lost then Added > 0 and Element (S, 0).Recover);
   procedure Acknowledge (S : in out State)
     with Pre => Valid (S) and Count (S) > 0,
       Post => Valid (S) and Count (S) = Count (S'Old) - 1 and
         (for all I in Input_Pending.Offset_Type =>
            (if I < Count (S) then Element (S, I) = Element (S'Old, I + 1)));
   -- A partial group began for the old consumer: finish consuming it but do
   -- not deliver its suffix alone or leak it to the replacement consumer.
   procedure Reset_Consumer (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and Count (S) = 0;
private
   type Bytes is array (Positive range 1 .. 6) of Byte;
   type State is record
      Pending : Input_Pending.Queue;
      Partial : Bytes := (others => 0);
      Used : Frame_Length := 0;
      Expected : Positive range 1 .. 6 := 1;
      Discard_Partial : Boolean := False;
   end record;
   function Valid (S : State) return Boolean is
     (S.Used < S.Expected and (if S.Used = 0 then not S.Discard_Partial));
   function Count (S : State) return Input_Pending.Count_Type is
     (Input_Pending.Count (S.Pending));
   function Element (S : State; Offset : Input_Pending.Offset_Type) return Input_Pending.Item is
     (Input_Pending.Element (S.Pending, Offset));
   function Wake_Deadline (S : State; Now, Otherwise : Word) return Word is
     (Input_Pending.Wake_Deadline (S.Pending, Now, Otherwise));
end Keyboard_Pending;
