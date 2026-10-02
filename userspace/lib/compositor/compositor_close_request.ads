with Compositor_Input_Queue;
package Compositor_Close_Request with SPARK_Mode, Pure is
   package IQ renames Compositor_Input_Queue;
   subtype Word is IQ.Word;
   use type Word;
   -- One serial per input channel. Zero means no unacknowledged close request.
   -- Ordinary input overflow never mutates this separately retained state.
   procedure Request (Pending, Next_Serial : in out Word)
     with Post =>
       (if Pending'Old /= 0 then
          Pending = Pending'Old and Next_Serial = Next_Serial'Old
        elsif Next_Serial'Old = 0 or Next_Serial'Old = Word'Last then
          Pending = 0 and Next_Serial = Next_Serial'Old
        else Pending = Next_Serial'Old and Next_Serial = Next_Serial'Old + 1);
   function Has_After (Pending, After : Word) return Boolean is
     (Pending /= 0 and then Pending > After);
   function Select_Close (Pending, After, Queued_Serial : Word) return Boolean is
     (Has_After (Pending, After) and then
       (Queued_Serial = 0 or else Pending < Queued_Serial));
   -- A retained close is a strict barrier between ordinary motion reports.
   -- Zero Newest_Serial denotes an empty ordinary queue.
   function May_Coalesce (Pending, Newest_Serial : Word) return Boolean is
     (Pending = 0 or else Newest_Serial > Pending);
   -- Delivery alone does not clear the latch: the next authenticated poll's
   -- After_Serial acknowledges it. This permits retry after a failed reply.
   procedure Acknowledge (Pending : in out Word; After : Word)
     with Post => Pending = (if Pending'Old <= After then 0 else Pending'Old);
end Compositor_Close_Request;
