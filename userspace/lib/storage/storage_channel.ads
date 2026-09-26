with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Async_Requests;

--  Single-dispatcher asynchronous filesystem channel. No borrowed buffer is
--  retained: submissions copy into this object's aligned grant buffer.
--  Keep the limited object alive and at a stable address through retirement.
package Storage_Channel is
   Transfer_Bytes : constant := 64 * 1024;
   type Operation is
     (Open_Existing, Open_Create, Close, Read_Data, Write_Data,
      Size, Resize, Flush);
   for Operation use
     (Open_Existing => 0, Open_Create => 1, Close => 2, Read_Data => 3,
      Write_Data => 4, Size => 5, Resize => 6, Flush => 7);

   type Phase is (Fresh, Ready, Waiting, Result_Ready, Failed, Retired);
   type Submission is (Submitted, Busy, Invalid_Request, Unavailable, Not_Submitted);
   type Completion_Result is (Ignored, Completed);
   type Transfer_Buffer is new String with Alignment => 4096;
   type Channel is limited private;

   function Status (Object : Channel) return Phase;
   function Pending_Token (Object : Channel) return Unsigned_64;
   procedure Initialize
     (Object : in out Channel; Endpoint : CuBit.Messages.CapabilitySlot;
      Success : out Boolean);

   --  Token comes from the dispatcher's process-wide, non-reusing allocator.
   --  Never reuse it across channels or replacement channel lifetimes. This
   --  object additionally enforces strictly increasing, nonzero tokens.
   --  A rejected submission does not mutate the file and is not retried here.
   procedure Submit
     (Object : in out Channel; Op : Operation; Handle, Position : Unsigned_64;
      Input : String; Read_Length : Natural; Token : Unsigned_64;
      Result : out Submission);

   -- Trusted in-process payload producer, called only after admission and
   -- before submission. Fill every byte; never retain the view, reenter this
   -- channel, or perform IPC. The callback writes directly into the owned loan.
   generic
      with procedure Fill (Buffer : out Transfer_Buffer);
   procedure Submit_With_Payload
     (Object : in out Channel; Op : Operation; Handle, Position : Unsigned_64;
      Input_Length, Read_Length : Natural; Token : Unsigned_64;
      Result : out Submission);

   --  ONLY pass entries drained from the kernel completion queue, not ordinary
   --  service messages. The kernel authenticates the reply capability/request
   --  lifetime; the token correlates work and is NOT authority. This procedure
   --  does not poll or steal other components' completions.
   procedure Complete
     (Object : in out Channel; Completion : CuBit.Messages.CompletionEntry;
      Result : out Completion_Result);

   --  Read data stays owned until taken. A too-small output leaves the result
   --  pending so the caller can supply a sufficient buffer. Non-read results
   --  accept a null string. No output bytes are touched on operation failure.
   procedure Take_Result
     (Object : in out Channel; Output : in out String;
      Code : out Unsigned_32; Value : out Unsigned_64; Taken : out Boolean);

   --  Terminal, including when I/O is still pending. No cancellation/rollback
   --  claim. Never reclaim the object before Retirement_Confirmed is true.
   --  May be called again to confirm a previously pending retirement.
   procedure Retire (Object : in out Channel; Retirement_Confirmed : out Boolean);

private
   type Channel is limited record
      Buffer : Transfer_Buffer (1 .. Transfer_Bytes) := [others => Character'Val (0)];
      Current : Phase := Fresh;
      Endpoint : CuBit.Messages.CapabilitySlot := 0;
      Grant : CuBit.Memory_Grants.Grant_Reference := (slot => 0, generation => 1);
      Has_Grant : Boolean := False;
      Revocation_Requested : Boolean := False;
      Poisoned : Boolean := False;
      Request_Lifetime : CuBit.Async_Requests.Tracker;
      Pending_Operation : Operation := Close;
      Length : Natural range 0 .. Transfer_Bytes := 0;
      Reply_Code : Unsigned_32 := 0;
      Reply_Value : Unsigned_64 := 0;
   end record;
end Storage_Channel;
