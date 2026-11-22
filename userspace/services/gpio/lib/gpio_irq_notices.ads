pragma Ada_2022;
with CuBit.Process_IDs;
package GPIO_IRQ_Notices with Pure, SPARK_Mode is
   type State is private;
   procedure Configure (S : in out State; Parent_Present : Boolean; Accepted : out Boolean);
   procedure Note (S : in out State; Publisher : CuBit.Process_IDs.Process_ID;
      IRQ_Doorbell, Frame_Valid : Boolean; Accepted : out Boolean);
   procedure Take (S : in out State; Pending : out Boolean);
   procedure Hold (S : in out State; Held : Boolean);
   function Requests_Held (S : State) return Boolean;
   function Parent_Present (S : State) return Boolean;
   -- Configure once, after native validation of the optional sole parent-line
   -- handle. Kernel event-queue Publisher metadata is required; message words
   -- never supply it. IRQ_Doorbell comes from the native IRQ ABI decoder.
   -- Note freezes requests in the same pump turn, before request dispatch.
   -- Taking a notice never unfreezes. A notice queued during ACK keeps the
   -- hold until its own sweep, even if the previous sweep requests release.
private
   type State is record
      Configured, Present, Pending, Held : Boolean := False;
   end record;
end GPIO_IRQ_Notices;
