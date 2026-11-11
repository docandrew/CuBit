pragma Ada_2022;
with ACPI_PCI_Window_Exchange;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Process_IDs;
package ACPI_PCI_Window_Client is
   package Q renames ACPI_PCI_Window_Exchange;
   package H renames CuBit.Handles;
   package P renames CuBit.Process_IDs;
   type Context is limited private;
   type Outcome is (Acquired, Denied, Invalid_Request, Invalid_Provider,
      Deadline_Passed, Invalid_Reply, Invalid_Memory, Cleanup_Pending, Already_Held);
   type Report is record
      Status : Outcome := Invalid_Request;
      Reason : Q.Disposition := Q.Unavailable;
   end record;
   -- Expected_Server is the full identity saved from the trusted startup
   -- endpoint, never a process number supplied in an RPC reply. Deadline is
   -- a finite kernel millisecond deadline. This call only goes down to devmgr.
   procedure Acquire
     (Run : in out Context; Target : H.Endpoint; Expected_Server : P.Process_ID;
      Request : Q.Request; Deadline : Q.Word; Result : out Report);
   function Ready (Run : Context) return Boolean;
   function Memory (Run : Context) return H.Device_Memory with Pre => Ready (Run);
   function Description (Run : Context) return Q.Reply with Pre => Ready (Run);
   function Requested (Run : Context) return Q.Request with Pre => Ready (Run);
   -- Close the owned handle, including rejected unexpected capabilities.
   -- A failed close retains it for retry; Acquire never overwrites it.
   procedure Release (Run : in out Context; Released : out Boolean);
   -- Backend owner handoff, not a capability copy. The recipient must retain
   -- Owned until it closes it or exits. This context is empty afterwards,
   -- including when the handle was rejected and still needs cleanup.
   procedure Move
     (Run : in out Context; Owned : out H.IPC.Carried;
      Value : out H.Device_Memory; Info : out Q.Reply;
      Request : out Q.Request; Accepted : out Boolean);
private
   type Context is limited record
      Owned : H.IPC.Carried;
      Value : H.Device_Memory := H.No_Device_Memory;
      Info : Q.Reply;
      Request : Q.Request;
      Accepted : Boolean := False;
   end record;
end ACPI_PCI_Window_Client;
