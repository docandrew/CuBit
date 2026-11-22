pragma Ada_2022;
with ACPI_Host_Bridge_Publications;
with ACPI_Service;
with CuBit.Handles;
with CuBit.Protocols;
with CuBit.Messages;
with CuBit.Control_Events;
with Interfaces;
generic
   with package Publisher is new ACPI_Host_Bridge_Publications.Producers
     (Core => ACPI_Service, others => <>);
   -- Fixed local native owner: the SAME finite runtime query window is open,
   -- no other evaluator/provider/receipt/recovery work can enter this Core.
   with function Query_Window_Admitted return Boolean;
package ACPI_Bootstrap.Host_Bridge_Publication is
   package G renames ACPI_Host_Bridge_Publications;
   package B renames G.B;
   type Admission is (Not_Ready, Owner_Busy, Invalid_Device, Wrong_Context,
      Not_Started, Already_Started, Admitted);
   type Run is limited private;
   function Observe (Item : Run) return Publisher.Report;
   function Transport (Item : Run) return B.Report;
   procedure Start (Item : in out Run; Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Root_ID, Sequence : G.W.Word;
      Budget : Natural; Maximum_Items : G.H.R.Item_Count; Allowed : out Admission)
      with Pre => Valid (Session);
   procedure Copy_And_Open (Item : in out Run; Session : in out State;
      Data : out G.S.Bytes; Descriptors : out G.H.Descriptor_List; Rows : out G.W.Rows;
      Endpoint : CuBit.Handles.Endpoint; Peer : B.P.Process_ID;
      Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
      Open_Token, Data_Token, Deadline : G.W.Word; Allowed : out Admission; Submitted : out Boolean)
      with Pre => Valid (Session);
   -- Stop may close query admission. Cleanup remains available for exactly
   -- the original Core/Device, without new AML, even when admission is closed.
   procedure Cancel (Item : in out Run; Session : in out State; Allowed : out Admission)
      with Pre => Valid (Session);
   procedure Complete_Open (Item : in out Run; Completion : CuBit.Messages.CompletionEntry; Handled : out Boolean);
   procedure Advance (Item : in out Run; Now : G.W.Word; Submitted : out Boolean);
   procedure Prepare_Wait (Item : in out Run; Armed : out Boolean);
   procedure Disarm (Item : in out Run);
   procedure Note_Event (Item : in out Run; Event : CuBit.Control_Events.Event; Ends : out Boolean);
   procedure Close (Item : in out Run; Completed : out Boolean);
   function Retired (Item : Run) return Boolean;
private
   type Run is limited record
      Started : Boolean := False;
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
      Work : Publisher.Session;
   end record;
end ACPI_Bootstrap.Host_Bridge_Publication;
