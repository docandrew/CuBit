pragma Ada_2022;
with ACPI_Bootstrap.Host_Bridge_Publication;
with ACPI_Host_Bridge_Publications;
with CuBit.Handles;
with CuBit.Protocols;
with Interfaces;
generic
   with package Publisher is new ACPI_Host_Bridge_Publications.Producers
     (Core => ACPI_Service, others => <>);
   with function Query_Window_Admitted return Boolean;
package ACPI_Requests.Host_Bridge_Publication is
   package G renames ACPI_Host_Bridge_Publications;
   package Controller is new ACPI_Bootstrap.Host_Bridge_Publication (Publisher, Query_Window_Admitted);
   subtype Run is Controller.Run;
   procedure Start (Item : in out Run; Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Root_ID, Sequence : G.W.Word;
      Budget : Natural; Maximum_Items : G.H.R.Item_Count; Allowed : out Controller.Admission)
      with Pre => Valid (Server);
   procedure Copy_And_Open (Item : in out Run; Server : in out State;
      Data : out G.S.Bytes; Descriptors : out G.H.Descriptor_List; Rows : out G.W.Rows;
      Endpoint : CuBit.Handles.Endpoint; Peer : G.B.P.Process_ID;
      Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
      Open_Token, Data_Token, Deadline : G.W.Word; Allowed : out Controller.Admission; Submitted : out Boolean)
      with Pre => Valid (Server);
   procedure Cancel (Item : in out Run; Server : in out State; Allowed : out Controller.Admission)
      with Pre => Valid (Server);
end ACPI_Requests.Host_Bridge_Publication;
