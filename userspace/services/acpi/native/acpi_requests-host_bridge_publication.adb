pragma Ada_2022;
package body ACPI_Requests.Host_Bridge_Publication is
   procedure Start (Item : in out Run; Server : in out State;
      Device : ACPI_Service.Node_Descriptor; Root_ID, Sequence : G.W.Word;
      Budget : Natural; Maximum_Items : G.H.R.Item_Count; Allowed : out Controller.Admission) is
   begin
      Controller.Start (Item, Server.Boot, Device, Root_ID, Sequence, Budget, Maximum_Items, Allowed);
   end Start;
   procedure Copy_And_Open (Item : in out Run; Server : in out State;
      Data : out G.S.Bytes; Descriptors : out G.H.Descriptor_List; Rows : out G.W.Rows;
      Endpoint : CuBit.Handles.Endpoint; Peer : G.B.P.Process_ID;
      Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
      Open_Token, Data_Token, Deadline : G.W.Word; Allowed : out Controller.Admission; Submitted : out Boolean) is
   begin
      Controller.Copy_And_Open (Item, Server.Boot, Data, Descriptors, Rows,
        Endpoint, Peer, Schema, Connector, Open_Token, Data_Token, Deadline, Allowed, Submitted);
   end Copy_And_Open;
   procedure Cancel (Item : in out Run; Server : in out State; Allowed : out Controller.Admission) is
   begin Controller.Cancel (Item, Server.Boot, Allowed); end Cancel;
end ACPI_Requests.Host_Bridge_Publication;
