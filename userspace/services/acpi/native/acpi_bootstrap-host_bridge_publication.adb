pragma Ada_2022;
package body ACPI_Bootstrap.Host_Bridge_Publication is
   function Observe (Item : Run) return Publisher.Report is (Publisher.Observe (Item.Work));
   function Transport (Item : Run) return B.Report is (Publisher.Transport (Item.Work));
   procedure Start (Item : in out Run; Session : in out State;
      Device : ACPI_Service.Node_Descriptor; Root_ID, Sequence : G.W.Word;
      Budget : Natural; Maximum_Items : G.H.R.Item_Count; Allowed : out Admission) is
   begin
      Allowed := Already_Started;
      if Item.Started then return; end if;
      Allowed := Not_Ready;
      if Storage_Released (Session) or else not Device_Walk_Finished (Session) then return; end if;
      Allowed := Owner_Busy;
      if not Query_Window_Admitted or else ACPI_Service.Retained_Results (Session.Core) /= 0 then return; end if;
      Allowed := Invalid_Device;
      if not ACPI_Service.Current_Node (Session.Core, Device) then return; end if;
      Item.Started := True; Item.Device := Device; Allowed := Admitted;
      Publisher.Start (Item.Work, Session.Core, Device, Root_ID, Sequence, Budget, Maximum_Items);
   end Start;
   procedure Copy_And_Open (Item : in out Run; Session : in out State;
      Data : out G.S.Bytes; Descriptors : out G.H.Descriptor_List; Rows : out G.W.Rows;
      Endpoint : CuBit.Handles.Endpoint; Peer : B.P.Process_ID;
      Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
      Open_Token, Data_Token, Deadline : G.W.Word; Allowed : out Admission; Submitted : out Boolean) is
   begin
      Submitted := False; Data := [others => 0]; Rows := [others => G.W.Empty_Row];
      Descriptors := [others => (Form => G.H.Empty, others => <>)];
      Allowed := Not_Started;
      if not Item.Started then return; end if;
      Allowed := Not_Ready;
      if Storage_Released (Session) or else not Device_Walk_Finished (Session) then return; end if;
      Allowed := Wrong_Context;
      if not ACPI_Service.Current_Node (Session.Core, Item.Device) then return; end if;
      Allowed := Owner_Busy;
      if not Query_Window_Admitted then return; end if;
      Allowed := Admitted;
      Publisher.Copy_And_Open (Item.Work, Session.Core, Data, Descriptors, Rows,
        Endpoint, Peer, Schema, Connector, Open_Token, Data_Token, Deadline, Submitted);
   end Copy_And_Open;
   procedure Cancel (Item : in out Run; Session : in out State; Allowed : out Admission) is
   begin
      Allowed := Not_Started;
      if not Item.Started then return; end if;
      Allowed := Wrong_Context;
      if Storage_Released (Session) or else not ACPI_Service.Current_Node (Session.Core, Item.Device) then return; end if;
      Allowed := Admitted;
      Publisher.Cancel (Item.Work, Session.Core);
   end Cancel;
   procedure Complete_Open (Item : in out Run; Completion : CuBit.Messages.CompletionEntry; Handled : out Boolean) is
   begin Publisher.Complete_Open (Item.Work, Completion, Handled); end Complete_Open;
   procedure Advance (Item : in out Run; Now : G.W.Word; Submitted : out Boolean) is
   begin Publisher.Advance (Item.Work, Now, Submitted); end Advance;
   procedure Prepare_Wait (Item : in out Run; Armed : out Boolean) is
   begin Publisher.Prepare_Wait (Item.Work, Armed); end Prepare_Wait;
   procedure Disarm (Item : in out Run) is
   begin Publisher.Disarm (Item.Work); end Disarm;
   procedure Note_Event (Item : in out Run; Event : CuBit.Control_Events.Event; Ends : out Boolean) is
   begin Publisher.Note_Event (Item.Work, Event, Ends); end Note_Event;
   procedure Close (Item : in out Run; Completed : out Boolean) is
   begin Publisher.Close (Item.Work, Completed); end Close;
   function Retired (Item : Run) return Boolean is (Publisher.Retired (Item.Work));
end ACPI_Bootstrap.Host_Bridge_Publication;
