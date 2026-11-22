pragma Ada_2022;
package body Query_Pump_Proof_Model with SPARK_Mode is
   procedure Acquire (Owner : in out Context; Loan, Expected_Owner, Byte_Length : W.Word; Mapping : out W.Word; Accepted : out Boolean) is
      pragma Unreferenced (Expected_Owner, Byte_Length);
   begin Mapping := Loan; Accepted := Owner.Acquire_OK; end Acquire;
   procedure Copy_Buffer (Source : Buffer; Destination : out P.Byte_Array; Accepted : out Boolean) with Global => null is
   begin
      Destination := [others => 0]; Accepted := True;
      for I in Destination'Range loop
         if I - Destination'First < Source'Length then Destination (I) := Source (I - Destination'First); end if;
      end loop;
   end Copy_Buffer;
   procedure Copy_In (Owner : in out Context; Mapping : W.Word; Data : out P.Byte_Array; Accepted : out Boolean) is
      pragma Unreferenced (Mapping);
   begin Copy_Buffer (Owner.Incoming, Data, Accepted); Accepted := Accepted and Owner.Copy_In_OK; end Copy_In;
   procedure Evaluate (Owner : in out Context; Request_Data : P.Byte_Array; Response_Data : out P.Byte_Array;
                       Written, Required_Bytes : out W.Word; Accepted : out Boolean) is
      pragma Unreferenced (Request_Data);
   begin
      Copy_Buffer (Owner.Outgoing, Response_Data, Accepted); Accepted := Accepted and Owner.Evaluate_OK;
      Written := Owner.Written; Required_Bytes := Owner.Required;
   end Evaluate;
   procedure Copy_Out (Owner : in out Context; Mapping : W.Word; Data : P.Byte_Array; Accepted : out Boolean) is
      pragma Unreferenced (Mapping, Data);
   begin Accepted := Owner.Copy_Out_OK; end Copy_Out;
   procedure Release (Owner : in out Context; Loan, Mapping : W.Word; Accepted : out Boolean) is
      pragma Unreferenced (Loan, Mapping);
   begin Accepted := Owner.Return_OK; end Release;
   procedure Save (Owner : in out Context; Handle : out W.Word; Accepted : out Boolean) is
   begin Handle := Owner.Saved_Handle; Accepted := Owner.Save_OK; end Save;
   procedure Answer (Owner : in out Context; Handle : W.Word; Operation : Loans.Report; Delivered : out Boolean) is
      pragma Unreferenced (Handle, Operation);
   begin Delivered := Owner.Answer_OK; end Answer;
   procedure Prepare (Owner : in out Context; Now, Deadline : out W.Word; Clock_Usable, Ready : out Boolean) is
   begin Now := Owner.Now; Deadline := Owner.Deadline; Clock_Usable := Owner.Clock_OK; Ready := Owner.Local_OK; end Prepare;
   procedure Firmware (Owner : in out Context; Handled : out Boolean) is
   begin Handled := Owner.Firmware_Work; end Firmware;
   procedure Poll (Owner : in out Context; Kind : out Request_Kind; Sender, Loan : out W.Word;
                   Stamp : out ACPI_Protocol.Role_Tag; Length : out W.Word) is
   begin Kind := Owner.Kind; Sender := Owner.Peer; Loan := Owner.Loan; Stamp := ACPI_Protocol.Observer_Tag; Length := Owner.Length; end Poll;
   procedure Immediate (Owner : in out Context; Sender : W.Word; Result : Sessions.Report; Delivered : out Boolean) is
      pragma Unreferenced (Sender, Result);
   begin Delivered := Owner.Answer_OK; end Immediate;
   procedure Wait_Activity (Owner : in out Context; Deadline : W.Word; Usable : out Boolean) is
      pragma Unreferenced (Deadline);
   begin Usable := Owner.Wait_OK; end Wait_Activity;
   procedure Run (Item : in out Pump.State; Owner : in out Context; Delay_Ms : Pump.Retry_Interval;
                  Request_Data, Response_Data : out P.Byte_Array; Result : out Pump.Report) is
   begin Pump.Step (Item, Owner, Delay_Ms, Request_Data, Response_Data, Result); end Run;
end Query_Pump_Proof_Model;
