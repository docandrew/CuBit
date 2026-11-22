pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_IRQ_Owner;
procedure Locked_Pad_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type G.Outcome, G.Pad_Number, G.Memory_Resource_Ordinal;
   use type I.Word, I.Tick, I.Ticket;
   Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 209, 16#0096_0000#, 16#700#, 65_536);
   Captured_Lock : constant I.Word := 16#000E_F5FF#;
   Mask : constant I.Word := 16#0002_0000#;
   Neighbor : constant I.Word := 16#8000_0000#;
   Good : constant I.Word := I.Input_Configuration (16#9000_0003#, I.Level_Low);
   Owner, Host, Lock, Config, Enabled, Pending : I.Word := 0;
   Ticks : I.Tick := 10;
   Reads, Writes, Config_Writes, Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end Check;
   procedure Clock (Now : out I.Tick; Available : out Boolean) is
   begin Ticks:=Ticks+1;Now:=Ticks;Available:=True;end Clock;
   procedure Read (ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Reads:=Reads+1;Status:=I.Access_OK;
      Value:=(case ID is when I.Pad_Owner=>Owner,when I.Host_Owner=>Host,
         when I.Configuration_Lock=>Lock,when I.Pad_Configuration=>Config,
         when I.Group_Enable=>Enabled,when I.Group_Status=>Pending);
   end Read;
   procedure Write (ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
   begin
      Writes:=Writes+1;Status:=I.Access_OK;
      case ID is
         when I.Write_Configuration=>
            Config_Writes:=Config_Writes+1;
            Check((Lock and Mask)=0,"locked PADCFG must never receive a configuration write");Config:=Value;
         when I.Write_Enable=>
            Check(((Value xor Enabled) and not Mask)=0,"neighbor enable bits preserved");Enabled:=Value;
         when I.Clear_Status=>
            Check(Value=Mask,"W1C targets only selected pad");Pending:=Pending and not Value;
      end case;
   end Write;
   package E is new Intel_GPIO_IRQ_Owner (Clock, Read, Write);
   use type E.Phase,E.Admission,E.Failure;
   procedure Until_Phase (S : in out E.State; Target : E.Phase) is
   begin
      for Step in 1..32 loop
         exit when E.Observe(S).At_Phase in E.Faulted or else E.Observe(S).At_Phase=Target;
         E.Advance(S);
      end loop;
      Check(E.Observe(S).At_Phase=Target,"bounded owner reaches expected semantic boundary");
   end Until_Phase;
   type Case_Kind is (Compatible, Other_Pad_Locked, Wrong_Mode, RX_Disabled,
      TX_Enabled, Wrong_Route, Wrong_Trigger, Wrong_Polarity, Foreign_Owner,
      ACPI_Mode, Rearm_Changed);
   procedure Run (Kind : Case_Kind) is
      S : E.State; A : E.Admission; Before : Natural; Event : I.Ticket;
   begin
      Owner:=0;Host:=Mask;Lock:=Captured_Lock;Config:=Good;
      Enabled:=Neighbor or Mask;Pending:=Neighbor or Mask;
      Reads:=0;Writes:=0;Config_Writes:=0;
      case Kind is
         when Other_Pad_Locked=>Lock:=Captured_Lock and not Mask;Config:=Good and not I.TX_Disable;
         when Wrong_Mode=>Config:=Config or 16#400#;
         when RX_Disabled=>Config:=Config or I.RX_Disable;
         when TX_Enabled=>Config:=Config and not I.TX_Disable;
         when Wrong_Route=>Config:=Config or 16#0010_0000#;
         when Wrong_Trigger=>Config:=Config xor I.Edge_Selection;
         when Wrong_Polarity=>Config:=Config xor I.Receive_Invert;
         when Foreign_Owner=>Owner:=Interfaces.Shift_Left(I.Word'(1),Natural(Plan.Selected.Owner_Field_Shift));
         when ACPI_Mode=>Host:=0;
         when others=>null;
      end case;
      E.Start_Parked(S,Plan,I.Level_Low,50_000,32,A);Check(A=E.Accepted,"exact profile admitted once");
      if Kind not in Compatible|Other_Pad_Locked|Rearm_Changed then
         Until_Phase(S,E.Faulted);
         Check(Writes=0 and not E.Observe(S).Effects_Possible,"refused ownership/configuration performs no writes");
         if Kind=Foreign_Owner then
            Check(E.Observe(S).Reason=E.Ownership_Refused and E.Observe(S).Failed_At=E.Read_Owner,
               "PAD_OWN refusal identified before host/lock reads");
         elsif Kind=ACPI_Mode then
            Check(E.Observe(S).Reason=E.Ownership_Refused and E.Observe(S).Failed_At=E.Read_Host,
               "HOSTSW ACPI mode is never offered as a parent IRQ source");
         else
            Check(E.Observe(S).Reason=E.Locked_Configuration_Refused and E.Observe(S).Failed_At=E.Read_Config,
               "incompatible locked input is distinct from ownership refusal");
         end if;
         Before:=Reads+Writes;for Step in 1..4 loop E.Advance(S);end loop;
         Check(Reads+Writes=Before,"terminal refusal never retries MMIO");return;
      end if;
      Until_Phase(S,E.Held_Masked);
      Check((Enabled and Mask)=0 and (Pending and Mask)=0 and (Pending and Neighbor)/=0,
         "locked compatible input is parked with neighbor status retained");
      Check(Config=Good and Config_Writes=(if Kind=Other_Pad_Locked then 1 else 0),
         "captured target lock reuses exact configuration; other pad locks do not forbid configuration");
      E.Resume(S,50_000,32,A);Check(A=E.Accepted,"resume known masked owner");Until_Phase(S,E.Listening);
      Pending:=Pending or Mask;E.Poll(S,50_000,32,A);Check(A=E.Accepted,"IRQ sweep samples current source");
      Until_Phase(S,E.Event_Ready);Event:=E.Observe(S).Event;
      Check(Event/=I.No_Ticket and E.Observe(S).Event_Pending and (Enabled and Mask)=0,
         "masked retained event has an exact ticket");
      if Kind=Rearm_Changed then Config:=Config xor I.Receive_Invert;end if;
      Before:=Writes;E.Rearm(S,Event,50_000,32,A);Check(A=E.Accepted,"exact event acknowledgement accepted");
      if Kind=Rearm_Changed then
         Until_Phase(S,E.Faulted);Check(E.Observe(S).Reason=E.Locked_Configuration_Refused and Writes=Before,
            "locked configuration is revalidated before re-enabling");return;
      end if;
      Until_Phase(S,E.Listening);E.Pause(S,50_000,32,A);Check(A=E.Accepted,"pause masks without configuration");
      Until_Phase(S,E.Held_Masked);
      Pending:=Pending or Mask;E.Resume(S,50_000,32,A);Check(A=E.Accepted,"resume preserves latched event");
      Until_Phase(S,E.Listening);Check((Pending and Mask)/=0,"resume never clears a new masked event");
      Check(Config=Good and Config_Writes=(if Kind=Other_Pad_Locked then 1 else 0),
         "complete lifecycle never writes locked PADCFG");
   end Run;
begin
   Check(Plan.Status=G.Ready and Plan.Selected.Pad=136 and Plan.Selected.Resource=1
      and Plan.Selected.Interrupt_Mask=Mask,"image32 ACPI209 maps to physical pad136 community1 bit17");
   for Kind in Case_Kind loop Run(Kind);end loop;
   Ada.Text_IO.Put_Line("GPIO locked pad checks" & Checks'Image);
end Locked_Pad_Tests;
