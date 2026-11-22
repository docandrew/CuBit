pragma Ada_2022;
with ACPI_FADT.Registers;
package body ACPI_Backend_Global_Signal with SPARK_Mode is
   package R renames ACPI_FADT.Registers;
   package C renames ACPI_PM1_Global_Release;
   package P renames Bindings.P;
   package T renames Bindings.T;
   use type R.Status, R.Address_Source, R.Space_Kind, C.Completion;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_64, P.Address_Space;
   function Observe (State : Context) return Report is (State.Info);
   function View (State : Context; Hardware : Hardware_State; Kind : Bank; Writing : Boolean)
      return Bindings.Access_View is
     (Select_Access (Hardware, State.Items (Kind).Window, State.Items (Kind).Space,
       State.Items (Kind).Address, T.Word_Access, Writing));
   procedure Prepare
     (State : in out Context; Hardware : Hardware_State;
      FADT : ACPI_FADT.Description; Held : Windows; Result : out Report)
   is
      procedure Run is
      begin
         for Kind in Bank loop
            declare
               Block_Kind : constant ACPI_FADT.Block_Kind :=
                 (if Kind = PM1A then ACPI_FADT.PM1A_Control else ACPI_FADT.PM1B_Control);
               Block : constant R.Block_Result := R.Describe (FADT, Block_Kind, Interfaces.Unsigned_64'Last);
            begin
               State.Info.At_Bank := Kind;
               if Block.Code = R.Absent and Kind = PM1B then null;
               elsif Block.Code /= R.Described then State.Info.Status := Register_Unavailable; return;
               elsif Block.Source = R.Extended_Address and then FADT.Extended (Block_Kind).Value.Bit_Offset /= 0 then
                  State.Info.Status := Bad_Register; return;
               else
                  State.Items (Kind) := (True, Held (Kind),
                    (if Block.Space = R.System_IO then P.IO_Space else P.Memory_Space), Block.Base);
                  if not View (State, Hardware, Kind, False).Allowed
                    or else not View (State, Hardware, Kind, True).Allowed
                  then State.Info.Status := Window_Unavailable; return; end if;
                  State.Info.Banks := State.Info.Banks + 1;
                  State.Info.Extended_Fallback := State.Info.Extended_Fallback or Block.Extended_Rejected;
               end if;
            end;
         end loop;
         if State.Items (PM1B).Present
           and then State.Items (PM1A).Space = State.Items (PM1B).Space
           and then State.Items (PM1A).Address = State.Items (PM1B).Address
         then State.Info.Status := Bad_Register; return; end if;
         State.Info.Status := Prepared;
      end Run;
   begin
      if State.Working then Result := (Status => Reentered, others => <>); return;
      elsif State.Stage /= Fresh then Result := (Status => Already_Consumed, others => <>); return; end if;
      State.Working := True; Run; State.Working := False;
      State.Stage := (if State.Info.Status = Prepared then Ready else Failed); Result := State.Info;
   end Prepare;
   procedure Emit (State : in out Context; Hardware : Hardware_State; Result : out Report) is
      procedure Run is
         type Values is array (Bank) of Interfaces.Unsigned_16;
         Saved : Values := [others => 0];
         Output : Interfaces.Unsigned_16;
         Status : C.Completion;
      begin
         State.Info.Reads := 0; State.Info.Writes := 0;
         for Kind in Bank loop
            if State.Items (Kind).Present then
               State.Info.At_Bank := Kind;
               declare Read_View : constant Bindings.Access_View := View (State, Hardware, Kind, False); begin
                  if not Read_View.Allowed then State.Info.Status := Not_Sent; return; end if;
                  Transfer_Word (Read_View, False, 0, Saved (Kind), Status);
                  if Status /= C.Completed then State.Info.Status := Not_Sent; return; end if;
                  State.Info.Reads := State.Info.Reads + 1;
               end;
            end if;
         end loop;
         for Kind in Bank loop
            if State.Items (Kind).Present then
               State.Info.At_Bank := Kind;
               declare Write_View : constant Bindings.Access_View := View (State, Hardware, Kind, True); begin
                  if not Write_View.Allowed then
                     State.Info.Status := (if State.Info.Writes = 0 then Not_Sent else Indeterminate); return;
                  end if;
                  Transfer_Word (Write_View, True, C.Compose (Saved (Kind)), Output, Status);
                  if Status /= C.Completed then
                     State.Info.Status := (if Status = C.Not_Attempted and State.Info.Writes = 0
                       then Not_Sent else Indeterminate); return;
                  end if;
                  State.Info.Writes := State.Info.Writes + 1;
               end;
            end if;
         end loop;
         State.Info.Status := Signaled;
      end Run;
   begin
      if State.Working then Result := (Status => Reentered, others => <>); return;
      elsif State.Stage /= Ready then Result := State.Info; return; end if;
      State.Working := True; Run; State.Working := False;
      if State.Info.Status = Indeterminate then State.Stage := Uncertain; end if;
      Result := State.Info;
   end Emit;
end ACPI_Backend_Global_Signal;
