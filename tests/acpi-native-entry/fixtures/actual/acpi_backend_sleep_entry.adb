pragma Ada_2022;
with ACPI_FADT.Registers;
package body ACPI_Backend_Sleep_Entry with SPARK_Mode is
   package R renames ACPI_FADT.Registers;
   package P renames W.P;
   package T renames W.T;
   use type Word, S.Register_Model, S.Action, S.Completion, R.Status, R.Address_Source, R.Space_Kind;
   use type ACPI_Backend_Binding_Types.Resource_Kind;
   use type P.Address_Space, T.Access_Width, Interfaces.Unsigned_8, S.Control_Word;
   Wake_Mask : constant Word := 16#8000#;
   Reduced_Wake_Mask : constant Word := 16#80#;
   Clear_Byte : constant Word := 16#FF#;
   function Port_Window_Matches (Current : ACPI_Backend_Binding_Types.Description;
      Retained : W.Window_Result; Expected_First : Interfaces.Unsigned_16;
      Expected_Count : Interfaces.Unsigned_32; Address, Length : Word;
      Reading : Boolean) return Boolean is
     (Expected_Count in 1 .. 65_536 and then Length /= 0
      and then Address <= Word (Interfaces.Unsigned_16'Last)
      and then Length - 1 <= Word (Interfaces.Unsigned_16'Last) - Address
      and then Current.Current and then Current.Kind = ACPI_Backend_Binding_Types.Port_Resource
      and then Current.Writable and then (not Reading or else Current.Readable)
      and then Current.Base = Word (Expected_First) and then Current.Length = Word (Expected_Count)
      and then Retained.Allowed and then W.Valid (Retained.Value)
      and then Retained.Value.Space = P.IO_Space
      and then Retained.Value.Base = Current.Base and then Retained.Value.Length = Current.Length
      and then Retained.Value.Readable = Current.Readable and then Retained.Value.Writable = Current.Writable
      and then Retained.Value.Delegable = Current.Delegable
      and then Address >= Current.Base and then Address - Current.Base < Current.Length
      and then Length <= Current.Length - (Address - Current.Base));
   function Observe (Item : State) return Report is (Item.Info);
   function Is_Control (Name : Register_Name) return Boolean is (Name in Control_A | Control_B);
   function Is_GPE (Name : Register_Name) return Boolean is (Name in GPE0_Block | GPE1_Block);
   function View (Item : State; Hardware : Hardware_State; Name : Register_Name;
      Offset : Natural; Width : T.Access_Width; Writing : Boolean) return Access_View is
     (Select_Access (Hardware, Item.Items (Name).Window, Item.Items (Name).Space,
       Item.Items (Name).Address + Word (Offset), Width, Writing));
   procedure Prepare (Item : in out State; Hardware : Hardware_State;
      FADT : ACPI_FADT.Description; Held : Windows; Result : out Report) is
      procedure Run is
         procedure Admit (Name : Register_Name; D : Descriptor; OK : out Boolean) is
            Width : constant T.Access_Width := (if Name in Reduced_Control | Reduced_Status or Is_GPE (Name)
              then T.Byte_Access else T.Word_Access);
         begin
            OK := False; Item.Info.At_Register := Name; Item.Items (Name) := D;
            for Other in Register_Name loop
               if Other /= Name and then Item.Items (Other).Present and then Item.Items (Other).Space = D.Space
                 and then (if D.Address <= Item.Items (Other).Address then
                   Item.Items (Other).Address - D.Address < Word (D.Length)
                   else D.Address - Item.Items (Other).Address < Word (Item.Items (Other).Length))
               then Item.Info.Status := Overlapping_Registers; return; end if;
            end loop;
            if not T.Aligned (D.Address, Width) then Item.Info.Status := Bad_Geometry; return; end if;
            if Is_GPE (Name) then
               for Offset in 0 .. D.Length - 1 loop
                  if not Allowed (View (Item, Hardware, Name, Offset, Width, True)) then
                     Item.Info.Status := Window_Unavailable; return;
                  end if;
               end loop;
            else
               if not Allowed (View (Item, Hardware, Name, 0, Width, True))
                 or else (Is_Control (Name) and then not Allowed (View (Item, Hardware, Name, 0, Width, False)))
               then Item.Info.Status := Window_Unavailable; return; end if;
            end if;
            OK := True;
         end Admit;
         OK : Boolean;
      begin
         if ACPI_FADT.Hardware_Reduced (FADT) then
            Item.Info.Model := S.Hardware_Reduced;
            for Name in Reduced_Control .. Reduced_Status loop
               declare Gas : constant ACPI_FADT.Optional_Address :=
                 (if Name = Reduced_Control then FADT.Sleep_Control else FADT.Sleep_Status); begin
                  Item.Info.At_Register := Name;
                  if not Gas.Present or else Gas.Value.Address = 0 then Item.Info.Status := Missing_Register; return;
                  elsif Gas.Value.Space not in 0 | 1 or else Gas.Value.Width /= 8 or else Gas.Value.Bit_Offset /= 0
                    or else Gas.Value.Access_Size not in 0 | 1
                    or else (Gas.Value.Space = 1 and Gas.Value.Address > Word (Interfaces.Unsigned_16'Last))
                  then Item.Info.Status := Bad_Geometry; return; end if;
                  Admit (Name, (True, Held (Name), (if Gas.Value.Space = 0 then P.Memory_Space else P.IO_Space),
                    Gas.Value.Address, 1), OK);
                  if not OK then return; end if;
               end;
            end loop;
         else
            Item.Info.Model := S.PM1A_Only;
            for Name in Control_A .. GPE1_Block loop
               declare
                  Kind : constant ACPI_FADT.Block_Kind :=
                    (case Name is when Control_A => ACPI_FADT.PM1A_Control, when Control_B => ACPI_FADT.PM1B_Control,
                     when Status_A => ACPI_FADT.PM1A_Event, when Status_B => ACPI_FADT.PM1B_Event,
                     when GPE0_Block => ACPI_FADT.GPE0, when others => ACPI_FADT.GPE1);
                  Block : constant R.Block_Result := R.Describe (FADT, Kind, Word'Last);
               begin
                  Item.Info.At_Register := Name;
                  if Block.Code = R.Absent and Name not in Control_A | Status_A then null;
                  elsif Block.Code /= R.Described then Item.Info.Status := Missing_Register; return;
                  elsif Block.Source = R.Extended_Address and then
                    (FADT.Extended (Kind).Value.Bit_Offset /= 0 or else
                     (if Is_GPE (Name) then FADT.Extended (Kind).Value.Access_Size not in 0 | 1
                      else FADT.Extended (Kind).Value.Access_Size not in 0 | 2))
                  then Item.Info.Status := Bad_Geometry; return;
                  else
                     Admit (Name, (True, Held (Name), (if Block.Space = R.System_IO then P.IO_Space else P.Memory_Space),
                       Block.Base, Block.Length), OK);
                     if not OK then return; end if;
                     if Name = Control_B then Item.Info.Model := S.PM1A_And_B; end if;
                  end if;
               end;
            end loop;
         end if;
         Item.Info.Status := Prepared;
      end Run;
   begin
      if Item.Working then Result := (Status => Reentered, Completion => S.Indeterminate, others => <>); return;
      elsif Item.Stage /= Fresh then Result := Item.Info; Result.Status := Already_Prepared; return; end if;
      Item.Working := True; Run; Item.Working := False;
      Item.Stage := (if Item.Info.Status = Prepared then Ready else Failed); Result := Item.Info;
   end Prepare;
   procedure Execute (Item : in out State; Hardware : Hardware_State;
      Command : S.Command; Deadline : Word; Output : out S.Control_Word; Result : out Report) is
      Writes : Natural := 0;
      procedure Fail (Why : Outcome; Unknown : Boolean := False) is
      begin
         Item.Info.Status := Why;
         Item.Info.Completion := (if Unknown or Writes /= 0 then S.Indeterminate else S.Not_Attempted);
      end Fail;
      procedure Datum (Name : Register_Name; Offset : Natural; Width : T.Access_Width;
         Writing : Boolean; Input : Word; Value : out Word; OK : out Boolean) is
         Now : Word; Available : Boolean; Done : S.Completion;
      begin
         Value := 0; OK := False; Item.Info.At_Register := Name;
         Read_Clock (Now, Available);
         if not Available then Fail (Clock_Unavailable); return;
         elsif Item.Clock_Seen and then Now < Item.Last_Time then Fail (Clock_Reversed); return;
         elsif Now >= Item.Until_Time then Fail (Deadline_Expired); return; end if;
         Item.Clock_Seen := True; Item.Last_Time := Now;
         if not Item.Items (Name).Present or else Offset >= Item.Items (Name).Length
           or else T.Octets (Width) > Item.Items (Name).Length - Offset
         then Fail (Bad_Geometry); return; end if;
         declare Borrowed : constant Access_View := View (Item, Hardware, Name, Offset, Width, Writing); begin
            if not Allowed (Borrowed) then Fail (Window_Unavailable); return; end if;
            Item.Info.Transfers := Item.Info.Transfers + 1;
            Transfer (Borrowed, Width, Writing, Input, Value, Done);
         end;
         if Done /= S.Completed then Fail ((if Done = S.Indeterminate then Unknown_Effect else Not_Sent), Done = S.Indeterminate); return; end if;
         if Writing then Writes := Writes + 1; Item.Info.Completed_Writes := Item.Info.Completed_Writes + 1; end if;
         OK := True;
      end Datum;
      procedure Run is
         Value : Word; OK : Boolean;
      begin
         if Deadline = 0 or else Deadline = Word'Last
           or else (Item.Until_Time /= 0 and then Item.Until_Time /= Deadline)
         then Fail (Invalid_Deadline); return; end if;
         Item.Until_Time := Deadline;
         if Item.Seen (Command.Kind) then Fail (Replayed_Action); return; end if;
         Item.Seen (Command.Kind) := True;
         if (Item.Info.Model = S.Hardware_Reduced and Command.Kind not in S.Clear_Wake | S.Reduced_Enable)
           or else (Item.Info.Model /= S.Hardware_Reduced and Command.Kind = S.Reduced_Enable)
           or else (Item.Info.Model /= S.PM1A_And_B and Command.Kind in S.Read_B | S.Stage_B | S.Enable_B)
           or else (Command.Kind in S.Mask_GPEs | S.Clear_GPEs | S.Clear_Wake | S.Read_A | S.Read_B and Command.Value /= 0)
           or else (Command.Kind = S.Reduced_Enable and then
             (Command.Value > 16#FF# or else (Command.Value and not 16#3C#) /= 0 or else (Command.Value and 16#20#) = 0))
           or else (Command.Kind in S.Stage_A | S.Stage_B and (Command.Value and 16#2000#) /= 0)
           or else (Command.Kind in S.Enable_A | S.Enable_B and (Command.Value and 16#2000#) = 0)
         then Fail (Invalid_Action); return; end if;
         case Command.Kind is
            when S.Mask_GPEs | S.Clear_GPEs =>
               for Name in GPE0_Block .. GPE1_Block loop
                  if Item.Items (Name).Present then
                     for I in 0 .. Item.Items (Name).Length / 2 - 1 loop
                        Datum (Name, (if Command.Kind = S.Mask_GPEs then Item.Items (Name).Length / 2 + I else I),
                          T.Byte_Access, True, (if Command.Kind = S.Mask_GPEs then 0 else Clear_Byte), Value, OK);
                        if not OK then return; end if;
                     end loop;
                  end if;
               end loop;
            when S.Clear_Wake =>
               if Item.Info.Model = S.Hardware_Reduced then
                  Datum (Reduced_Status, 0, T.Byte_Access, True, Reduced_Wake_Mask, Value, OK);
                  if not OK then return; end if;
               else
                  for Name in Status_A .. Status_B loop
                     if Item.Items (Name).Present then
                        Datum (Name, 0, T.Word_Access, True, Wake_Mask, Value, OK);
                        if not OK then return; end if;
                     end if;
                  end loop;
               end if;
            when S.Read_A | S.Read_B =>
               Datum ((if Command.Kind = S.Read_A then Control_A else Control_B), 0, T.Word_Access, False, 0, Value, OK);
               if not OK then return; end if;
               if Value > Word (S.Control_Word'Last) then Fail (Bad_Geometry); return; end if;
               Output := S.Control_Word (Value);
            when S.Stage_A | S.Enable_A | S.Stage_B | S.Enable_B =>
               Datum ((if Command.Kind in S.Stage_A | S.Enable_A then Control_A else Control_B),
                 0, T.Word_Access, True, Word (Command.Value), Value, OK);
               if not OK then return; end if;
            when S.Reduced_Enable =>
               Datum (Reduced_Control, 0, T.Byte_Access, True, Word (Command.Value), Value, OK);
               if not OK then return; end if;
         end case;
         Item.Info.Status := Transferred; Item.Info.Completion := S.Completed;
      end Run;
   begin
      Output := 0;
      if Item.Working then Result := (Status => Reentered, Completion => S.Indeterminate, others => <>); return;
      elsif Item.Stage /= Ready then Result := Item.Info; return; end if;
      Item.Working := True; Item.Info.Completion := S.Not_Attempted; Run; Item.Working := False;
      if Item.Info.Status /= Transferred then Item.Stage := Failed; end if;
      Result := Item.Info;
   end Execute;
end ACPI_Backend_Sleep_Entry;
