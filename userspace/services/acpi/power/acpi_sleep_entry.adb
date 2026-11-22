pragma Ada_2022;
with ACPI_PM1_Global_Release;
package body ACPI_Sleep_Entry with SPARK_Mode is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   type Pair_Step is (Pair_Mask, Pair_Clear_GPE, Pair_Clear_Wake, Pair_Read_A, Pair_Read_B,
      Pair_Stage_A, Pair_Stage_B, Pair_Enable_A, Pair_Enable_B);
   type Single_Step is (Single_Mask, Single_Clear_GPE, Single_Clear_Wake, Single_Read_A, Single_Stage_A, Single_Enable_A);
   type Reduced_Step is (Reduced_Clear_Wake, Reduced_Enter);
   function Rank (Model : Register_Model; Kind : Action) return Command_Count is
     (case Model is
       when No_Registers => Maximum_Commands,
       when PM1A_And_B => (case Kind is
         when Mask_GPEs => Pair_Step'Pos (Pair_Mask), when Clear_GPEs => Pair_Step'Pos (Pair_Clear_GPE),
         when Clear_Wake => Pair_Step'Pos (Pair_Clear_Wake), when Read_A => Pair_Step'Pos (Pair_Read_A),
         when Read_B => Pair_Step'Pos (Pair_Read_B), when Stage_A => Pair_Step'Pos (Pair_Stage_A),
         when Stage_B => Pair_Step'Pos (Pair_Stage_B), when Enable_A => Pair_Step'Pos (Pair_Enable_A),
         when Enable_B => Pair_Step'Pos (Pair_Enable_B), when others => Maximum_Commands),
       when PM1A_Only => (case Kind is
         when Mask_GPEs => Single_Step'Pos (Single_Mask), when Clear_GPEs => Single_Step'Pos (Single_Clear_GPE),
         when Clear_Wake => Single_Step'Pos (Single_Clear_Wake), when Read_A => Single_Step'Pos (Single_Read_A),
         when Stage_A => Single_Step'Pos (Single_Stage_A), when Enable_A => Single_Step'Pos (Single_Enable_A),
         when others => Maximum_Commands),
       when Hardware_Reduced => (case Kind is
         when Clear_Wake => Reduced_Step'Pos (Reduced_Clear_Wake), when Reduced_Enable => Reduced_Step'Pos (Reduced_Enter),
         when others => Maximum_Commands));
   function Valid (Item : State) return Boolean is
     (Item.Info.Confirmed_Writes <= Item.Info.Offered
      and then (if Item.Info.Status = Fresh then Item.Info.Offered = 0 and Item.Info.Confirmed_Writes = 0)
      and then (if Item.Info.Status = Ready then Rank (Item.Model, Item.Info.At_Action) < Maximum_Commands
        and then Item.Info.Offered = Rank (Item.Model, Item.Info.At_Action))
      and then (if Item.Info.Status = Awaiting_Completion then Rank (Item.Model, Item.Info.At_Action) < Maximum_Commands
        and then Item.Info.Offered = Rank (Item.Model, Item.Info.At_Action) + 1
        and then Item.Info.Confirmed_Writes < Item.Info.Offered));
   function Observe (Item : State) return Report is (Item.Info);
   function Writes (Kind : Action) return Boolean is (Kind not in Read_A | Read_B);
   procedure Check_Time (Item : in out State; Clock_Available : Boolean; Now : Tick; Good : out Boolean)
     with Post => Item.Model = Item.Model'Old and then Item.Info.At_Action = Item.Info.At_Action'Old
       and then Item.Info.Offered = Item.Info.Offered'Old
       and then Item.Info.Confirmed_Writes = Item.Info.Confirmed_Writes'Old
       and then (if Good then Item.Info.Status = Item.Info.Status'Old
         else Item.Info.Status in Clock_Unavailable | Clock_Reversed | Window_Expired)
   is
   begin
      Good := False;
      if not Clock_Available then Item.Info.Status := Clock_Unavailable;
      elsif Now < Item.Last_Time then Item.Info.Status := Clock_Reversed;
      elsif Now >= Item.Until_Time then Item.Info.Status := Window_Expired;
      else Item.Last_Time := Now; Good := True;
      end if;
   end Check_Time;
   procedure Start (Item : in out State; Types_Copied : Boolean;
      Registers : Register_Model; Type_A, Type_B : E.Sleep_Type;
      Owner_Ready, Clock_Available : Boolean; Now, Deadline : Tick;
      Result : out Report) is
   begin
      if Item.Info.Status /= Fresh then Result := Item.Info; return; end if;
      Item.Info.Status := Not_Prepared;
      if not Types_Copied then null;
      elsif Registers = No_Registers then Item.Info.Status := Missing_Registers;
      elsif not Owner_Ready then Item.Info.Status := Owner_Unsafe;
      elsif not Clock_Available then Item.Info.Status := Clock_Unavailable;
      elsif Deadline = 0 or Deadline = Tick'Last or Now >= Deadline then Item.Info.Status := Invalid_Window;
      else
         Item.Model := Registers; Item.A := Type_A; Item.B := Type_B;
         Item.Last_Time := Now; Item.Until_Time := Deadline;
         Item.Info.Status := Ready;
         Item.Info.At_Action := (if Registers = Hardware_Reduced then Clear_Wake else Mask_GPEs);
      end if;
      Result := Item.Info;
   end Start;
   procedure Take (Item : in out State; Owner_Ready, Clock_Available : Boolean;
      Now : Tick; Value : out Command; Available : out Boolean; Result : out Report) is
      Good : Boolean;
   begin
      Value := (others => <>); Available := False;
      if Item.Info.Status not in Ready | Settling then Result := Item.Info; return; end if;
      if not Owner_Ready then Item.Info.Status := Owner_Unsafe; Result := Item.Info; return; end if;
      Check_Time (Item, Clock_Available, Now, Good);
      if not Good then Result := Item.Info; return; end if;
      if Item.Info.Status = Settling then
         if Now - Item.Enable_Time >= Settle_Microseconds then Item.Info.Status := Still_Running; end if;
         Result := Item.Info; return;
      end if;
      Value.Kind := Item.Info.At_Action;
      case Value.Kind is
         when Stage_A | Enable_A => Value.Value := E.PM1_Sleep_Value (Item.Saved_A, Item.A, Value.Kind = Enable_A);
         when Stage_B | Enable_B => Value.Value := E.PM1_Sleep_Value (Item.Saved_B, Item.B, Value.Kind = Enable_B);
         when Reduced_Enable => Value.Value := Control_Word
           (Interfaces.Shift_Left (Interfaces.Unsigned_8 (Item.A), E.Reduced_SLP_TYP_Shift) or E.Reduced_SLP_EN);
         when others => null;
      end case;
      Item.Info.Offered := Item.Info.Offered + 1;
      Item.Info.Enable_Offered := Item.Info.Enable_Offered or Value.Kind in Enable_A | Enable_B | Reduced_Enable;
      Item.Info.Status := Awaiting_Completion;
      Available := True; Result := Item.Info;
   end Take;
   procedure Confirm (Item : in out State; Status : Completion;
      Read_Value : Control_Word; Clock_Available : Boolean; Now : Tick;
      Result : out Report) is
      Good : Boolean;
   begin
      if Item.Info.Status /= Awaiting_Completion then Result := Item.Info; return; end if;
      if Status /= Completed then
         Item.Info.Status := (if Status = Not_Attempted then Transfer_Refused else Transfer_Indeterminate);
         Result := Item.Info; return;
      end if;
      if Writes (Item.Info.At_Action) then Item.Info.Confirmed_Writes := Item.Info.Confirmed_Writes + 1; end if;
      Check_Time (Item, Clock_Available, Now, Good);
      if not Good then Result := Item.Info; return; end if;
      case Item.Info.At_Action is
         when Mask_GPEs => Item.Info.At_Action := Clear_GPEs;
         when Clear_GPEs => Item.Info.At_Action := Clear_Wake;
         when Clear_Wake => Item.Info.At_Action := (if Item.Model = Hardware_Reduced then Reduced_Enable else Read_A);
         when Read_A => Item.Saved_A := Read_Value and not ACPI_PM1_Global_Release.Command_Bits;
            Item.Info.At_Action := (if Item.Model = PM1A_And_B then Read_B else Stage_A);
         when Read_B => Item.Saved_B := Read_Value and not ACPI_PM1_Global_Release.Command_Bits; Item.Info.At_Action := Stage_A;
         when Stage_A => Item.Info.At_Action := (if Item.Model = PM1A_And_B then Stage_B else Enable_A);
         when Stage_B => Item.Info.At_Action := Enable_A;
         when Enable_A =>
            if Item.Model = PM1A_And_B then Item.Info.At_Action := Enable_B;
            else Item.Info.Status := Settling; Item.Enable_Time := Now; end if;
         when Enable_B | Reduced_Enable => Item.Info.Status := Settling; Item.Enable_Time := Now;
      end case;
      if Item.Info.Status = Awaiting_Completion then Item.Info.Status := Ready; end if;
      Result := Item.Info;
   end Confirm;
end ACPI_Sleep_Entry;
