package body CCL.Ownership with
   SPARK_Mode => On
is
   function Has_Borrow (Item : Binding_Record) return Boolean is
     (Item.RO_Borrows > 0 or else Item.RW_Borrow);

   function Increment_Borrow (Item : Borrow_Count) return Borrow_Count is
     (case Item is
         when 0 => 1, when 1 => 2, when 2 => 3, when 3 => 4,
         when 4 => 5, when 5 => 6, when 6 => 7, when 7 => 8,
         when 8 => 8);

   function Decrement_Borrow (Item : Borrow_Count) return Borrow_Count is
     (case Item is
         when 0 => 0, when 1 => 0, when 2 => 1, when 3 => 2,
         when 4 => 3, when 5 => 4, when 6 => 5, when 7 => 6,
         when 8 => 7);

   procedure Initialize (Item : out Environment) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Declare_Binding
     (Item    : in out Environment;
      Binding : Binding_Id;
      Kind    : Type_Id;
      Error   : out Ownership_Error) is
   begin
      if Item.Bindings (Binding).State /= Not_Declared then
         Error := Binding_Already_Declared;
      else
         Item.Bindings (Binding) :=
           (State => Available, Kind => Kind, others => <>);
         Error := Ownership_Valid;
      end if;
   end Declare_Binding;

   procedure Copy_Value
     (Item    : Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Error   : out Ownership_Error) is
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
      elsif Item.Bindings (Binding).State /= Available then
         Error := Value_Not_Available;
      elsif Item.Bindings (Binding).RW_Borrow then
         Error := Borrow_Conflict;
      elsif Types (Item.Bindings (Binding).Kind).Mode /= Unrestricted then
         Error := Copy_Requires_Unrestricted;
      else
         Error := Ownership_Valid;
      end if;
   end Copy_Value;

   procedure Move_Value
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error) is
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
      elsif Item.Bindings (Binding).State /= Available or else
        Has_Borrow (Item.Bindings (Binding))
      then
         Error := Value_Not_Available;
      else
         Item.Bindings (Binding).State := Moved;
         Error := Ownership_Valid;
      end if;
   end Move_Value;

   procedure Drop_Value
     (Item    : in out Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Error   : out Ownership_Error) is
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
      elsif Item.Bindings (Binding).State /= Available or else
        Has_Borrow (Item.Bindings (Binding))
      then
         Error := Value_Not_Available;
      elsif Types (Item.Bindings (Binding).Kind).Mode = Must_Handle then
         Error := Drop_Requires_Unrestricted_Or_Move_Only;
      else
         Item.Bindings (Binding).State := Explicitly_Discarded;
         Error := Ownership_Valid;
      end if;
   end Drop_Value;

   procedure Apply_Disposition
     (Item    : in out Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Verb    : Disposition_Id;
      Error   : out Ownership_Error)
   is
      Definition : Type_Definition;
      Found      : Boolean := False;
      Selected   : Disposition := (others => <>);
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
         return;
      elsif Item.Bindings (Binding).State /= Available or else
        Has_Borrow (Item.Bindings (Binding))
      then
         Error := Value_Not_Available;
         return;
      end if;

      Definition := Types (Item.Bindings (Binding).Kind);
      if Definition.Dispositions_Length > 0 then
         for Index in 0 .. Definition.Dispositions_Length - 1 loop
            if Definition.Dispositions (Index).Verb = Verb then
               Found := True;
               Selected := Definition.Dispositions (Index);
               exit;
            end if;
         end loop;
      end if;

      if not Found then
         Error := Unknown_Disposition;
      elsif Selected.Effect = Transition then
         Item.Bindings (Binding).Kind := Selected.Next_Type;
         Error := Ownership_Valid;
      else
         Item.Bindings (Binding).State := Handled;
         Error := Ownership_Valid;
      end if;
   end Apply_Disposition;

   procedure Borrow_RO
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error)
   is
      Current : constant Borrow_Count := Item.Bindings (Binding).RO_Borrows;
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
      elsif Item.Bindings (Binding).State /= Available then
         Error := Value_Not_Available;
      elsif Item.Bindings (Binding).RW_Borrow then
         Error := Borrow_Conflict;
      elsif Current = MAX_RO_BORROWS then
         Error := Read_Borrow_Limit;
      else
         Item.Bindings (Binding).RO_Borrows :=
           Increment_Borrow (Current);
         Error := Ownership_Valid;
      end if;
   end Borrow_RO;

   procedure Return_RO
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error)
   is
      Current : constant Borrow_Count := Item.Bindings (Binding).RO_Borrows;
   begin
      if Current = 0 then
         Error := No_Matching_Borrow;
      else
         Item.Bindings (Binding).RO_Borrows :=
           Decrement_Borrow (Current);
         Error := Ownership_Valid;
      end if;
   end Return_RO;

   procedure Borrow_RW
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error) is
   begin
      if Item.Bindings (Binding).State = Not_Declared then
         Error := Binding_Not_Declared;
      elsif Item.Bindings (Binding).State /= Available then
         Error := Value_Not_Available;
      elsif Has_Borrow (Item.Bindings (Binding)) then
         Error := Borrow_Conflict;
      else
         Item.Bindings (Binding).RW_Borrow := True;
         Error := Ownership_Valid;
      end if;
   end Borrow_RW;

   procedure Return_RW
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error) is
   begin
      if not Item.Bindings (Binding).RW_Borrow then
         Error := No_Matching_Borrow;
      else
         Item.Bindings (Binding).RW_Borrow := False;
         Error := Ownership_Valid;
      end if;
   end Return_RW;

   procedure Join
     (Left, Right : Environment;
      Result      : out Environment;
      Error       : out Ownership_Error) is
   begin
      Result := Left;
      Error := Ownership_Valid;
      for Binding in Binding_Id loop
         if Left.Bindings (Binding) /= Right.Bindings (Binding) then
            Error := Branch_Ownership_Mismatch;
            return;
         end if;
      end loop;
   end Join;

   procedure Check_Scope
     (Item  : Environment;
      Types : Type_Table;
      Error : out Ownership_Error) is
   begin
      Error := Ownership_Valid;
      for Binding in Binding_Id loop
         if Has_Borrow (Item.Bindings (Binding)) then
            Error := Outstanding_Borrow;
            return;
         elsif Item.Bindings (Binding).State = Available then
            case Types (Item.Bindings (Binding).Kind).Mode is
               when Must_Handle =>
                  Error := Outstanding_Must_Handle;
                  return;
               when Move_Only =>
                  Error := Outstanding_Move_Only;
                  return;
               when Unrestricted => null;
            end case;
         end if;
      end loop;
   end Check_Scope;

   function State
     (Item : Environment; Binding : Binding_Id) return Binding_State is
     (Item.Bindings (Binding).State);

   function Kind
     (Item : Environment; Binding : Binding_Id) return Type_Id is
     (Item.Bindings (Binding).Kind);

   function Combine
     (Left, Right : Ownership_Mode) return Ownership_Mode is
     (if Left = Must_Handle or else Right = Must_Handle then Must_Handle
      elsif Left = Move_Only or else Right = Move_Only then Move_Only
      else Unrestricted);
end CCL.Ownership;
