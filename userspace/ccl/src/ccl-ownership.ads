with Interfaces;

package CCL.Ownership with
   SPARK_Mode => On
is
   use Interfaces;

   MAX_TYPES        : constant := 32;
   MAX_BINDINGS     : constant := 32;
   MAX_DISPOSITIONS : constant := 8;
   MAX_RO_BORROWS   : constant := 8;

   subtype Type_Id is Natural range 0 .. MAX_TYPES - 1;
   subtype Binding_Id is Natural range 0 .. MAX_BINDINGS - 1;
   subtype Disposition_Id is Unsigned_8;
   subtype Disposition_Count is Natural range 0 .. MAX_DISPOSITIONS;
   subtype Borrow_Count is Natural range 0 .. MAX_RO_BORROWS;

   type Ownership_Mode is (Unrestricted, Move_Only, Must_Handle);
   type Disposition_Effect is (Consume, Transfer, Transition);

   type Disposition is record
      Verb      : Disposition_Id := 0;
      Effect    : Disposition_Effect := Consume;
      Next_Type : Type_Id := 0;
   end record;

   type Disposition_Array is
     array (Natural range 0 .. MAX_DISPOSITIONS - 1) of Disposition;

   type Type_Definition is record
      Mode                : Ownership_Mode := Unrestricted;
      Dispositions_Length : Disposition_Count := 0;
      Dispositions        : Disposition_Array := [others => (others => <>)];
   end record;

   type Type_Table is array (Type_Id) of Type_Definition;

   type Binding_State is
     (Not_Declared, Available, Moved, Handled, Explicitly_Discarded);

   type Ownership_Error is
     (Ownership_Valid,
      Binding_Not_Declared,
      Binding_Already_Declared,
      Value_Not_Available,
      Copy_Requires_Unrestricted,
      Drop_Requires_Unrestricted_Or_Move_Only,
      Unknown_Disposition,
      Read_Borrow_Limit,
      Borrow_Conflict,
      No_Matching_Borrow,
      Branch_Ownership_Mismatch,
      Outstanding_Must_Handle,
      Outstanding_Move_Only,
      Outstanding_Borrow);

   type Environment is private;

   procedure Initialize (Item : out Environment);

   procedure Declare_Binding
     (Item    : in out Environment;
      Binding : Binding_Id;
      Kind    : Type_Id;
      Error   : out Ownership_Error);

   procedure Copy_Value
     (Item    : Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Move_Value
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Drop_Value
     (Item    : in out Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Apply_Disposition
     (Item    : in out Environment;
      Types   : Type_Table;
      Binding : Binding_Id;
      Verb    : Disposition_Id;
      Error   : out Ownership_Error);

   procedure Borrow_RO
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Return_RO
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Borrow_RW
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Return_RW
     (Item    : in out Environment;
      Binding : Binding_Id;
      Error   : out Ownership_Error);

   procedure Join
     (Left, Right : Environment;
      Result      : out Environment;
      Error       : out Ownership_Error);

   procedure Check_Scope
     (Item  : Environment;
      Types : Type_Table;
      Error : out Ownership_Error);

   function State
     (Item : Environment; Binding : Binding_Id) return Binding_State;

   function Kind
     (Item : Environment; Binding : Binding_Id) return Type_Id;

   function Read_Borrows
     (Item : Environment; Binding : Binding_Id) return Borrow_Count;

   function Has_Write_Borrow
     (Item : Environment; Binding : Binding_Id) return Boolean;

   function Combine
     (Left, Right : Ownership_Mode) return Ownership_Mode;

private
   type Binding_Record is record
      State      : Binding_State := Not_Declared;
      Kind       : Type_Id := 0;
      RO_Borrows : Borrow_Count := 0;
      RW_Borrow  : Boolean := False;
   end record;

   type Binding_Array is array (Binding_Id) of Binding_Record;

   type Environment is record
      Bindings : Binding_Array := [others => (others => <>)];
   end record;
end CCL.Ownership;
