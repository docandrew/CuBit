pragma Ada_2022;
with Interfaces;
generic
   Final_Receipt : Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
package ACPI_Notification_Receipts with SPARK_Mode is
   pragma Pure;
   use type Interfaces.Unsigned_64;
   subtype Number is Interfaces.Unsigned_64 range 0 .. Final_Receipt;
   type State is private with Default_Initial_Condition => Valid (State)
     and then Current (State) = 0 and then Issued_Count (State) = 0
     and then Last_Acknowledged (State) = 0;
   function Valid (S : State) return Boolean;
   function Current (S : State) return Number;
   function Issued_Count (S : State) return Number;
   function Last_Acknowledged (S : State) return Number;
   function Can_Issue (S : State) return Boolean;
   procedure Issue (S : in out State) with
     Pre => Valid (S) and then Can_Issue (S),
     Post => Valid (S) and then Current (S) = Issued_Count (S'Old) + 1
       and then Issued_Count (S) = Current (S)
       and then Last_Acknowledged (S) = Last_Acknowledged (S'Old);
   type Ack_Result is (Accepted, Repeated, Invalid);
   procedure Acknowledge (S : in out State; ID : Number; Result : out Ack_Result)
     with Pre => Valid (S), Post => Valid (S)
       and then (if Result = Accepted then Current (S) = 0
         and then Last_Acknowledged (S) = ID and then ID = Current (S'Old))
       and then Result =
         (if ID /= 0 and then ID = Current (S'Old) then Accepted
          elsif ID /= 0 and then ID = Last_Acknowledged (S'Old) then Repeated
          else Invalid)
       and then Issued_Count (S) = Issued_Count (S'Old)
       and then (if Result /= Accepted then S = S'Old);
private
   type State is record
      Issued, Active, Acknowledged : Number := 0;
   end record;
   function Valid (S : State) return Boolean is
     (S.Acknowledged <= S.Issued and then
      (if S.Active = 0 then S.Acknowledged = S.Issued
       else S.Active = S.Issued and then S.Acknowledged < S.Active));
   function Current (S : State) return Number is (S.Active);
   function Issued_Count (S : State) return Number is (S.Issued);
   function Last_Acknowledged (S : State) return Number is (S.Acknowledged);
   function Can_Issue (S : State) return Boolean is
     (S.Active = 0 and then S.Issued < Final_Receipt);
end ACPI_Notification_Receipts;
