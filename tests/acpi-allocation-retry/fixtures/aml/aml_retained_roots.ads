with AML_Identity;
with AML_Retained_Identities;
generic
   type Value_Type is private;
   Empty_Value : Value_Type;
   Max_Roots : Positive;
   Max_Incarnation : AML_Retained_Identities.Incarnation_Budget :=
     AML_Retained_Identities.Incarnation'Last;
package AML_Retained_Roots with SPARK_Mode, Pure is
   pragma Unevaluated_Use_Of_Old (Allow);
   use AML_Retained_Identities;
   subtype Root_Count is Natural range 0 .. Max_Roots;
   subtype Root_Index is Positive range 1 .. Max_Roots;
   type Token is private;
   No_Token : constant Token;
   type State is limited private;
   type Model is private;
   type Result_Status is (Ready, Root_Limit, Identity_Exhausted,
                         Invalid_Root, Wrong_Phase, Invalid_Owner);
   type Entry_Phase is (Vacant, Reserved, Published);
   type Read_Result is record
      Status : Result_Status := Invalid_Root;
      Value : Value_Type := Empty_Value;
   end record;
   function Valid (Store : State) return Boolean;
   function Snapshot (Store : State) return Model with Ghost;
   function Owner (Store : State) return AML_Identity.Identity;
   function Last_Incarnation (Store : State) return Incarnation;
   function Pending_Count (Store : State) return Root_Count;
   function Published_Count (Store : State) return Root_Count;
   function Bound (After, Before : Model; New_Owner : AML_Identity.Identity;
                   Clear : Boolean) return Boolean with Ghost;
   function Changed (After, Before : Model; Root : Token;
                     Previous, Phase : Entry_Phase; Value : Value_Type;
                     Issued : Boolean) return Boolean with Ghost;
   function Published_From_Reservation
     (After, Before : Model; Root : Token; Value : Value_Type) return Boolean with Ghost;
   procedure Bind (Store : in out State; Arena : AML_Identity.Identity;
                   Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Bound (Snapshot (Store), Snapshot (Store)'Old, Arena, False)
        else Snapshot (Store) = Snapshot (Store)'Old)
       and then Status in Ready | Invalid_Owner;
   procedure Reset (Store : in out State; Arena : AML_Identity.Identity;
                    Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Bound (Snapshot (Store), Snapshot (Store)'Old, Arena, True)
        else Snapshot (Store) = Snapshot (Store)'Old)
       and then Status in Ready | Invalid_Owner;
   procedure Reserve (Store : in out State; Root : out Token;
                      Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Changed (Snapshot (Store), Snapshot (Store)'Old,
          Root, Vacant, Reserved, Empty_Value, True)
        else Snapshot (Store) = Snapshot (Store)'Old and then Root = No_Token)
       and then Status in Ready | Invalid_Owner | Root_Limit | Identity_Exhausted;
   -- Trusted owner validates and canonicalizes Value before this call.
   -- The registry never interprets or authenticates payload contents.
   procedure Publish (Store : in out State; Root : Token; Value : Value_Type;
                      Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Changed (Snapshot (Store), Snapshot (Store)'Old,
          Root, Reserved, Published, Value, False)
        else Snapshot (Store) = Snapshot (Store)'Old)
       and then Status in Ready | Invalid_Root | Wrong_Phase;
   -- Trusted owner fills a preallocated published result slot. The token,
   -- issuer and every other entry remain exact; no new capacity is needed.
   function Replaced (After, Before : Model; Root : Token; Value : Value_Type)
     return Boolean with Ghost;
   function Discarded_Reservation (After, Before : Model) return Boolean with Ghost;
   procedure Replace (Store : in out State; Root : Token; Value : Value_Type;
                      Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Replaced (Snapshot (Store), Snapshot (Store)'Old, Root, Value)
        else Snapshot (Store) = Snapshot (Store)'Old)
       and then Status in Ready | Invalid_Root | Wrong_Phase;
   function Read_Matches (Store : State; Root : Token; Result : Read_Result)
                          return Boolean with Ghost;
   function Read (Store : State; Root : Token) return Read_Result with
     Pre => Valid (Store), Post => Read_Matches (Store, Root, Read'Result);
   procedure Cancel (Store : in out State; Root : in out Token;
                     Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Root = No_Token and then
          Changed (Snapshot (Store), Snapshot (Store)'Old, Root'Old, Reserved, Vacant, Empty_Value, False)
        else Snapshot (Store) = Snapshot (Store)'Old and then Root = Root'Old)
       and then Status in Ready | Invalid_Root | Wrong_Phase;
   procedure Release (Store : in out State; Root : in out Token;
                      Status : out Result_Status) with
     Pre => Valid (Store), Post => Valid (Store) and then
       (if Status = Ready then Root = No_Token and then
          Changed (Snapshot (Store), Snapshot (Store)'Old, Root'Old, Published, Vacant, Empty_Value, False)
        else Snapshot (Store) = Snapshot (Store)'Old and then Root = Root'Old)
       and then Status in Ready | Invalid_Root | Wrong_Phase;
   function Phase_At (Store : State; Index : Root_Index) return Entry_Phase;
   function Census_Matches (Store : State; Index : Root_Index; Result : Read_Result)
                            return Boolean with Ghost;
   function Published_At (Store : State; Index : Root_Index) return Read_Result with
     Pre => Valid (Store), Post => Census_Matches (Store, Index, Published_At'Result);
private
   use type AML_Identity.Identity;
   type Token is record
      Arena : AML_Identity.Identity := AML_Identity.No_Identity;
      Slot : Root_Count := 0;
      Stamp : Incarnation := 0;
   end record;
   No_Token : constant Token := (others => <>);
   type Entry_Data is record
      Phase : Entry_Phase := Vacant;
      Stamp : Incarnation := 0;
      Value : Value_Type := Empty_Value;
   end record;
   type Entry_Array is array (Root_Index) of Entry_Data;
   type Model is record
      Arena : AML_Identity.Identity := AML_Identity.No_Identity;
      Last : Incarnation := 0;
      Entries : Entry_Array := [others => <>];
   end record;
   type State is limited record
      Data : Model;
   end record;
   function Snapshot (Store : State) return Model is (Store.Data);
   function Owner (Store : State) return AML_Identity.Identity is (Store.Data.Arena);
   function Last_Incarnation (Store : State) return Incarnation is (Store.Data.Last);
   function Phase_At (Store : State; Index : Root_Index) return Entry_Phase is
     (Store.Data.Entries (Index).Phase);
end AML_Retained_Roots;
