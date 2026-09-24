pragma Ada_2022;
--  Bounded diagnostic content and irreversible renderer lifecycle. Hardware,
--  synchronization and pointers belong to Boot_Diagnostics, not this model.
package Boot_Panel with SPARK_Mode, Pure is
   Columns : constant := 96;
   subtype Line is String (1 .. Columns);
   type Row is (Heading, Current_Step, Last_Completed, Latest_Detail, First_Error);
   type Lines is array (Row) of Line;
   type Phase is (Unavailable, Active, Retired);
   type State is private with Preelaborable_Initialization;
   function Lifecycle (S : State) return Phase;
   function Failed (S : State) return Boolean;
   function Content (S : State; R : Row) return Line;
   function Fit (Text : String) return Line;
   procedure Initialize (S : in out State)
     with Post => (if Lifecycle (S'Old) = Unavailable then Lifecycle (S) = Active
                   else S = S'Old);
   procedure Retire (S : in out State)
     with Post => Lifecycle (S) = Retired;
   procedure Begin_Step (S : in out State; Text : String)
     with Post => Lifecycle (S) = Lifecycle (S'Old) and
       (if Lifecycle (S'Old) /= Active or Failed (S'Old) then S = S'Old);
   procedure Complete_Step (S : in out State; Text : String)
     with Post => Lifecycle (S) = Lifecycle (S'Old) and
       (if Lifecycle (S'Old) /= Active or Failed (S'Old) then S = S'Old);
   procedure Fail (S : in out State; Text : String)
     with Post => Lifecycle (S) = Lifecycle (S'Old) and
       (if Lifecycle (S'Old) /= Active or Failed (S'Old) then S = S'Old
        else Failed (S) and Content (S, First_Error) = Fit (Text));
   procedure Append (S : in out State; C : Character; Changed : out Boolean)
     with Post => Lifecycle (S) = Lifecycle (S'Old) and
       (if Lifecycle (S'Old) /= Active or Failed (S'Old) then
          S = S'Old and not Changed);
private
   type State is record
      Life : Phase := Unavailable;
      Stopped : Boolean := False;
      Text : Lines := [others => [others => ' ']];
      Pending : Line := [others => ' '];
      Used : Natural range 0 .. Columns := 0;
   end record;
   function Lifecycle (S : State) return Phase is (S.Life);
   function Failed (S : State) return Boolean is (S.Stopped);
   function Content (S : State; R : Row) return Line is (S.Text (R));
end Boot_Panel;
