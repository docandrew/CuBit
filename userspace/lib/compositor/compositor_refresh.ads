-- Bounded sequential refresh; publication never changes a visible snapshot.
generic
   Maximum_Items : Positive;
package Compositor_Refresh with Pure, SPARK_Mode is
   subtype Item_Count is Natural range 0 .. Maximum_Items;
   type Phase is (Idle, Listing, Reading, Ready);
   type State is private;
   function Current (S : State) return Phase;
   function Requested (S : State) return Boolean;
   function Index (S : State) return Item_Count;
   function Count (S : State) return Item_Count;
   procedure Request (S : in out State)
     with Post => Requested (S) and Current (S) = Current (S'Old) and
       Index (S) = Index (S'Old) and Count (S) = Count (S'Old);
   procedure Start (S : in out State)
     with Pre => Current (S) = Idle and Requested (S),
       Post => Current (S) = Listing and not Requested (S) and Index (S) = 0;
   procedure Listed (S : in out State; N : Item_Count)
     with Pre => Current (S) = Listing,
       Post => Count (S) = N and Requested (S) = Requested (S'Old) and
         (if N = 0 then Current (S) = Ready and Index (S) = 0
          else Current (S) = Reading and Index (S) = 1);
   procedure Read_Item (S : in out State)
     with Pre => Current (S) = Reading,
       Post => Count (S) = Count (S'Old) and Requested (S) = Requested (S'Old) and
         (if Index (S'Old) = Count (S'Old) then Current (S) = Ready
          else Current (S) = Reading and Index (S) = Index (S'Old) + 1);
   procedure Publish (S : in out State; Visible : Boolean; Published : out Boolean)
     with Post => (Published = (Current (S'Old) = Ready and not Visible)) and
       (if Published then Current (S) = Idle and Requested (S) = Requested (S'Old)
        else S = S'Old);
private
   type State is record
      Stage : Phase := Idle;
      Wanted : Boolean := False;
      Position, Total : Item_Count := 0;
   end record with Dynamic_Predicate =>
     (if Stage = Reading then Position in 1 .. Total);
   function Current (S : State) return Phase is (S.Stage);
   function Requested (S : State) return Boolean is (S.Wanted);
   function Index (S : State) return Item_Count is (S.Position);
   function Count (S : State) return Item_Count is (S.Total);
end Compositor_Refresh;
