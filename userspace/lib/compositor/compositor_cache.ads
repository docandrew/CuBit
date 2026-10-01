with Compositor_Formats;
with Compositor_Policy;
with System;
generic
   type Library_State is private;
   type Handle is private;
   No_Handle : Handle;
   with procedure Start (Library : out Library_State; Success : out Boolean);
   with procedure Import_View
     (Library : in out Library_State; Description : Compositor_Formats.Image;
      View : out Handle);
   with procedure Render_View
     (Library : in out Library_State; Target, Source : Handle;
      Description : Compositor_Formats.Draw; Result : out Compositor_Policy.Completion);
   with procedure Release_View (Library : in out Library_State; View : Handle; Safe : out Boolean);
   with procedure Stop (Library : in out Library_State);
package Compositor_Cache with SPARK_Mode is
   use type Compositor_Policy.State;
   --  Fixed private cache of Mesa views, indexed by existing Desktop slots.
   --  No application-visible identities or authority are introduced here.
   type Slot is range 0 .. 9;
   subtype Source_Slot is Slot range 2 .. Slot'Last;
   type State is private;
   function Mode (S : State) return Compositor_Policy.State;
   function Attempted (S : State) return Boolean;
   function Can_Retire (S : State) return Boolean;
   procedure Initialize (S : in out State; Opt_In : Boolean)
     with Pre => not Attempted (S), Post => Attempted (S) and Can_Retire (S);
   procedure Forget (S : in out State; Index : Slot)
     with Pre => Can_Retire (S);
   procedure Ensure (S : in out State; Index : Slot;
                     Description : Compositor_Formats.Image;
                     Capacity : Compositor_Formats.Byte_Count; Success : out Boolean)
     with Pre => Can_Retire (S), Post => (if Success then Can_Retire (S));
   procedure Ensure_Source (S : in out State; Description : Compositor_Formats.Image;
                            Capacity : Compositor_Formats.Byte_Count;
                            Index : out Source_Slot; Success : out Boolean)
     with Pre => Can_Retire (S), Post => (if Success then Can_Retire (S));
   procedure Forget_Source (S : in out State; Pixels : System.Address)
     with Pre => Can_Retire (S);
   procedure Render (S : in out State; Target, Source : Slot;
                     Description : Compositor_Formats.Draw; Success : out Boolean)
     with Pre => Can_Retire (S),
       Post => (if Success then Mode (S) = Compositor_Policy.Ready);
   procedure Shutdown (S : in out State)
     with Pre => Can_Retire (S);
private
   use type Compositor_Policy.State;
   type Cached_View is record
      View : Handle := No_Handle;
      Description : Compositor_Formats.Image;
   end record;
   type Entries is array (Slot) of Cached_View;
   type State is record
      Library : Library_State;
      Started, Tried : Boolean := False;
      Status : Compositor_Policy.State := Compositor_Policy.Legacy;
      Views : Entries;
   end record;
   function Mode (S : State) return Compositor_Policy.State is (S.Status);
   function Attempted (S : State) return Boolean is (S.Tried);
   function Can_Retire (S : State) return Boolean is
     (Compositor_Policy.May_Retire (S.Status));
end Compositor_Cache;
