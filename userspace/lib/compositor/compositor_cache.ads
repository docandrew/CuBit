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
   type Slot is range 0 .. 137;
   subtype Color_Slot is Slot range 0 .. 9;
   subtype Target_Slot is Slot range 0 .. 1;
   subtype Source_Slot is Slot range 2 .. 9;
   subtype Mask_Slot is Slot range 10 .. Slot'Last;
   subtype Retained_Source_Slot is Slot range 2 .. Slot'Last;
   subtype Batch_Count is Natural range 0 .. 32;
   subtype Batch_Index is Positive range 1 .. 32;
   type Mask_Indices is array (Batch_Index) of Mask_Slot;
   type Handle_Batch is array (Batch_Index) of Handle;
   type State is private;
   function Mode (S : State) return Compositor_Policy.State;
   function Attempted (S : State) return Boolean;
   function Can_Retire (S : State) return Boolean;
   function Empty (S : State; Index : Slot) return Boolean;
   function Targets_Clear (S : State) return Boolean;
   function Views_Clear (S : State) return Boolean;
   procedure Initialize (S : in out State; Opt_In : Boolean)
     with Pre => not Attempted (S), Post => Attempted (S) and Can_Retire (S);
   procedure Forget (S : in out State; Index : Slot)
     with Pre => Can_Retire (S),
       Post => (if Can_Retire (S) then Empty (S, Index)) and
         (for all J in Slot => (if J /= Index then Empty (S, J) = Empty (S'Old, J)));
   -- Retires destination imports only. Other views and foreign contexts must
   -- not alias the allocation the caller intends to release.
   procedure Forget_Targets (S : in out State)
     with Pre => Can_Retire (S),
       Post => (if Can_Retire (S) then Targets_Clear (S)) and
         (for all J in Retained_Source_Slot => Empty (S, J) = Empty (S'Old, J));
   procedure Ensure (S : in out State; Index : Color_Slot;
                     Description : Compositor_Formats.Image;
                     Capacity : Compositor_Formats.Byte_Count; Success : out Boolean)
     with Pre => Can_Retire (S), Post => (if Success then Can_Retire (S));
   -- Mask handles share the same library, retirement gate and shutdown walk.
   -- They cannot consume client-image slots or escape context-owned teardown.
   generic
      with function Valid_Mask (Description : Compositor_Formats.Image;
                                Capacity : Compositor_Formats.Byte_Count) return Boolean;
      with procedure Import_Mask
        (Library : in out Library_State; Description : Compositor_Formats.Image;
         View : out Handle);
   procedure Ensure_Mask
     (S : in out State; Index : Mask_Slot; Description : Compositor_Formats.Image;
      Capacity : Compositor_Formats.Byte_Count; Success : out Boolean)
     with Pre => Can_Retire (S),
       Post => (if Success then Can_Retire (S) and not Empty (S, Index)) and
         (for all J in Slot => (if J /= Index then Empty (S, J) = Empty (S'Old, J)));
   procedure Ensure_Source (S : in out State; Description : Compositor_Formats.Image;
                            Capacity : Compositor_Formats.Byte_Count;
                            Index : out Source_Slot; Success : out Boolean)
     with Pre => Can_Retire (S), Post => (if Success then Can_Retire (S));
   procedure Forget_Source (S : in out State; Pixels : System.Address)
     with Pre => Can_Retire (S);
   -- Alternate validated draw commands share the same retained imports and
   -- completion state machine. Validation runs before any foreign rendering.
   generic
      with function Fits_Views (Source, Target : Compositor_Formats.Image) return Boolean;
      with procedure Draw_Views (Library : in out Library_State; Target, Source : Handle;
                                 Result : out Compositor_Policy.Completion);
   procedure Render_Checked (S : in out State; Target, Source : Slot; Success : out Boolean)
     with Pre => Can_Retire (S),
       Post => (if Success then Mode (S) = Compositor_Policy.Ready);
   procedure Render (S : in out State; Target, Source : Slot;
                     Description : Compositor_Formats.Draw; Success : out Boolean)
     with Pre => Can_Retire (S),
       Post => (if Success then Mode (S) = Compositor_Policy.Ready);
   generic
      with function Fits_Source (I : Batch_Index; Source, Target : Compositor_Formats.Image) return Boolean;
      with procedure Draw_Batch
        (Library : in out Library_State; Target : Handle; Sources : Handle_Batch;
         Length : Batch_Count; Result : out Compositor_Policy.Completion);
   procedure Render_Masks
     (S : in out State; Target : Target_Slot; Sources : Mask_Indices;
      Length : Batch_Count; Success : out Boolean)
     with Pre => Can_Retire (S),
       Post => (if Success then Mode (S) = Compositor_Policy.Ready);
   procedure Shutdown (S : in out State)
     with Pre => Can_Retire (S), Post => (if Can_Retire (S) then Views_Clear (S));
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
   function Empty (S : State; Index : Slot) return Boolean is
     (S.Views (Index).View = No_Handle);
   function Targets_Clear (S : State) return Boolean is
     (for all I in Target_Slot => Empty (S, I));
   function Views_Clear (S : State) return Boolean is
     (for all I in Slot => Empty (S, I));
end Compositor_Cache;
