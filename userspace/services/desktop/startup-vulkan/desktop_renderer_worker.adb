with System;
with Interfaces.C;
package body Desktop_Renderer_Worker with SPARK_Mode => Off is
   use Interfaces;
   use type Interfaces.C.int;

   --  Mesa compiles shaders on this thread: give it the main thread's stack
   --  (desktop.gpr links with stack-size=16777216), not libc's default.
   Worker_Stack_Bytes : constant := 16 * 1024 * 1024;

   --  Opaque libc storage, larger than musl's pthread_attr_t (56 bytes).
   type Attribute_Storage is array (1 .. 16) of Unsigned_64 with Convention => C;
   subtype Thread_Handle is Unsigned_64;

   function pthread_attr_init (Attr : System.Address) return C.int
     with Import, Convention => C, External_Name => "pthread_attr_init";
   function pthread_attr_setstacksize (Attr : System.Address; Bytes : C.size_t) return C.int
     with Import, Convention => C, External_Name => "pthread_attr_setstacksize";
   function pthread_attr_destroy (Attr : System.Address) return C.int
     with Import, Convention => C, External_Name => "pthread_attr_destroy";
   function pthread_create
     (Thread : access Thread_Handle; Attr : System.Address;
      Entry_Point : System.Address; Argument : System.Address) return C.int
     with Import, Convention => C, External_Name => "pthread_create";
   function pthread_detach (Thread : Thread_Handle) return C.int
     with Import, Convention => C, External_Name => "pthread_detach";

   --  Written by the main thread before pthread_create (which orders them
   --  before the worker's reads), read by the worker.
   In_Configuration : Boolean := False;
   In_Width, In_Height : Unsigned_64 := 0;
   In_Epoch : Compositor_Pool.ID := 0;
   --  Written by the worker before Done; read by the main thread after it.
   Out_Evidence : Compositor_Backend_Selection.Readiness := (others => False)
     with Volatile;
   Out_Diagnostic : Desktop_Renderer_Startup.Pipeline_Diagnostic := (others => <>)
     with Volatile;
   Done : Unsigned_8 := 0 with Atomic, Volatile;

   function Worker (Argument : System.Address) return System.Address
     with Convention => C;
   function Worker (Argument : System.Address) return System.Address is
      pragma Unreferenced (Argument);
      Evidence : Compositor_Backend_Selection.Readiness;
      Diagnostic : Desktop_Renderer_Startup.Pipeline_Diagnostic;
   begin
      Desktop_Renderer_Startup.Run
        (In_Configuration, In_Width, In_Height, In_Epoch, Evidence, Diagnostic);
      Out_Evidence := Evidence;
      Out_Diagnostic := Diagnostic;
      Done := 1;
      return System.Null_Address;
   end Worker;

   procedure Start
     (Configuration : Boolean; Width, Height : Unsigned_64;
      Epoch : Compositor_Pool.ID; Started : out Boolean)
   is
      Attributes : aliased Attribute_Storage := (others => 0);
      Thread : aliased Thread_Handle := 0;
      Ignore : C.int;
   begin
      Started := False;
      In_Configuration := Configuration;
      In_Width := Width;
      In_Height := Height;
      In_Epoch := Epoch;
      if pthread_attr_init (Attributes'Address) /= 0 then
         return;
      end if;
      if pthread_attr_setstacksize (Attributes'Address, Worker_Stack_Bytes) = 0 then
         Started := pthread_create
           (Thread'Access, Attributes'Address, Worker'Address, System.Null_Address) = 0;
      end if;
      Ignore := pthread_attr_destroy (Attributes'Address);
      if Started then
         Ignore := pthread_detach (Thread);
      end if;
   end Start;

   function Finished return Boolean is (Done /= 0);

   procedure Take
     (Evidence : out Compositor_Backend_Selection.Readiness;
      Diagnostic : out Desktop_Renderer_Startup.Pipeline_Diagnostic) is
   begin
      Evidence := Out_Evidence;
      Diagnostic := Out_Diagnostic;
   end Take;
end Desktop_Renderer_Worker;
