with Interfaces.C; use Interfaces.C;
with System;
package body Thread_Exit_Key is
   type Key_Type is new unsigned;
   Key : aliased Key_Type := 0;
   Once : aliased int := 0;

   procedure Thread_Exit with Import, Convention => C, External_Name => "cualloc_thread_exit";

   procedure Destructor (Value : System.Address) with Convention => C;
   procedure Destructor (Value : System.Address) is
      pragma Unreferenced (Value);
   begin
      Thread_Exit;
   end Destructor;

   function pthread_key_create (Key : access Key_Type; Destructor : System.Address) return int
     with Import, Convention => C, External_Name => "pthread_key_create";
   function pthread_setspecific (Key : Key_Type; Value : System.Address) return int
     with Import, Convention => C, External_Name => "pthread_setspecific";
   function pthread_once (Control : access int; Routine : System.Address) return int
     with Import, Convention => C, External_Name => "pthread_once";

   procedure Make with Convention => C;
   procedure Make is
      Ignore : int;
   begin
      Ignore := pthread_key_create (Key'Access, Destructor'Address);
   end Make;

   procedure Arm is
      Ignore : int;
   begin
      Ignore := pthread_once (Once'Access, Make'Address);
      --  Any non-null value: the destructor runs only for non-null ones.
      Ignore := pthread_setspecific (Key, Key'Address);
   end Arm;
end Thread_Exit_Key;
