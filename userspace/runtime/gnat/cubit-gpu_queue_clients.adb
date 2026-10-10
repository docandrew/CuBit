pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body CuBit.GPU_Queue_Clients is

   use type System.Address;
   use type GQ.Timeline_Value, Q.Token;

   --  A status line is rewritten in a few stores; a reader that keeps
   --  colliding with the writer this many times reports failure.
   Status_Tries : constant := 64;

   procedure Barrier;
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;

   function Own_Word (C : Client; Offset : Natural) return System.Address;
   function Driver_Word (C : Client; Offset : Natural) return System.Address;
   function Own_Word (C : Client; Offset : Natural) return System.Address is
     (C.Own + Storage_Offset (Offset));
   function Driver_Word (C : Client; Offset : Natural) return System.Address is
     (C.Driver + Storage_Offset (Offset));

   procedure Read_Status
     (C : Client; Context : GQ.Context_Index; Line : out GQ.Status_Line; OK : out Boolean)
   is
      Lines : constant GQ.Status_Lines with Import, Volatile,
        Address => Driver_Word (C, GQ.Server_Status_At);
      Before, After : Unsigned_64;
   begin
      Line := (others => <>);
      OK := False;
      if C.Driver = System.Null_Address then
         return;
      end if;
      for Try in 1 .. Status_Tries loop
         Before := Lines (Context).Version;
         Barrier;
         Line := Lines (Context);
         Barrier;
         After := Lines (Context).Version;
         if Before mod 2 = 0 and then Before = After then
            OK := True;
            return;
         end if;
      end loop;
   end Read_Status;

   function State (C : Client; Context : GQ.Context_Index) return GQ.Context_State is
      Line : GQ.Status_Line;
      OK : Boolean;
   begin
      Read_Status (C, Context, Line, OK);
      if not OK then
         return GQ.Unused;
      end if;
      for S in GQ.Context_State loop
         if GQ.Context_State'Enum_Rep (S) = Line.State then
            return S;
         end if;
      end loop;
      return GQ.Lost;   --  not a state the driver writes: trust nothing
   end State;

   function Reached (C : Client; Context : GQ.Context_Index; Target : GQ.Timeline_Value)
     return Boolean
   is
      Line : GQ.Status_Line;
      OK : Boolean;
   begin
      if Target = GQ.No_Wait then
         return True;
      end if;
      Read_Status (C, Context, Line, OK);
      return OK and then GQ.Timeline_Value (Line.Completed) >= Target;
   end Reached;

   procedure Attach (C : in out Client; Own_Region, Driver_Region : System.Address) is
      Line : GQ.Status_Line;
      OK : Boolean;
   begin
      C.Own := Own_Region;
      C.Driver := Driver_Region;
      C.Ring := (others => <>);
      C.Next_Tag := 0;
      C.Kicked := 0;
      for Context in GQ.Context_Index loop
         Read_Status (C, Context, Line, OK);
         C.Next (Context) := (if OK then GQ.Timeline_Value (Line.Accepted) + 1 else 0);
      end loop;
   end Attach;

   procedure Detach (C : in out Client) is
   begin
      C.Own := System.Null_Address;
      C.Driver := System.Null_Address;
   end Detach;

   function Attached (C : Client) return Boolean is
     (C.Own /= System.Null_Address and then C.Driver /= System.Null_Address);

   function Can_Submit (C : in out Client) return Boolean is
   begin
      if not Attached (C) then
         return False;
      end if;
      declare
         Taken : constant Unsigned_32 with Import, Volatile,
           Address => Driver_Word (C, GQ.Server_Taken_At);
         Accepted : Boolean;
      begin
         Q.Accept_Taken (C.Ring, Q.Submissions.Index (Taken), Accepted);
      end;
      return Q.Can_Submit (C.Ring);
   end Can_Submit;

   function Outstanding (C : Client) return Natural is (C.Ring.Pending);

   function Next_Signal (C : Client; Context : GQ.Context_Index) return GQ.Timeline_Value is
     (C.Next (Context));

   procedure Submit
     (C : in out Client; Item : Job; Tag : out Token; Signal : out GQ.Timeline_Value;
      Submitted, Kick : out Boolean)
   is
      Armed : Unsigned_32;
   begin
      Tag := 0;
      Signal := 0;
      Submitted := False;
      Kick := False;
      if not Can_Submit (C) then
         return;
      end if;
      declare
         Descriptors : Q.Submissions.Ring with Import,
           Address => Own_Word (C, GQ.Client_Descriptors_At);
         Produced : Unsigned_32 with Import, Volatile,
           Address => Own_Word (C, GQ.Client_Submitted_At);
         Wake : constant Unsigned_32 with Import, Volatile,
           Address => Driver_Word (C, GQ.Server_Wake_At);
         Item_Wire : constant GQ.Descriptor :=
           (Operation      => GQ.Opcode'Enum_Rep (Item.Operation),
            Flags          => 0,
            Context        => Item.Context,
            Wait_1_Context => (if Item.First.Target = GQ.No_Wait then 0
                               else GQ.Nibble (Item.First.Context)),
            Wait_2_Context => (if Item.Second.Target = GQ.No_Wait then 0
                               else GQ.Nibble (Item.Second.Context)),
            Batch_Handle   => Item.Handle,
            Signal_Value   => Unsigned_64 (C.Next (Item.Context)),
            Batch_GPU      => Item.GPU,
            Batch_Offset   => Item.Offset,
            Batch_Bytes    => Item.Bytes,
            Wait_1_Value   => Unsigned_64 (Item.First.Target),
            Wait_2_Value   => Unsigned_64 (Item.Second.Target),
            Deadline       => Unsigned_64 (Item.Deadline));
      begin
         C.Next_Tag := C.Next_Tag + 1;
         Tag := C.Next_Tag;
         Signal := C.Next (Item.Context);
         Q.Submit (C.Ring, Descriptors, Tag, Item_Wire);
         Barrier;
         Produced := Unsigned_32 (C.Ring.Requests.Produced);
         Barrier;
         C.Next (Item.Context) := C.Next (Item.Context) + 1;
         Submitted := True;
         --  The count before the wake word: kick only a driver that
         --  sleeps, once per arming.
         Armed := Wake;
         if Armed /= 0 and then Armed /= C.Kicked then
            C.Kicked := Armed;
            Kick := True;
         end if;
      end;
   end Submit;

   function Records_Waiting (C : in out Client) return Boolean is
   begin
      if not Attached (C) then
         return False;
      end if;
      declare
         Completed : constant Unsigned_32 with Import, Volatile,
           Address => Driver_Word (C, GQ.Server_Completed_At);
         Accepted : Boolean;
      begin
         Q.Completions.Accept_Produced
           (C.Ring.Answers, Q.Completions.Index (Completed), Accepted);
      end;
      return C.Ring.Answers.Available > 0;
   end Records_Waiting;

   procedure Reap (C : in out Client; Item : out Q.Completion; Got : out Boolean) is
   begin
      Item := (Tag => 0, Answer => (others => <>));
      Got := False;
      if not Records_Waiting (C) then
         return;
      end if;
      declare
         Records : constant Q.Completions.Ring with Import,
           Address => Driver_Word (C, GQ.Server_Completions_At);
         Reaped : Unsigned_32 with Import, Volatile,
           Address => Own_Word (C, GQ.Client_Reaped_At);
      begin
         Barrier;
         Q.Reap (C.Ring, Records, Item, Got);
         Barrier;
         Reaped := Unsigned_32 (C.Ring.Answers.Consumed);
      end;
   end Reap;

end CuBit.GPU_Queue_Clients;
