with Intel_GPU_VM_Buffer;
with Intel_GPU_ADLN_PPGTT;
package body Intel_GPU_Buffer_Requests.Binding is
   package Binder is new Intel_GPU_VM_Buffer (VM);
   function Closed_Buffer_Disjoint
     (Object : Service; Image : VM.Image; Session, ID : Unsigned_64)
      return Boolean is
      Backing : Intel_GPU_Buffer_Reply.Backing;
      function Conflicts (Page : Unsigned_64) return Boolean is
        (Intel_GPU_Buffer_Reply.Overlaps_DMA (Backing, Page, 4096));
      function Disjoint is new VM.Backing_Disjoint (Conflicts);
   begin
      if Object.Failed or else not Owner_Ready or else Session = 0 or else
        ID = 0 or else ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
        or else not VM.Sealed (Image) then return False; end if;
      Backing := Intel_GPU_Buffer_Handles.Closed_Backing
        (Object.Handles, Session, Intel_GPU_Buffer_Handles.Handle (ID));
      return Intel_GPU_Buffer_Reply.Valid (Backing) and then Disjoint (Image);
   end Closed_Buffer_Disjoint;
   procedure Handle_Update
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      Before : constant Unsigned_64 := Coordinator.Generation (State);
      Preparation : Preparation_Result;
      Result : Coordinator.Result;
      use type Coordinator.Result;
   begin
      Response := [Denied, Version, 0, 0];
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Response (0) := Unavailable;
      if not Coordinator.Can_Submit (State) then return; end if;
      Prepare_Request (Object, Source, Candidate, Tables, VM_Session, Before,
        Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Preparation);
      case Preparation is
         when Request_Denied => Response (0) := Denied; return;
         when Malformed => Response (0) := Bad_Request; return;
         when Stale_Generation | Not_Ready | Eligible => return;
         when Prepared => null;
      end case;
      Coordinator.Execute (State, Before, Result);
      if Result /= Coordinator.Complete then return; end if;
      -- Callback-driven event pumping may retire a session. Never acknowledge
      -- a committed update to a now-stale authority, nor reopen submission.
      if Object.Failed or else not Owner_Ready or else
        Session_Of (Sender, Stamp) /= VM_Session or else
        not Coordinator.Can_Submit (State) or else
        Coordinator.Generation (State) /= Before + 1
      then
         Coordinator.Fail (State);
         return;
      end if;
      Response := [OK, Version, Coordinator.Generation (State), 0];
   end Handle_Update;
   procedure Check_Update_Request
     (Object : Service; Source : VM.Image;
      VM_Session, Current_Generation, Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Status : out Preparation_Result)
   is
      Operation : constant Unsigned_64 := Shift_Right (Request (0), 16) and 16#FFFF#;
      Expected : constant Unsigned_64 := Shift_Right (Request (0), 32);
      ID : constant Unsigned_64 := Request (1) and 16#FFFF_FFFF#;
      Offset : constant Unsigned_64 := Shift_Right (Request (1), 32) * 4096;
      GPU : constant Unsigned_64 := Request (2);
      Bytes : constant Unsigned_64 := Request (3);
      Limit : constant Unsigned_64 :=
        Unsigned_64 (Intel_GPU_Buffer_Backing.Page_Count'Last) * 4096;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   begin
      Status := Request_Denied;
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Status := Malformed;
      if Request_Label /= Update_Label or else Length /= 4 or else Flags /= 0 or else
        Reserved /= 0 or else (Request (0) and 16#FFFF#) /= Version or else
        Operation > 1 or else ID = 0 or else GPU = 0 or else GPU >= 2 ** 48 or else
        GPU mod 4096 /= 0 or else Bytes = 0 or else Bytes mod 4096 /= 0 or else
        Bytes > Limit or else Offset > Limit - Bytes or else Bytes > 2 ** 48 - GPU
      then return; end if;
      Status := Not_Ready;
      -- Never wrap the 32-bit wire generation, nor accept work after loss.
      if Object.Failed or else not Owner_Ready or else
        Current_Generation >= Unsigned_64 (Unsigned_32'Last) then return; end if;
      Status := Stale_Generation;
      if Expected /= Current_Generation then return; end if;
      Status := Not_Ready;
      if not VM.Sealed (Source) or else
        ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
      then return; end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if not Backing.Ready or else Offset > Backing.Bytes or else
        Bytes > Backing.Bytes - Offset or else not Owner_Ready or else
        Session_Of (Sender, Stamp) /= VM_Session or else Object.Failed
      then return; end if;
      Status := Eligible;
   end Check_Update_Request;
   procedure Prepare_Request
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session, Current_Generation : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Status : out Preparation_Result)
   is
      Operation : constant Unsigned_64 := Shift_Right (Request (0), 16) and 16#FFFF#;
      ID : constant Unsigned_64 := Request (1) and 16#FFFF_FFFF#;
      Offset : constant Unsigned_64 := Shift_Right (Request (1), 32) * 4096;
      GPU : constant Unsigned_64 := Request (2);
      Bytes : constant Unsigned_64 := Request (3);
      Accepted : Boolean;
   begin
      Check_Update_Request (Object, Source, VM_Session, Current_Generation,
        Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Status);
      if Status /= Eligible then return; end if;
      Prepare_Change (Object, Source, Candidate, Tables, VM_Session,
        Sender, Stamp, ID, GPU, Offset, Bytes, Operation = 1, Accepted);
      Status := (if Accepted then Prepared else Not_Ready);
   end Prepare_Request;
   function Batch_Mapped
     (Object : Service; Image : VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64) return Boolean
   is
      Backing : Intel_GPU_Buffer_Reply.Backing;
   begin
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session or else
        not Owner_Ready or else Object.Failed or else ID = 0 or else
        ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last) or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 8 /= 0 or else
        Offset mod 8 /= 0 or else Bytes = 0 or else Bytes mod 4 /= 0
      then return False; end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      return Binder.Matches_Range (Image, Backing, GPU, Offset, Bytes) and then
        Owner_Ready and then Session_Of (Sender, Stamp) = VM_Session;
   end Batch_Mapped;
   procedure Prepare_Change
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean)
   is
      Backing : Intel_GPU_Buffer_Reply.Backing;
   begin
      Accepted := False;
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session or else
        not Owner_Ready or else Object.Failed or else not VM.Sealed (Source) or else
        ID = 0 or else ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
      then return; end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if not Backing.Ready then return; end if;
      VM.Prepare_Update (Candidate, Source, Tables, Accepted);
      if not Accepted then return; end if;
      if Remove then
         Binder.Unbind_Range (Candidate, Backing, GPU, Offset, Bytes, Accepted);
      else
         Binder.Bind_Range (Candidate, Backing, GPU, Offset, Bytes,
           Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, Accepted);
      end if;
      if Accepted then VM.Seal_Update (Candidate, Accepted); end if;
      Accepted := Accepted and then Owner_Ready and then
        Session_Of (Sender, Stamp) = VM_Session;
   end Prepare_Change;
   procedure Handle
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      Accepted : Boolean;
   begin
      Response := [Denied, Version, 0, 0];
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Bind_Label or Length /= 4 or Flags /= 0 or Reserved /= 0
        or (Request (0) and 16#FFFF#) /= Version
        or (Shift_Right (Request (0), 16) and 16#FFFF#) > 1 then return; end if;
      Response (0) := Unavailable;
      if VM.Sealed (Image) or else not Owner_Ready or else Object.Failed then return; end if;
      if (Request (0) and 16#10000#) /= 0 then
         Unbind (Object, Image, VM_Session, Sender, Stamp, Request (1), Request (2),
                 Shift_Right (Request (0), 32) * 4096, Request (3), Accepted);
      else
         Bind (Object, Image, VM_Session, Sender, Stamp, Request (1), Request (2),
               Shift_Right (Request (0), 32) * 4096, Request (3), Accepted);
      end if;
      Response := (if Accepted then [OK, Version, Request (2), Request (3)]
                   else [Denied, Version, 0, 0]);
   end Handle;
   procedure Change_Range
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Accepted : out Boolean; Remove : Boolean)
   is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Backing : Intel_GPU_Buffer_Reply.Backing;
   begin
      Accepted := False;
      if Session = 0 or else Session /= VM_Session or else Object.Failed or else
        not Owner_Ready or else ID = 0 or else
        ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
      then return; end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if not Backing.Ready then return; end if;
      if Remove then
         Binder.Unbind_Range (Image, Backing, GPU, Offset, Bytes, Accepted);
      else
         Binder.Bind_Range (Image, Backing, GPU, Offset, Bytes,
           Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, Accepted);
      end if;
   end Change_Range;
   procedure Bind
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Change_Range (Object, Image, VM_Session, Sender, Stamp, ID, GPU,
                    Offset, Bytes, Accepted, False);
   end Bind;
   procedure Unbind
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Change_Range (Object, Image, VM_Session, Sender, Stamp, ID, GPU,
                    Offset, Bytes, Accepted, True);
   end Unbind;
end Intel_GPU_Buffer_Requests.Binding;
