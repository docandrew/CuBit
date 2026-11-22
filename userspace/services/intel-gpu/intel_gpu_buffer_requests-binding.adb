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
   function Populated_Count (Tables : VM.Backing_Pages) return Natural is
      Count : Natural := 0;
   begin
      for P in Tables'Range loop
         exit when Tables (P) = 0;
         Count := P;
      end loop;
      return Count;
   end Populated_Count;
   procedure Handle_Update_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      procedure Prepare_Stream is new Prepare_Request_From_Pages (Read_Page);
      Before : constant Unsigned_64 := Coordinator.Generation (State);
      Preparation : Preparation_Result;
      Result : Coordinator.Result;
      use type Coordinator.Result;
   begin
      Response := [Denied, Version, 0, 0];
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Response (0) := Unavailable;
      if not Coordinator.Can_Submit (State) then return; end if;
      Prepare_Stream (Object, Source, Candidate, Table_Count, VM_Session, Before,
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
   end Handle_Update_From_Pages;
   procedure Handle_Update
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is

      Count : constant Natural := Populated_Count (Tables);
      Tail_Valid : constant Boolean := (for all P in Count + 1 .. Tables'Last => Tables (P) = 0);
      function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
        (if Tail_Valid then Tables (Page) else 0);
      procedure Stream is new Handle_Update_From_Pages (Read_Page, Coordinator);
   begin
      Stream (Object, Source, Candidate, Count, State,
        VM_Session, Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Response);
   end Handle_Update;
   procedure Begin_In_Place
     (Object : Service; Source : in out VM.Image;
      State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words; Started : out Boolean)
   is
      Before : constant Unsigned_64 := Coordinator.Generation (State);
      ID : constant Unsigned_64 := Request (1) and 16#FFFF_FFFF#;
      Offset : constant Unsigned_64 := Shift_Right (Request (1), 32) * 4096;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Preparation : Preparation_Result;
      Result : Coordinator.Result;
      Ready : Boolean;
      use type Coordinator.Result;
   begin
      Started := False;
      Response := [Denied, Version, 0, 0];
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Response (0) := Unavailable;
      if not Coordinator.Can_Submit (State) then return; end if;
      Check_Update_Request (Object, Source, VM_Session, Before,
        Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Preparation);
      case Preparation is
         when Request_Denied => Response (0) := Denied; return;
         when Malformed => Response (0) := Bad_Request; return;
         when Eligible => null;
         when others => return;
      end case;
      if (Shift_Right (Request (0), 16) and 16#FFFF#) /=
        (if Remove then 1 else 0) then
         Response (0) := Bad_Request; return;
      end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if Remove and then
        not Binder.Matches_Range (Source, Backing, Request (2), Offset, Request (3))
      then return; end if;
      Capture (Backing, Request (2), Offset, Request (3), VM.Revision (Source), Ready);
      if not Ready or else Object.Failed or else not Owner_Ready or else
        Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Coordinator.Begin_Update (State, Before, Started, Result);
   end Begin_In_Place;
   procedure Finish_In_Place
     (Object : Service; State : in out Coordinator.State;
      VM_Session, Sender, Stamp, Previous_Generation : Unsigned_64;
      Response : out Words) is
   begin
      Response := [Unavailable, Version, 0, 0];
      if Object.Failed or else not Owner_Ready or else
        VM_Session = 0 or else Previous_Generation = Unsigned_64'Last or else
        Session_Of (Sender, Stamp) /= VM_Session or else
        not Coordinator.Can_Submit (State) or else
        Coordinator.Generation (State) /= Previous_Generation + 1
      then
         Coordinator.Fail (State); return;
      end if;
      Response := [OK, Version, Coordinator.Generation (State), 0];
   end Finish_In_Place;
   procedure Handle_In_Place
     (Object : Service; Source : in out VM.Image;
      State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      procedure Begin_Request is new Begin_In_Place (Coordinator, Remove, Capture);
      procedure Finish_Request is new Finish_In_Place (Coordinator);
      Before : constant Unsigned_64 := Coordinator.Generation (State);
      Started, Finished : Boolean;
      Status : Coordinator.Result;
      use type Coordinator.Result;
   begin
      Begin_Request (Object, Source, State, VM_Session, Sender, Stamp,
        Request_Label, Length, Flags, Reserved, Request, Response, Started);
      if not Started then return; end if;
      loop
         Coordinator.Advance_Once (State, Finished, Status);
         exit when Finished;
      end loop;
      if Status /= Coordinator.Complete then return; end if;
      Finish_Request (Object, State, VM_Session, Sender, Stamp, Before, Response);
   end Handle_In_Place;
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
   procedure Prepare_Request_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; VM_Session, Current_Generation : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Status : out Preparation_Result)
   is
      procedure Prepare_Stream is new Prepare_Change_From_Pages (Read_Page);
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
      Prepare_Stream (Object, Source, Candidate, Table_Count, VM_Session,
        Sender, Stamp, ID, GPU, Offset, Bytes, Operation = 1, Accepted);
      Status := (if Accepted then Prepared else Not_Ready);
   end Prepare_Request_From_Pages;
   procedure Prepare_Request
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session, Current_Generation : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Status : out Preparation_Result)
   is

      Count : constant Natural := Populated_Count (Tables);
      Tail_Valid : constant Boolean := (for all P in Count + 1 .. Tables'Last => Tables (P) = 0);
      function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
        (if Tail_Valid then Tables (Page) else 0);
      procedure Stream is new Prepare_Request_From_Pages (Read_Page);
   begin
      Stream (Object, Source, Candidate, Count, VM_Session, Current_Generation,
        Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Status);
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
   procedure Prepare_Change_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean)
   is
      Backing : Intel_GPU_Buffer_Reply.Backing;
      function Read_Owned_Page (Page : VM.Page_Number) return Unsigned_64 is
         DMA : Unsigned_64;
      begin
         if not Owner_Ready or else Object.Failed or else
           Session_Of (Sender, Stamp) /= VM_Session then return 0; end if;
         DMA := Read_Page (Page);
         return (if Owner_Ready and then not Object.Failed and then
           Session_Of (Sender, Stamp) = VM_Session then DMA else 0);
      end Read_Owned_Page;
      procedure Prepare_Stream is new VM.Prepare_Update_From_Pages (Read_Owned_Page);
   begin
      Accepted := False;
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session or else
        not Owner_Ready or else Object.Failed or else not VM.Sealed (Source) or else
        ID = 0 or else ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last)
      then return; end if;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if not Backing.Ready then return; end if;
      if Table_Count not in VM.Page_Number then return; end if;
      Prepare_Stream (Candidate, Source, Table_Count, Accepted);
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
   end Prepare_Change_From_Pages;
   procedure Prepare_Change
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean)
   is

      Count : constant Natural := Populated_Count (Tables);
      Tail_Valid : constant Boolean := (for all P in Count + 1 .. Tables'Last => Tables (P) = 0);
      function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
        (if Tail_Valid then Tables (Page) else 0);
      procedure Stream is new Prepare_Change_From_Pages (Read_Page);
   begin
      Stream (Object, Source, Candidate, Count, VM_Session,
        Sender, Stamp, ID, GPU, Offset, Bytes, Remove, Accepted);
   end Prepare_Change;
   procedure Check_Offline_Bind_Request
     (Object : Service; Source : VM.Image;
      VM_Session, Expected_Revision, Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Status : out Preparation_Result)
   is
      ID : constant Unsigned_64 := Request (1);
      Offset : constant Unsigned_64 := Shift_Right (Request (0), 32) * 4096;
      GPU : constant Unsigned_64 := Request (2);
      Bytes : constant Unsigned_64 := Request (3);
      Limit : constant Unsigned_64 := Unsigned_64 (Intel_GPU_Buffer_Backing.Page_Count'Last) * 4096;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   begin
      Status := Request_Denied;
      if VM_Session = 0 or else Session_Of (Sender, Stamp) /= VM_Session then return; end if;
      Status := Malformed;
      if Request_Label /= Bind_Label or else Length /= 4 or else Flags /= 0 or else Reserved /= 0 or else
        (Request (0) and 16#FFFF#) /= Version or else
        (Shift_Right (Request (0), 16) and 16#FFFF#) /= 0 or else
        ID = 0 or else ID > Unsigned_64 (Intel_GPU_Buffer_Handles.Handle'Last) or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > Limit or else
        Offset > Limit - Bytes or else Bytes > 2 ** 48 - GPU
      then return; end if;
      Status := Not_Ready;
      if Object.Failed or else not Owner_Ready or else VM.Sealed (Source) or else
        VM.Root_DMA (Source) = 0 then return; end if;
      Status := Stale_Generation;
      if VM.Revision (Source) /= Expected_Revision then return; end if;
      Status := Request_Denied;
      Backing := Intel_GPU_Buffer_Handles.Resolve
        (Object.Handles, VM_Session, Intel_GPU_Buffer_Handles.Handle (ID));
      if not Backing.Ready or else Offset > Backing.Bytes or else Bytes > Backing.Bytes - Offset
      then return; end if;
      Status := Not_Ready;
      if not Owner_Ready or else Object.Failed or else Session_Of (Sender, Stamp) /= VM_Session
      then return; end if;
      Status := Eligible;
   end Check_Offline_Bind_Request;
   procedure Handle
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      Accepted : Boolean := False;
      Status : Preparation_Result;
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
         Check_Offline_Bind_Request (Object, Image, VM_Session, VM.Revision (Image),
           Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Status);
         if Status = Eligible then
            Bind (Object, Image, VM_Session, Sender, Stamp, Request (1), Request (2),
                  Shift_Right (Request (0), 32) * 4096, Request (3), Accepted);
         end if;
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
