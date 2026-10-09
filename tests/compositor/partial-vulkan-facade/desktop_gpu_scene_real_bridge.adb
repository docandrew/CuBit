with System;
with Desktop_Compositor; with Desktop_Vulkan_Startup; with Vulkan_Device_Owner;
with Compositor_Damage; with Compositor_Formats; with Compositor_Pool;
with CuBit.Display_Geometry;
package body Desktop_GPU_Scene_Real_Bridge is
 package C renames Desktop_Compositor; package D renames Desktop_Vulkan_Startup;
 package R renames Compositor_Damage; package G renames CuBit.Display_Geometry;
 use type Interfaces.Unsigned_64;
 use type C.Output_Start, C.Render_Completion, C.Target_Release, Vulkan_Device_Owner.Phase;
 type Pixel_Array is array (0..6143) of Interfaces.Unsigned_32;
 Pixels : array (0..2) of aliased Pixel_Array := (others=>(others=>16#DEADBEEF#));
 Histories : array (0..2) of R.State;
 Active : Natural range 0..2 := 0;
 Image : Compositor_Formats.Image := (Pixels(0)'Address,96,64,384,1);
 Expected_Bytes : Interfaces.Unsigned_64 := 0;
 Before_Work : C.Transfer_Counters;
 procedure Watch (Base : System.Address; Bytes : Interfaces.Unsigned_64)
   with Import, Convention => C, External_Name => "watch_output_copy";
 function Readback_Bytes return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "recorded_readback_bytes";
 function Copied_Bytes return Interfaces.Unsigned_64
   with Import, Convention => C, External_Name => "output_copy_bytes";
 Writer : Compositor_Pool.Ticket := Compositor_Pool.None;
 Completion : C.Render_Completion;
 function Open return Interfaces.C.int is OK : Boolean; begin
  for H of Histories loop R.Add(H,(0,0,96,64)); end loop;
  D.Initialize(25); if D.Current/=Vulkan_Device_Owner.Ready then return 1; end if;
  D.Configure_Targets(96,64,1,1024*1024,OK); if not OK then return 2; end if;
  D.Prepare_Pipeline(OK); if not OK then return 3; end if;
  D.Configure_Upload(8192,OK); if not OK then return 4; end if;
  D.Configure_Readback(96*64*4,OK); if not OK then return 5; end if;
  C.Configure_Renderer((others=>True),OK); return (if OK and C.Full_Output then 0 else 6);
 end Open;
 function Frame (Step : Interfaces.C.int) return Interfaces.C.int is
  N : constant Natural:=Natural(Step); X : constant Natural:=20+(N mod 4)*4;
  Old_X : constant Natural:=20+((N+3) mod 4)*4;
  Y : constant Natural:=8+(N mod 4)*3;
  Old_Y : constant Natural:=8+((N+3) mod 4)*3;
  Repair : R.State;
  Plan : R.State; Started : C.Output_Start; Drawn,Restart : Boolean; Area : Natural:=0;
  L,T,Right,Bottom : Natural;
 begin
  Active:=(N / 2 + N mod 2) mod 3; Image.Pixels:=Pixels(Active)'Address;
  Writer:=(Compositor_Pool.Live_Slot(Active+1),1,Interfaces.Unsigned_64(N+1));
  if N=0 then R.Add(Plan,(0,0,96,64)); else
   R.Add(Plan,(Old_X,24,Old_X+8,32)); R.Add(Plan,(X,24,X+8,32));
   R.Add(Plan,(70,Old_Y,74,Old_Y+4)); R.Add(Plan,(70,Y,74,Y+4)); end if;
  for H of Histories loop
   for I in 1..R.Count(Plan) loop R.Add(H,R.Item(Plan,I)); end loop;
  end loop;
  Repair:=Histories(Active); Expected_Bytes:=0;
  for I in 1..R.Count(Repair) loop
   declare B : constant R.Box:=R.Item(Repair,I); begin
    Expected_Bytes:=Expected_Bytes+Interfaces.Unsigned_64((B.Right-B.Left)*(B.Bottom-B.Top)*4);
   end;
  end loop;
  Before_Work := C.Readback_Work;
  Watch(Image.Pixels,96*64*4);
  C.Begin_Output(Image,96*64*4,Writer,(96,64,G.Unrotated,(1,1),0,0),False,Started,Plan,Repair);
  if Started/=C.Started then return 10; end if;
  for I in 1..R.Count(Plan) loop
   declare B : constant R.Box:=R.Item(Plan,I); begin
    Area:=Area+(B.Right-B.Left)*(B.Bottom-B.Top);
   end;
  end loop;
  if N>2 and Area>=1024 then return 13; end if;
  for I in 1..R.Count(Repair) loop R.Add(Plan,R.Item(Repair,I)); end loop;
  for I in 1..R.Count(Plan) loop
   declare B : constant R.Box:=R.Item(Plan,I); begin
    C.Draw_Fill(Image,96*64*4,(G.Pixel_Edge(B.Left),G.Pixel_Edge(B.Top),G.Pixel_Edge(B.Right),G.Pixel_Edge(B.Bottom)),16#00102030#,False,Drawn,Restart);
    if not Drawn or Restart then return 11; end if;
    L:=Natural'Max(B.Left,X); T:=Natural'Max(B.Top,24); Right:=Natural'Min(B.Right,X+8); Bottom:=Natural'Min(B.Bottom,32);
    if L<Right and T<Bottom then
     C.Draw_Fill(Image,96*64*4,(G.Pixel_Edge(L),G.Pixel_Edge(T),G.Pixel_Edge(Right),G.Pixel_Edge(Bottom)),16#00123456#,False,Drawn,Restart);
     if not Drawn or Restart then return 12; end if;
    end if;
    L:=Natural'Max(B.Left,70); T:=Natural'Max(B.Top,Y); Right:=Natural'Min(B.Right,74); Bottom:=Natural'Min(B.Bottom,Y+4);
    if L<Right and T<Bottom then
     C.Draw_Fill(Image,96*64*4,(G.Pixel_Edge(L),G.Pixel_Edge(T),G.Pixel_Edge(Right),G.Pixel_Edge(Bottom)),16#00ABCDEF#,False,Drawn,Restart);
     if not Drawn or Restart then return 15; end if;
    end if;
   end;
  end loop;
  C.Complete_Output(Image.Pixels,Writer,False,False,Completion);
  return (if Completion in C.Pending|C.Complete then 0 else 14);
 end Frame;
 function Pump return Interfaces.C.int is begin
  if Completion=C.Pending then C.Complete_Output(Image.Pixels,Writer,False,True,Completion); end if;
  if Completion=C.Complete then
   if Copied_Bytes/=Expected_Bytes then return 4; end if;
   if Readback_Bytes/=Expected_Bytes then return 5; end if;
   if C.Readback_Work.Saturated or else
      C.Readback_Work.GPU_Submitted - Before_Work.GPU_Submitted /= Readback_Bytes or else
      C.Readback_Work.CPU_Copied - Before_Work.CPU_Copied /= Copied_Bytes then return 6; end if;
   R.Clear(Histories(Active));
  end if;
  return (case Completion is when C.Complete=>0, when C.Pending=>1, when others=>2);
 end Pump;
 function Wrong_Writer return Interfaces.C.int is
  Result : C.Render_Completion;
  Before_Copy : constant Interfaces.Unsigned_64 := Copied_Bytes;
  Before_Transfer : constant Interfaces.Unsigned_64 := Readback_Bytes;
  Prior_Work : constant C.Transfer_Counters := C.Readback_Work;
  use type C.Transfer_Counters;
  Foreign : constant Compositor_Pool.Ticket := (Writer.Buffer,Writer.Epoch,Writer.Serial+1000);
 begin
  C.Complete_Output(Image.Pixels,Foreign,False,True,Result);
  return (if Result=C.Unsafe and C.Readback_Work=Prior_Work and Copied_Bytes=Before_Copy and Readback_Bytes=Before_Transfer then 0 else 1);
 end Wrong_Writer;
 function Pixel (Index : Interfaces.C.int) return Interfaces.Unsigned_32 is (Pixels(Active)(Natural(Index)));
 function Close return Interfaces.C.int is Result : C.Target_Release; begin
  C.Forget_Targets(Result); if Result/=C.Targets_Retired then return 1; end if;
  D.Stop; return (if D.Current=Vulkan_Device_Owner.Retired and D.Charged_Bytes=0 then 0 else 2);
 end Close;
end Desktop_GPU_Scene_Real_Bridge;
