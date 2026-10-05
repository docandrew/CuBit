with CCL.Objects;
with Interfaces;

--  What an evaluation may ask of a stream its session holds
--  (docs/ccl-streams.md). An evaluation never waits on or drains a stream:
--  it reads bounded views of what has already arrived. One reader serves
--  both engines; the host answers from its own stream table.
package CCL.Streams with SPARK_Mode is
   --  A session's name for a stream it holds: meaningful only in that
   --  session's table, like a file descriptor. It grants nothing; the
   --  authority was exercised when the stream was opened.
   Maximum_Handle : constant := 2**31 - 1;
   type Handle is range 0 .. Maximum_Handle;
   No_Handle : constant Handle := 0;

   --  The most elements one window view returns: what one image holds,
   --  its count cell and then one cell per scalar element.
   Maximum_Window : constant := CCL.Objects.Maximum_Cells - 1;
   subtype Window_Length is Positive range 1 .. Maximum_Window;

   --  Delivered and dropped elements are counted, never wrapped.
   subtype Element_Total is Interfaces.Integer_64 range 0 .. Interfaces.Integer_64'Last;

   type View_Kind is
     (Latest_View,   --  the newest element: an image of T
      Window_View,   --  the newest Count elements, oldest first: an image of List<T>
      Arrived_View,  --  elements delivered since the stream was opened
      Lost_View,     --  elements dropped by a lossy stream's ring
      Wait_View);   --  a task's result: an image of T, once it completed

   function Returns_Elements (View : View_Kind) return Boolean is
     (View in Latest_View | Window_View | Wait_View);

   type View_Request is record
      Stream : Handle := No_Handle;
      View : View_Kind := Latest_View;
      Count : Window_Length := 1;
   end record;

   type View_Status is
     (View_Answered,
      No_Such_Stream,  --  not a stream this session holds (or one it closed)
      Stream_Empty);   --  latest, before the first element; wait, while pending

   --  Elements is an image with No_Schema laid out as the evaluation's own
   --  T (or List<T>): the evaluation validates it against that type before
   --  using it, so a reply never has to be trusted.
   type View_Reply is record
      Status : View_Status := No_Such_Stream;
      Total : Element_Total := 0;
      Elements : CCL.Objects.Image;
   end record;
end CCL.Streams;
