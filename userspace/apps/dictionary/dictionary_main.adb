--  Dictionary (docs/lexicon.md): the app's Dictionary mode first.
with Dictionary_App;
procedure Dictionary_Main is
begin
   Dictionary_App.Run (Dictionary_App.Views.Dictionary_Mode);
end Dictionary_Main;
