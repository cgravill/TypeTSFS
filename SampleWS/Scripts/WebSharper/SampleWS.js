(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,JSON,Json,Provider,Id,console,SampleWS,Main,CustomGeneric;
 Runtime.Define(Global,{
  SampleWS:{
   Main:{
    CustomGeneric:Runtime.Class({},{
     New:function()
     {
      return Runtime.New(this,{});
     }
    }),
    makeRectangle:function()
    {
     return JSON.stringify(((Provider.get_Default().EncodeRecord(undefined,[["Width",Id,0],["Height",Id,0]]))())({
      Width:2,
      Height:3
     }));
    },
    makeRectangleShape:function()
    {
     var _;
     _=Provider.get_Default();
     return JSON.stringify(((_.EncodeUnion(undefined,"",[["Rectangle",[[null,null,_.EncodeRecord(undefined,[["Width",Id,0],["Height",Id,0]])]]],["Square",[[null,null,_.EncodeRecord(undefined,[["Length",Id,0]])]]],["MaybeCircle",[["$0","Item",_.EncodeRecord(undefined,[["Radius",Id,0],["Numbers",_.EncodeList(Id),0]]),1]]]]))())({
      $:0,
      $0:{
       Width:2,
       Height:3
      }
     }));
    },
    makeShapes:function()
    {
     var rect;
     rect={
      $:0,
      $0:{
       Width:2,
       Height:3
      }
     };
     return console?console.log(rect):undefined;
    },
    something:Runtime.Field(function()
    {
     return CustomGeneric.New();
    })
   }
  }
 });
 Runtime.OnInit(function()
 {
  JSON=Runtime.Safe(Global.JSON);
  Json=Runtime.Safe(Global.WebSharper.Json);
  Provider=Runtime.Safe(Json.Provider);
  Id=Runtime.Safe(Provider.Id);
  console=Runtime.Safe(Global.console);
  SampleWS=Runtime.Safe(Global.SampleWS);
  Main=Runtime.Safe(SampleWS.Main);
  return CustomGeneric=Runtime.Safe(Main.CustomGeneric);
 });
 Runtime.OnLoad(function()
 {
  Main.something();
  return;
 });
}());
