unit PCPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Dialogs,
  ExtCtrls, emptypanel, ThreadedPanel, ThreadedEventPanel;

type
  TPCPanel = class(TThreadedEventPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
    fImage: TImage;
    fPicture: TPicture;
    fCaption: TLabel;
    fCaptionString: string;
    procedure SetCaption(Value: string);
    procedure SetPicture(value: TPicture);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
     property Picture:TPicture
       read fPicture
       write SetPicture;

     property Caption:String
       read fCaptionstring
       write SetCaption;
 end;


implementation

constructor TPCPanel.Create(AOwner: TComponent);
begin
   Inherited Create(AOwner);

   fCaption:=TLabel.Create(Self);
   fCaption.Parent:=Self;
   fCaption.Align:=albottom;
   fCaption.Caption:='Caption';
   fCaption.Show;

   fPicture:=TPicture.Create;
   fImage:=TImage.Create(self);
   fImage.Parent:=self;
   fImage.Align:=alclient;
   fImage.Stretch:=True;
   fImage.Show;

end;

destructor TPCPanel.Destroy;
begin
   fImage.Free;
   fPicture.Free;
   fCaption.Free;
   Inherited Destroy;
end;

procedure TPCPanel.SetCaption(Value: string);
begin
   fCaptionString:=value;
   fCaption.Caption:=value;
end;

procedure TPCPanel.SetPicture(Value: TPicture);
begin
   fPicture.Assign(Value);
   fImage.Picture.Assign(fPicture);
end;

end.
