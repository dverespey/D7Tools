//
//     Name:   TitlePanel.pas
//     Desc:   TitlePanel Component
//             This component is a modified event panel. It contains a new caption
//             and title labels. It can be used to display table information in the caption
//             with the title containg the description of what is being displayed.
//
//     Author: David Verespey
//
//     Revision History:
//
//     09/01/96    Start Program
//
//
unit TitlePanel;

interface

uses
  Windows, Messages, SysUtils, stdctrls,extctrls, Classes, Graphics, Controls,
  Forms, Dialogs, ThreadedEventPanel;

type
   TTitlePanel = class(TThreadedEventPanel)
   private
    { Private declarations }
       fTitle: string;
       fTitleLabel: TLabel;
       fCaption: string;
       fCaptionLabel: TLabel;
   protected
    { Protected declarations }
    	procedure SetTitle(value: string);
    	procedure SetCaption(value: string);
   public
    { Public declarations }
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure paint; override;
   published
    { Published declarations }

    	property Caption: string
       	read fCaption
           write SetCaption;

       property Title: String
           read fTitle
           write SetTitle;
   end;


implementation

//-------------------------
//
//     Update Panel
//
//-------------------------
constructor TTitlePanel.Create(AOwner: TComponent);
begin
   Inherited Create(AOwner);
   fTitleLabel := TLabel.Create(self);
   fTitleLabel.Parent:=self;
   SetTitle('Title');
   fTitleLabel.Left:=5;
   fTitleLabel.Top:=5;
   fTitleLabel.Show;

   fCaptionLabel := TLabel.Create(self);
   fCaptionLabel.Parent:=self;
   SetCaption('Value');
   fCaptionLabel.Show;
end;

destructor TTitlePanel.Destroy;
begin
   fTitleLabel.Free;
   inherited destroy;
end;

procedure TTitlePanel.SetTitle(value: string);
begin
   fTitle:=value;
   FTitleLabel.Caption := value;
end;

procedure TTitlePanel.SetCaption(value: string);
begin
   fCaption:=value;
   FCaptionLabel.Caption := value;
end;

procedure TTitlePanel.Paint;
begin
   fCaptionLabel.Left:=(Width - fCaptionLabel.Width) div 2;
   fCaptionLabel.Top:=Height div 2;
   Inherited Paint;
end;

end.
